# POSIX.1-2024 Conformance Audits — `users/` utilities

This file collects per-utility POSIX conformance audits for the user/terminal
utilities crate. Each audit follows the playbook in `audits.md`.

The crate ships ten binaries: nine POSIX.1-2024 utilities — `id`, `logname`,
`logger`, `mesg`, `newgrp`, `pwd`, `tty`, `write`, `talk` — plus the `talkd`
daemon. `talkd` is **not** specified by POSIX, but the project treats it as
*implicitly required* — POSIX `talk` cannot function without a talk daemon,
exactly as the project treats `crond` as implicitly required by
`crontab`/`at`/`batch`. `talkd` is therefore audited against the implicit
requirements imposed by POSIX `talk`, the BSD `ntalk` protocol it must serve,
and secure-daemon practice (the same lens `cron/audit.md` applied to `crond`).

**Method:** static spec-vs-code audit against the sliced POSIX.1-2024 tree
(`~/tmp/posix.2024/sliced/`), every Critical/Major "absent"/"wrong" claim
confirmed by reading the cited implementation lines **and** the cited spec
lines directly (and, for the shared helpers, `plib/src/curuser.rs`). No code
was modified — this is an audit, not a fix.

**Date:** 2026-06-18 (audit). **Remediation:** 2026-06-18, branch `users-audit`.

## Remediation summary (2026-06-18)

All findings have since been remediated across 12 themed, independently-committed
phases (build + `clippy --all-targets` + `fmt` clean and tests green per phase).
Shared foundation: `plib::curuser::{login_name_strict, ttyname_of}`, a new
`plib::exec::exec_error_exit` (126/127), a ported `portable-pty` test harness
(`users/tests/common`), and a crate-wide migration to `plib::diag` + `gettext`.

| Utility | Disposition | Notes |
|---|---|---|
| `id` | **Audited** ✓ | `-G` now emits {gid,egid}∪getgroups(); egid name fallback; i18n. |
| `logname` | **Audited** ✓ | strict `getlogin()` (no `$USER`), fails + diagnoses; tests added. |
| `logger` | **Audited** ✓ | `-i`/`-f`/`-p`/`-t`, stdin bodies, `user.notice` default. |
| `mesg` | **Audited** ✓ | 0/1/>1 exit ladder (verified vs system `mesg`); PTY tests. |
| `pwd` | **Audited** ✓ | `-L`/`-P` last-wins, byte-faithful output; tests. |
| `tty` | **Audited** ✓ | stdin-only name; ttyname-fail → exit 2. |
| `newgrp` | **Audited** ✓ | execs a shell in both modes; invoke-anyway; gshadow+group-DB verify (Linux), constant-time, fail-closed; privilege drop kept. |
| `write` | **Audited** ✓ | canonical-mode per-char rendering; alert→recipient; superuser override; no panics; SIGHUP/SIGPIPE; `/dev` char-device validation. |
| `talkd` | resolved; **stays Stage 3 with `talk`** | all items fixed or documented (`#TD7` local-only WON'T-FIX). Not separately promoted because the README bundles it with `talk`. |
| `talk` | **not promoted (Stage 3)** | Criticals (#TK1/2/3) + Majors (#TK5/6/8) fixed; **#TK7** (full char-processing) and Minors #TK9/#TK10/#TK14 **deferred** — the interactive curses/input engine is unverifiable in CI, so changing it blind risks silent regressions. |

Per-finding ✓-fixed annotations appear inline in each section below. README
promotions: `id`, `logname`, `logger`, `mesg`, `newgrp`, `pwd`, `tty`, `write`
→ **Stage 6 — Audited**; `talk (with talkd local daemon)` remains Stage 3.

The headline-counts table below is the **original audit snapshot** (what was
found), preserved for the record; the disposition table above reflects the
current state.

## Headline counts

| Utility | Critical | Major | Minor | Tests |
|---|---|---|---|---|
| `id` | 0 | 2 | 4 | yes (400 lines) |
| `logname` | 0 | 2 | 3 | **none** |
| `logger` | 2 | 1 | 2 | yes (observable-only, 77 lines) |
| `mesg` | 2 | 1 | 2 | **none** |
| `pwd` | 0 | 2 | 4 | **none** |
| `tty` | 0 | 1 | 2 | yes (447 lines, PTY) |
| `newgrp` | 3 | 4 | 5 | **none** |
| `write` | 2 | 4 | 4 | yes (864 lines, PTY) |
| `talk` | 3 | 5 | 6 | yes (353 lines, plumbing-only) |
| `talkd` | 3 | 4 | 5 | unit-only (registry) |

## Cross-cutting themes

Four patterns recur across the crate. They are called out per-utility below,
but the shared root causes are worth fixing once:

1. **Exit status ignores the spec ladder.** `mesg` always exits 0 regardless
   of the messaging state the spec keys 0/1 on (#MG1/#MG2); `talk` exits
   `128+SIGINT` (130) where the spec mandates **0** on SIGINT (#TK1);
   `logname` never fails even when no login name exists (#LN2). The spec exit
   contract is the machine-readable interface for these utilities — getting it
   wrong silently breaks shell scripts.

2. **`plib::curuser` shared helpers diverge from spec.** `login_name()`
   (`plib/src/curuser.rs:12-41`) falls back to the **`$USER` environment
   variable** and then `getpwuid()` and finally the literal `"unknown"`,
   never failing — which `logname` (#LN1/#LN2) inherits, violating the spec's
   "login name = `getlogin()`; diagnose + non-zero exit otherwise" rule.
   `tty()` (`curuser.rs:43-61`) searches STDIN→STDOUT→STDERR, but `tty`
   (#TY1) must report the name of **standard input only**. `mesg`, by
   contrast, correctly searches all three fds via its own `find_tty` (that is
   the System-V sequence the spec endorses for `mesg`).

3. **`setlocale` is wired but diagnostics are hardcoded English.** Almost
   every utility calls `setlocale(LC_ALL,"")` + `textdomain` (`pwd`, `mesg`,
   `tty`, `id`, `newgrp`, `write`, `talk`, `talkd`, `logger`), yet runtime
   diagnostic/prompt strings are raw `eprintln!`/`println!` literals, not
   `gettext()`-wrapped — so `LC_MESSAGES` is inert. (`logname` calls no
   locale init at all.) Same disposition as the `dev/` crate: plumbing is in
   place, string-level `gettext()` wrapping is the remaining work.

4. **Zero tests on the security- and correctness-critical utilities.**
   `newgrp` (GID/UID manipulation, password verification, shell exec),
   `mesg`, `pwd`, `logname`, and `talkd`'s wire/datagram path have **no**
   tests. `newgrp`'s absence is itself a Major finding given #NG1–#NG3.

---

## `id`

**Implementation:** `users/id.rs` (375 lines) + tests `users/tests/id/mod.rs` (400 lines).
**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 §3, `id` (pp. 3055–3058).
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/id.md`
**Date:** 2026-06-18

### TL;DR

The default output format, `-u`, `-g`, `-G`, and their `-n`/`-r` modifiers are
implemented and largely correct on the common (current-process) path, with
reasonable test coverage. The headline conformance risk is in `-G`: for the
current process, the effective group ID (egid) is silently dropped from the
`-G` list whenever it is not already present in `getgroups()` output
(`id.rs:172-177,202-221`) — verified: egid is added to `seen_gids` (line 176)
but never pushed into `unique_groups`, so a distinct egid absent from
`getgroups()` never reaches `-G`/`groups=`. Several name-lookup edge cases
also diverge, and locale init is wired but diagnostics are raw English. No
crashes or hangs.

### Priority issues

#### Critical
(none)

#### Major
- [x] **#I1 — `-G` for the current process can omit the effective group ID.** ✓ fixed (phase 1) — current-process branch now builds {gid, egid} ∪ getgroups() with egid pushed explicitly. `users/id.rs:172-177,202-221`. In `get_group_info` egid is added to `seen_gids` (line 176) before the supplementary loop; the non-named branch builds `unique_groups` from `gid` + `getgroups()`, skipping anything in `seen_gids`, so an egid that differs from gid and is absent from `getgroups()` never reaches the `-G`/default `groups=` list. Spec (100623-100625) requires `-G` to output effective, real, and supplementary IDs. Fix: after seeding gid, explicitly push egid into `unique_groups` (when distinct).
- [x] **#I2 — `-G`/default group set is built from real gid only, egid filtered.** ✓ fixed (phase 1) — bundled with #I1; egid now always included when distinct. `users/id.rs:161-221,304-346`. The `-G` set is seeded only with `userinfo.gid` (real); egid is filtered via `seen_gids` (see #I1). A process whose real and effective gids both differ from its supplementary set prints an incomplete `-G`. Fix: build the `-G` set as the dedup union {gid, egid} ∪ getgroups(), effective-first.

#### Minor
- [x] **#I3 — Diagnostics not localized despite gettext setup.** ✓ fixed (phase 1) — migrated to `plib::diag::{init_locale, error}`; messages wrapped in `gettext()`. `users/id.rs:101,120,362,370`.
- [x] **#I4 — `egid=` name lookup in default output ignores live `getgrgid` fallback.** ✓ fixed (phase 1) — `egid=` branch now falls back to `get_groupname()` like the `-g` path. `users/id.rs:323-329`.
- [ ] **#I5 — Named-user "omit `(%s)`" comments slightly misleading; output conforms.** `users/id.rs:104-108,242,266,291`. Behavior is correct (omits the name when unmappable per spec); documentation-only.
- [ ] **#I6 — Single-operand surface; extras rejected by clap (exit 2).** `users/id.rs:37-38`. Spec SYNOPSIS allows only `[user]`; clap exits 2 on extras (acceptable, `>0`). Noted for completeness, no change needed.

### Detailed conformance matrix

#### Options

| Option | Spec | Status | Notes (file:line) |
|---|---|---|---|
| `-G` | 100623 | CONFORMS ✓ | Space-separated, primary first; egid+real always included (fixed #I1/#I2, phase 1). id.rs:276-299 |
| `-g` | 100626 | CONFORMS | Effective gid; `-r`→real, `-n`→name w/ numeric fallback. id.rs:254-273 |
| `-u` | 100630 | CONFORMS | Effective uid; `-r`→real, `-n`→name. id.rs:229-251 |
| `-n` | 100627 | CONFORMS | Modifier on `-G`/`-g`/`-u`; ignored alone. id.rs:34-35 |
| `-r` | 100629 | CONFORMS | Real instead of effective; ignored alone. id.rs:31-32 |
| Mutual exclusion `-G`/`-g`/`-u` | 100604 | CONFORMS | clap `group="output"` rejects combinations. id.rs:22,25,28 |
| `--` terminator | XBD 12.2 | CONFORMS | clap default |

#### Operands / STDIN
- [x] `user` operand — `getpwnam`; effective IDs assumed equal to real per spec 100616-100617. `id.rs:116-139`. CONFORMS.
- [x] STDIN "Not used" — never read. CONFORMS (N/A).
- [x] Named-user supplementary groups scanned via `plib::group::load()`, primary first. `id.rs:179-201`. CONFORMS.

#### Environment variables

| Var | Status | Notes |
|---|---|---|
| `LANG`/`LC_ALL`/`LC_CTYPE` | CONFORMS | `setlocale(LcAll,"")` id.rs:353 |
| `LC_MESSAGES` | CONFORMS ✓ | `plib::diag` + `gettext()`-wrapped diagnostics (fixed #I3, phase 1); inert only until `.mo` catalogs ship (tree-wide) |
| `NLSPATH` (XSI) | N/A | gettextrs stub-backed |

#### Asynchronous events
- [x] Default (spec 100654). No custom handling. N/A.

#### STDOUT / STDERR
- [x] Default format `uid=%u(%s) gid=%u(%s)` — id.rs:304-313. CONFORMS.
- [x] `euid=` inserted only when euid≠uid — id.rs:316-321. CONFORMS.
- [x] `egid=` inserted only when egid≠gid — id.rs:324-329, name fallback added (fixed #I4, phase 1). CONFORMS.
- [x] `groups=` list — membership set now {gid,egid}∪getgroups() (fixed #I1/#I2, phase 1). CONFORMS.
- [x] Omit `(%s)` when name unmappable — id.rs:305,311,318,326,342. CONFORMS.
- [x] STDERR for diagnostics only — id.rs:362,370. CONFORMS.

#### Exit status / consequences of errors
- [x] 0 on success — id.rs:374. CONFORMS.
- [x] >0 on error — invalid user / lookup / write error → `ExitCode::from(1)`; clap arg errors exit 2. CONFORMS (spec 100696-100698).

### Test coverage signal

Not covered:
- [ ] `-G` with distinct effective vs. real group ID (would expose #I1/#I2).
- [ ] Default `euid=`/`egid=` branches (need a setuid/setgid harness) — #I4 untested.
- [ ] `-G -g` mutual-exclusion rejection; `-G -r` interaction.
- [ ] uid/gid with no passwd/group entry (the omit-name path).
- [ ] `LC_MESSAGES` effect on diagnostics (#I3, currently inert).
- [ ] Multiple operands / `id -- root`.

---

## `logname`

**Implementation:** `users/logname.rs` (16 lines) → `plib/src/curuser.rs:12-41` (`login_name`).
**Spec:** POSIX.1-2024, `logname` (pp. 3119–3120), lines 102985–103000.
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/logname.md`
**Tests:** **none** — `users/tests/users-tests.rs` declares only `id, logger, talk, tty, write`.
**Date:** 2026-06-18

### TL;DR

`logname` is a 4-line wrapper around `plib::curuser::login_name()`. Output is
the correct `"%s\n"` format and it takes no options/operands. But it diverges
materially from the normative requirement: the spec ties the login name
strictly to `getlogin()` and requires a **non-zero exit with a stderr
diagnostic** when `getlogin()` fails. The implementation instead silently
falls back to `$USER`, then `getpwuid()`, then the literal `"unknown"` —
verified at `curuser.rs:24-40` — never failing, never diagnosing, always
exiting 0. The `$USER` fallback is the worst part: the spec's APPLICATION
USAGE warns logname must ignore environment because "environment changes could
produce erroneous results."

### Priority issues

#### Major
- [x] **#LN1 — Falls back to the `$USER` environment variable; the spec forbids env-derived names.** ✓ fixed (phase 2) — `logname` now calls the new `curuser::login_name_strict()` (getlogin only, no `$USER`/getpwuid). `users/logname.rs`.
- [x] **#LN2 — Never fails on `getlogin()` failure; spec mandates non-zero exit + stderr diagnostic.** ✓ fixed (phase 2) — on `None`, emits `logname: no login name` via `plib::diag::error` and exits 1. `users/logname.rs`.

#### Minor
- [x] **#LN3 — `getpwuid()` fallback diverges from getlogin() semantics.** ✓ fixed (phase 2) — strict helper drops the getpwuid fallback entirely. `plib/src/curuser.rs` `login_name_strict`.
- [x] **#LN4 — No locale initialization.** ✓ fixed (phase 2) — `plib::diag::init_locale("logname")`. `users/logname.rs`.
- [x] **#LN5 — No tests.** ✓ fixed (phase 2) — new `users/tests/logname/mod.rs`: strict-contract (exit 0 non-empty name XOR exit 1 + diagnostic) and operand-rejection tests.

### Detailed conformance matrix

| Area | Status | Notes (file:line) |
|---|---|---|
| Options (None) | CONFORMS `- [x]` | No args parsed. logname.rs:12 |
| Operands (None) | CONFORMS `- [x]` | None accepted. logname.rs:12 |
| STDIN (Not used) | CONFORMS `- [x]` | Never read. |
| Env — LOGNAME ignored | CONFORMS `- [x]` | Not consulted. curuser.rs:12-39 |
| Env — USER ignored | CONFORMS ✓ | `login_name_strict` ignores `$USER` (fixed #LN1, phase 2) |
| Env — LC_*/LANG/NLSPATH | CONFORMS ✓ | `plib::diag::init_locale` (fixed #LN4, phase 2) |
| Async events (Default) | N/A `- [x]` | |
| STDOUT `"%s\n"` | CONFORMS `- [x]` | `println!` emits name + `\n`. logname.rs |
| STDERR (diagnostics only) | CONFORMS ✓ | `logname: no login name` on getlogin() failure (fixed #LN2, phase 2) |
| Login name = getlogin() | CONFORMS ✓ | getlogin only, no fallback (fixed #LN1/#LN3, phase 2) |
| Exit status (0 ok / >0 err) | CONFORMS ✓ | exits 1 on no login name (fixed #LN2, phase 2) |

### Test coverage signal

- [x] ✓ added (phase 2) — `users/tests/logname/mod.rs` strict-contract + operand-rejection tests.

---

## `logger`

**Implementation:** `users/logger.rs` (43 lines) + tests `users/tests/logger/mod.rs` (77 lines).
**Spec:** POSIX.1-2024, `logger` (pp. 3115–3118), lines 102862–102947.
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/logger.md`
**Date:** 2026-06-18

### TL;DR

A minimal wrapper over the `syslog` crate. It concatenates all argv operands
with single spaces and sends one message to the `LOG_USER` facility over the
Unix syslog socket. The operand-concatenation and facility-default semantics
conform, but it diverges substantially from the Issue-8 SYNOPSIS
(`logger [-i] [-f file] [-p priority] [-t tag] [string...]`, verified at spec
line 102862): **none** of the four options are implemented and there is no
STDIN/`-f` message-body input. The behaviors present also have defects: the
default priority is **`user.err`** (`writer.err`, `logger.rs:35`) where the
spec mandates **`user.notice`** (verified, spec 102902), the tag is hardcoded
`"logger"` with `pid:0`, and with no operands an empty message is logged
instead of reading stdin. Functional for the simplest `logger string...`
case; non-conformant for the full surface.

### Priority issues

#### Critical
- [x] **#LG1 — No option parsing; `-f`/`-i`/`-p`/`-t` are treated as message text.** ✓ fixed (phase 6) — clap now parses `-i`/`-f`/`-p`/`-t`; only the `string...` operands form the body. Verified in syslog that flags are not logged. `users/logger.rs`.
- [x] **#LG2 — No STDIN / `-f file` message-body input.** ✓ fixed (phase 6) — with no operands, message bodies are read from `-f file` (or stdin), one non-empty line per message. `users/logger.rs` `collect_messages`.

#### Major
- [x] **#LG3 — Default priority is `user.err`, not `user.notice`.** ✓ fixed (phase 6) — default is `user.notice`; `-p facility.level` honored (bare level defaults facility to `user`). `users/logger.rs`.

#### Minor
- [x] **#LG4 — Default tag/PID hardcoded; `-i`/`-t` unsupported.** ✓ fixed (phase 6) — default tag is the login name, `-t` overrides, `-i` logs `std::process::id()`. `users/logger.rs`.
- [x] **#LG5 — Operand-embedded newline sent as one multi-line record.** ✓ addressed (phase 6) — the stdin/`-f` path now splits on lines (one message each); the operand path remains a single concatenated message (operand-newline behavior is unspecified). `users/logger.rs`.

### Detailed conformance matrix

#### Options — CONFORMS ✓ (all four implemented, phase 6)
- [x] `-f file` — reads bodies from file (or `-`/stdin). logger.rs `collect_messages`.
- [x] `-i` — logs `std::process::id()`. logger.rs.
- [x] `-p priority` — `facility.level`, default `user.notice`. logger.rs `parse_priority`.
- [x] `-t tag` — overrides the default login-name tag. logger.rs.
- [x] clap provides `--` / XBD 12.2 handling.

#### Operands / STDIN
- [x] `string...` operands concatenated with single space — CONFORMS. logger.rs `collect_messages` (spec 102907-102909). Verified by `test_logger_multiple_args`.
- [x] STDIN/`-f` fallback when no operands — CONFORMS ✓ (fixed #LG2, phase 6); one non-empty line per message. logger.rs.

#### Environment variables
- [x] LANG/LC_ALL/LC_CTYPE/LC_MESSAGES — PARTIAL. `setlocale(LcAll,"")` logger.rs:15 honors precedence; no per-category handling. NLSPATH not separately handled (acceptable).

#### Asynchronous events
- [x] Default (spec 102935). No handlers. CONFORMS.

#### STDOUT / STDERR
- [x] STDOUT not used (spec 102937). No stdout writes. CONFORMS.
- [x] STDERR for diagnostics only — logger.rs:32,38. CONFORMS (spec 102939).

#### Exit status
- [x] 0 on success, >0 on error — logger.rs:36 / 33,39. CONFORMS (spec 102946-102947).

### Test coverage signal

Observable-behavior only (the test header acknowledges logger is "difficult to test"). 9 tests assert exit 0 + empty stdout/stderr for basic/multi-arg/special/unicode/empty/no-arg/long/quoted/newline inputs. Not covered:
- [ ] `-t`/`-p`/`-i`/`-f` parsing (#LG1) — and `test_logger_no_args`/`test_logger_empty_string` *lock in* the non-conformant empty-body behavior (#LG2).
- [ ] STDIN reading (#LG2).
- [ ] Actual logged priority/tag/facility (#LG3/#LG4) — unspecified output makes this hard, but the priority is checkable.
- [ ] The error paths (logger.rs:32,38).

---

## `mesg`

**Implementation:** `users/mesg.rs` (137 lines).
**Spec:** POSIX.1-2024, `mesg` (pp. 3216–3218), lines 106994–107000.
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/mesg.md`
**Tests:** **none** — no `users/tests/mesg/` module.
**Date:** 2026-06-18

### TL;DR

`mesg` correctly searches stdin→stdout→stderr for the first terminal
(`mesg.rs:33-43`, the System-V sequence the spec endorses), fchmods the
group/other write bits for `y`/`n`, and reports state as `is y`/`is n`
(unspecified format — acceptable). It has **two Critical conformance
defects**, both flagged by its own TODO (`mesg.rs:9-12`): (1) it **never sets
the spec-mandated exit status** — `main()` returns `Ok(())` (exit 0) for the
no-operand report, `y`, and `n` uniformly (verified `mesg.rs:122-137`), where
the spec keys exit 0 on "receiving allowed" and 1 on "not allowed" (verified
spec 106996-106998); (2) error paths exit 1 (via `Box<dyn Error>` from
`main`), colliding with the legitimate "not messageable" status of 1 instead
of `>1`. No tests at all.

### Priority issues

#### Critical
- [x] **#MG1 — Exit status does not reflect messaging state.** ✓ fixed (phase 5) — `main` now exits 0 when receiving is allowed and 1 when not, for the report, `y`, and `n` cases. `users/mesg.rs`.
- [x] **#MG2 — Error paths exit 1, indistinguishable from "not messageable".** ✓ fixed (phase 5) — no-terminal, invalid operand, and fchmod failure now emit a diagnostic and exit 2. `users/mesg.rs`.

#### Major
- [x] ~~**#MG3 — Exit must reflect the state *before* the command.**~~ ✓ fixed (phase 5) — **the "before the command" framing was incorrect.** Behaviorally verified against the system `mesg`: the exit status reflects the **resulting** state (`mesg y` → 0, `mesg n` → 1; report → current state). Implemented accordingly via `exit_for(resulting_allowed)`. `users/mesg.rs`.

#### Minor
- [x] **#MG4 — Accepts `Y`/`N` beyond the POSIX `y`/`n` operands.** ✓ fixed (phase 5) — restricted to lowercase `y`/`n`; any other operand is an error (exit 2). `users/mesg.rs`.
- [x] **#MG5 — Diagnostics partially unlocalized.** ✓ fixed (phase 5) — all diagnostics now go through `plib::diag::error` + `gettext`. `users/mesg.rs`.

### Detailed conformance matrix

| Area | Status | Notes (file:line) |
|---|---|---|
| Options (None) | CONFORMS `- [x]` | No options. |
| `y` grants permission | CONFORMS `- [x]` | adds S_IWGRP\|S_IWOTH. mesg.rs:99-104 |
| `n` denies permission | CONFORMS `- [x]` | clears the bits. mesg.rs:105-110 |
| No operand → report, no change | CONFORMS `- [x]` | `None => show_mesg`. mesg.rs:132 |
| Only `y`/`n` in POSIX locale | CONFORMS ✓ | restricted to lowercase `y`/`n` (fixed #MG4, phase 5). mesg.rs |
| STDIN (Not used) | CONFORMS `- [x]` | stdin only inspected for is_terminal. |
| First tty among stdin,stdout,stderr | CONFORMS `- [x]` | find_tty. mesg.rs:33-43 |
| Operates on that device's mode bits | CONFORMS `- [x]` | fstat/fchmod on chosen fd. mesg.rs:53-69,113 |
| Env LANG/LC_*/LC_MESSAGES | CONFORMS ✓ | `plib::diag` + all diagnostics `gettext`-wrapped (fixed #MG5, phase 5). mesg.rs |
| Async events (Default) | N/A `- [x]` | |
| STDOUT (state, unspecified format) | CONFORMS `- [x]` | `is y`/`is n`, only on no-operand. mesg.rs:74,76,132 |
| STDERR (diagnostics only) | CONFORMS `- [x]` | mesg.rs:64,115 |
| Exit 0 = receiving allowed | CONFORMS ✓ | `exit_for(allowed)` (fixed #MG1, phase 5) |
| Exit 1 = not allowed | CONFORMS ✓ | `mesg n`/report-when-denied → 1 (fixed #MG1, phase 5) |
| Exit >1 = error | CONFORMS ✓ | no-tty/invalid-operand/fchmod-fail → 2 (fixed #MG2, phase 5) |

### Test coverage signal

- [x] ✓ added (phase 5) — `users/tests/mesg/mod.rs`: PTY-backed `y`→0/`n`→1 round-trip with report, no-terminal→2, invalid-operand→2. Exit semantics behaviorally cross-checked against the system `mesg`.

---

## `pwd`

**Implementation:** `users/pwd.rs` (69 lines).
**Spec:** POSIX.1-2024, `pwd` (pp. 3365–3367), lines 112732–112744.
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/pwd.md`
**Tests:** **none** — no `pwd` module in `users/tests/`.
**Date:** 2026-06-18

### TL;DR

`pwd` parses `-L`/`-P` via clap and defaults to `current_dir()` (a
`getcwd`-equivalent physical path). The `-L` path correctly validates `$PWD`
(absolute, no `.`/`..` components) via `dirname_valid()` before using it. Two
real divergences: (1) **`-L`/`-P` "last one wins" is unimplemented and `-P` is
a dead flag** — verified that `args.process` (`pwd.rs:28`) is declared but
**never read**; the code only branches on `args.env` (`-L`), so `-L -P` does
not override to physical and `-P` does nothing; (2) error/output paths use
`to_string_lossy()` (`pwd.rs:66`), which mangles non-UTF-8 paths. The `-L`
`$PWD` validation logic itself is correct.

### Priority issues

#### Major
- [x] **#PW1 — `-L`/`-P` "last one wins" not implemented; `-P` ignored.** ✓ fixed (phase 3) — `-L`/`-P` are now `overrides_with` each other (last wins); physical mode ignores `$PWD` and uses getcwd. Verified with a real symlinked cwd. `users/pwd.rs`.
- [x] **#PW2 — `-P` does not explicitly guarantee symlink-free resolution.** ✓ fixed (phase 3) — `-P` uses `current_dir()` (getcwd, symlink-free on Linux/macOS); documented in `resolve_cwd`. `users/pwd.rs`.

#### Minor
- [x] **#PW3 — `$PWD` length not bounded against {PATH_MAX}.** ✓ fixed (phase 3) — logical mode falls back to getcwd when `$PWD` length ≥ `libc::PATH_MAX`. `users/pwd.rs`.
- [x] **#PW4 — Diagnostics not localized; default Rust error formatting.** ✓ fixed (phase 3) — `plib::diag::error` + `gettext`. `users/pwd.rs`.
- [x] **#PW5 — `to_string_lossy()` can corrupt non-UTF-8 paths.** ✓ fixed (phase 3) — emits raw `OsStr` bytes via `write_all`. `users/pwd.rs`.
- [x] **#PW6 — No tests.** ✓ fixed (phase 3) — new `users/tests/pwd/mod.rs`: invalid/`..` `$PWD` fallback, `-L` honors valid `$PWD`, `-P` ignores it, and both last-wins orderings. The stale `pwd.rs:10` TODO removed.

### Detailed conformance matrix

| Area | Status | Notes (file:line) |
|---|---|---|
| Options conform to XBD 12.2 | CONFORMS ✓ | `overrides_with` last-wins (fixed #PW1, phase 3). pwd.rs |
| Option −L (use valid `$PWD`) | CONFORMS `- [x]` | Validates absolute + no `.`/`..`. pwd.rs `dirname_valid` |
| Option −P (physical, no symlinks) | CONFORMS ✓ | `-P` → getcwd, ignores `$PWD` (fixed #PW1/#PW2, phase 3). pwd.rs |
| Last −L/−P wins; default −L | CONFORMS ✓ | `overrides_with` (fixed #PW1, phase 3). pwd.rs |
| `$PWD` absolute / no `.`,`..` | CONFORMS `- [x]` | `dirname_valid`. pwd.rs |
| `$PWD` > PATH_MAX handling | CONFORMS ✓ | falls back to getcwd at ≥ PATH_MAX (fixed #PW3, phase 3). pwd.rs |
| Operands (None) | CONFORMS `- [x]` | clap errors on extras. pwd.rs:23-29 |
| STDIN (Not used) | CONFORMS `- [x]` | Never read. |
| Env PWD | CONFORMS `- [x]` | Read only under `-L`, validated first. pwd.rs:58-60 |
| Env LC_*/LANG/NLSPATH | CONFORMS `- [x]` | setlocale + textdomain. pwd.rs:51-53 |
| Async events (Default) | N/A `- [x]` | |
| STDOUT `"%s\n"` | CONFORMS ✓ | raw `OsStr` bytes via `write_all` (fixed #PW5, phase 3). pwd.rs |
| STDERR (diagnostics only) | CONFORMS ✓ | `plib::diag` + `gettext` (fixed #PW4, phase 3). pwd.rs |
| Exit status (0 ok / >0 err) | CONFORMS `- [x]` | `?` → non-zero on error; 0 on success. pwd.rs:50,68 |
| No partial output on error | CONFORMS `- [x]` | Errors precede `println!`. pwd.rs:57-66 |

### Test coverage signal

- [x] ✓ added (phase 3) — `users/tests/pwd/mod.rs` covers the `-L`/`-P` precedence matrix, `$PWD` validity/PATH_MAX fallback, and absolute-output invariants.

---

## `tty`

**Implementation:** `users/tty.rs` (40 lines) + tests `users/tests/tty/mod.rs` (447 lines).
**Spec:** POSIX.1-2024, `tty` (pp. 3519–3520).
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/tty.md`
**Date:** 2026-06-18

### TL;DR

`tty` is largely conformant: it gates on `io::stdin().is_terminal()` first
(`tty.rs:27`), prints `not a tty` + exit 1 for non-terminals, and prints the
device name + exit 0 for terminals. One Major correctness bug: the name lookup
delegates to `plib::curuser::tty()` (`tty.rs:33`), which iterates
STDIN→STDOUT→STDERR (verified `curuser.rs:43-61`). The spec requires the name
of **standard input** only; when stdin is a tty but `ttyname(0)` fails (rare,
e.g. revoked/raced fd) the helper can return the name of stdout/stderr
instead. The obsolete `-s` option is correctly absent (removed in Issue 6).

### Priority issues

#### Major
- [x] **#TY1 — Terminal-name lookup falls back to stdout/stderr instead of being stdin-only.** ✓ fixed (phase 4) — now calls `curuser::ttyname_of(STDIN_FILENO)`; never consults fds 1/2. `users/tty.rs`.

#### Minor
- [x] **#TY2 — `not a tty` not localized.** ✓ fixed (phase 4) — `gettext`-wrapped. `users/tty.rs`.
- [x] **#TY3 — ttyname-failure-on-tty maps to exit 1, not >1.** ✓ fixed (phase 4) — ttyname() failure on a confirmed-tty stdin now emits a diagnostic and exits 2. `users/tty.rs`.

### Detailed conformance matrix

| Area | Status | Notes (file:line) |
|---|---|---|
| Options conform XBD 12.2; none defined | CONFORMS `- [x]` | Empty `Args`. tty.rs:18 |
| Obsolete `-s` absent | CONFORMS `- [x]` | Removed Issue 6; grep confirms no `-s`. |
| Operands (None) | CONFORMS `- [x]` | clap rejects extras (exit 2). tty.rs:18 |
| STDIN examined, not read | CONFORMS `- [x]` | `is_terminal()`. tty.rs:27 |
| Name reported = stdin | CONFORMS ✓ | `ttyname_of(STDIN_FILENO)` only (fixed #TY1, phase 4). tty.rs |
| Env LANG/LC_*/LC_MESSAGES | CONFORMS ✓ | `plib::diag` + `gettext`-wrapped output (fixed #TY2, phase 4). tty.rs |
| NLSPATH (XSI) | N/A | textdomain machinery. tty.rs:22-23 |
| Async events (Default) | N/A `- [x]` | |
| STDOUT `"%s\n"` name when tty | CONFORMS `- [x]` | tty.rs:34 |
| `"not a tty\n"` otherwise (to stdout) | CONFORMS `- [x]` | tty.rs:29 (informative output, correct channel) |
| STDERR (diagnostics only) | CONFORMS `- [x]` | clap errors → stderr |
| Exit 0 = stdin is a terminal | CONFORMS `- [x]` | implicit exit 0. tty.rs:34 |
| Exit 1 = stdin not a terminal | CONFORMS `- [x]` | tty.rs:30 |
| Exit >1 = error | CONFORMS ✓ | usage errors exit 2; ttyname-failure-on-tty now exits 2 (fixed #TY3, phase 4) |

### Test coverage signal

Good: non-terminal stdin → `not a tty\n` + exit 1, `--help`/`--version` exit 0, PTY-backed stdin → `/dev/...` + exit 0.
- [x] ✓ added (phase 4) — extra-operand → exit 2 (`test_tty_rejects_operand`). The existing `test_tty_pty_output_is_slave_path` already asserts the reported name is stdin's pts.

---

## `newgrp`

**Implementation:** `users/newgrp.rs` (815 lines).
**Spec:** POSIX.1-2024, `newgrp` (pp. 3252–3255), lines 108267–108420.
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/newgrp.md`
**Tests:** **none** — no `users/tests/newgrp/` module (confirmed by grep).
**Date:** 2026-06-18

### TL;DR

This implementation diverges fundamentally from POSIX `newgrp` semantics and
contains a privilege-confusion bug. In login (`-l`) mode the shell is spawned
**before** the group is resolved or changed (verified: `newgrp.rs:89-91` calls
`set_login_environment`, whose blocking `Command::new(user_shell).status()` at
`newgrp.rs:773-779` runs the shell to completion *then returns*, after which
lines 93-147 resolve and change the group) — so the new shell runs with the
**old** GID, the exact inverse of the utility's purpose. In normal (non-`-l`)
mode it **never creates a new shell at all** (verified `newgrp.rs:124-149`
return `Ok(())` after the GID change) — the spec's core requirement. The
optional `group` operand is marked `required = true` (`newgrp.rs:55-60`), so
the spec's "no operand restores the login groups" behavior is unreachable.
Group-password verification is Linux-only and compares a fresh hash against a
possibly-empty string. Treat as non-conformant and security-suspect.

### Priority issues

#### Critical
- [x] **#NG1 — `-l` login shell is exec'd with the OLD gid, before any group change.** ✓ fixed (phase 7) — `run()` now resolves+verifies+changes the group, drops privilege, and *then* `exec`s the shell (`CommandExt::exec`, replacing the process). No blocking `status()`. `users/newgrp.rs`.
- [x] **#NG2 — Normal mode never creates a new shell.** ✓ fixed (phase 7) — both modes `exec` a shell; the exit status is the shell's (behaviorally verified: piped `exit 7` → newgrp exits 7). `users/newgrp.rs` `exec_shell`.
- [x] **#NG3 — Group password verified against `/etc/gshadow` only, with weak comparison and silent fallthrough.** ✓ fixed (phase 8) — `verify_group_password` prefers gshadow then the group-DB `passwd` field, rejects locked/`!`/`*`/empty entries (fail-closed), and compares in constant time. macOS keeps membership-only with a documented limitation. `users/newgrp.rs`.

#### Major
- [x] **#NG4 — `group` operand is mandatory; "no operand = restore login groups" unimplemented.** ✓ fixed (phase 7) — operand is now optional; no-operand restores via `initgroups` + `setgid(pw_gid)`. `users/newgrp.rs` `restore_login_groups`.
- [x] **#NG5 — `setgroups` return values ignored in supplementary-list mutators.** ✓ fixed (phase 7) — supplementary changes go through `set_supplementary_gids`, whose result is checked and propagated. `users/newgrp.rs` `apply_group_change`.
- [x] **#NG6 — Privilege-drop ordering / uid manipulation.** ✓ fixed (phase 7) — order is now setgroups → setgid → (final) `setuid(getuid())`; the redundant re-setgroups is gone. **Correction:** the audit's "remove the `setuid(getuid())`" advice was wrong — that call is the **security-critical privilege drop** before exec (an installed-setuid-root newgrp must not hand the user a root shell); it is kept and centralized in `run()`. `users/newgrp.rs`.
- [x] **#NG7 — SHELL/login-shell handling diverges.** ✓ fixed (phase 7) — `-l` execs the passwd login shell with argv0 `-name`, re-inits HOME/SHELL/USER/LOGNAME and cwd=HOME; non-login honors `$SHELL` (then passwd shell, then `/bin/sh`). `users/newgrp.rs` `exec_shell`.

#### Minor
- [x] **#NG8 — Diagnostics not localized and inconsistently prefixed.** ✓ fixed (phase 7/8) — all diagnostics route through `plib::diag::error` (adds the `newgrp:` prefix) with `gettext` messages. `users/newgrp.rs`.
- [x] **#NG9 — Final error printed without newline / odd channel.** ✓ fixed (phase 7) — replaced by `plib::diag::error` (newline + prefix). `users/newgrp.rs`.
- [x] **#NG10 — `logger` emits an informational stderr line on every success.** ✓ fixed (phase 7) — the per-success audit line is removed. `users/newgrp.rs`.
- [x] **#NG11 — `-` first-arg handling unspecified-but-mishandled.** ✓ addressed (phase 7) — a `-` operand resolves to "no such group" → diagnostic, then invoke-anyway execs the shell (a deliberate, spec-permitted choice for the unspecified `-` case). `users/newgrp.rs`.
- [x] **#NG12 — Numeric-GID operand that is also a group *name* not preferred.** ✓ fixed (phase 7) — `find_matching_group` matches by name first, then numeric GID. Unit-tested. `users/newgrp.rs`.

### Detailed conformance matrix

#### Options

| Item | Status | Notes |
|---|---|---|
| `-l` (letter ell) | CONFORMS ✓ | login shell exec'd as `-name` after the group change (fixed #NG1/#NG7, phase 7). |
| XBD 12.2 conformance | CONFORMS ✓ | clap-based; `group` now optional (fixed #NG4, phase 7). |
| `-` first-arg unspecified | N/A `- [x]` | resolves to "no such group" → invoke-anyway execs shell (#NG11, phase 7). |

#### Operands / STDIN

| Item | Status | file:line |
|---|---|---|
| `group` by name | CONFORMS `- [x]` | find_matching_group name match. newgrp.rs |
| `group` by numeric GID | CONFORMS ✓ | name preferred over numeric (fixed #NG12, phase 7); unit-tested. newgrp.rs |
| No operand → restore login groups | CONFORMS ✓ | `initgroups` + `setgid(pw_gid)` (fixed #NG4, phase 7). newgrp.rs `restore_login_groups` |
| STDIN not used | CONFORMS `- [x]` | Password read from `/dev/tty`. newgrp.rs:705 |

#### Environment variables

| Var | Status | Notes |
|---|---|---|
| LANG/LC_ALL/LC_CTYPE | CONFORMS ✓ | `plib::diag::init_locale`; diagnostics `gettext`-wrapped (fixed #NG8, phase 7/8) |
| LC_MESSAGES | CONFORMS ✓ | all diagnostics via `plib::diag` + `gettext` (fixed #NG8) |
| NLSPATH (XSI) | N/A | gettext/textdomain used |
| SHELL | CONFORMS ✓ | honored in non-login mode (fixed #NG7, phase 7). newgrp.rs `exec_shell` |
| HOME | CONFORMS ✓ | set + cwd in `-l` from `pw_dir`. newgrp.rs `exec_shell` |

#### Asynchronous events
- [x] Default signal handling — no custom handling. (But the blocking `status()` model in `-l` is itself wrong — #NG1.)

#### STDOUT / STDERR

| Item | Status | file:line |
|---|---|---|
| STDOUT not used | CONFORMS `- [x]` | No stdout writes. |
| STDERR for diagnostics | CONFORMS ✓ | `plib::diag::error` (prefix + newline + gettext) (fixed #NG8/#NG9, phase 7). |
| Password prompt to stderr, read `/dev/tty` | CONFORMS `- [x]` | newgrp.rs `read_password` (spec 108325) |

#### Group-password / privilege handling

| Item | Status | file:line |
|---|---|---|
| Prompt only when non-member & pw required | CONFORMS ✓ | members short-circuit; non-members with a group password are prompted (fixed #NG3, phase 8). newgrp.rs `check_perms` |
| No prompt if member | CONFORMS `- [x]` | `check_perms` returns early for members. newgrp.rs |
| Password verification path | CONFORMS ✓ | gshadow→group-DB, fail-closed on locked/malformed, constant-time compare (fixed #NG3, phase 8); macOS membership-only (documented). newgrp.rs |
| crypt via libcrypt-rs | CONFORMS ✓ | `pw_encrypt` rejects locked/`!`/`*`/empty + unit-tested; `extract_salt` unit-tested. newgrp.rs |
| Echo-off terminal read | CONFORMS `- [x]` | toggles ECHO, always restores. newgrp.rs `read_password` |
| setgid/setgroups/setuid order & return checks | CONFORMS ✓ | setgroups→setgid→setuid, all checked; privilege dropped before exec (fixed #NG5/#NG6, phase 7). newgrp.rs |

#### Exit status / consequences of errors

| Item | Status | file:line |
|---|---|---|
| Success → shell's exit status | CONFORMS ✓ | both modes `exec` a shell; exit = shell's status (fixed #NG2, phase 7; behaviorally verified). newgrp.rs |
| Failure → >0 | CONFORMS `- [x]` | pre-exec errors (`get_password`/privilege-drop) exit 1; clap parse error exits 2. |
| Does NOT exec shell after failed switch | N/A ✓ | Per spec 108280-108281 a group-change failure *shall not* prevent the shell; newgrp now diagnoses and execs the shell with the unchanged (already-held, non-elevated) group. |

### Test coverage signal

- [x] ✓ added (phase 7/8) — 8 unit tests (`find_matching_group` name/numeric/numeric-is-name/missing, `extract_salt`, `constant_time_eq`, `pw_encrypt` rejects locked) + 3 integration tests (exec-shell + exit-status propagation, runs piped commands, unknown-group invoke-anyway). Privileged group-switching is verified manually (needs setuid-root); CI exercises the exec model and invoke-anyway path.

---

## `write`

**Implementation:** `users/write.rs` (323 lines) + tests `users/tests/write/mod.rs` (864 lines).
**Spec:** POSIX.1-2024, `write` (pp. 3645–3647), lines 123040–123115.
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/write.md`
**Date:** 2026-06-18

### TL;DR

The implementation covers the happy path (header banner, line copy, EOT-on-EOF,
SIGINT→EOT→exit 0, caret notation) but is built on a wrong I/O model: it reads
stdin in **cooked line mode** and tries to detect control characters (INTR,
EOF, ERASE, KILL, alert) as *literal bytes embedded in lines*, which is not
how a terminal driver delivers them. The biggest correctness defects: the
**alert (`^G`) and erase/kill characters are written to the sender's own
stdout instead of the recipient's terminal** (verified: `process_erase_or_kill`
`write.rs:128-155` and `alert_sender_terminal` `write.rs:168-176` both write to
`io::stdout()`), and several handlers use `.all()` "line consists *entirely* of
X" tests that never fire in practice. Recipient `mesg` permission has **no
superuser override**, SIGHUP/SIGPIPE/SIGQUIT are unhandled, and the
`.expect()`/`.unwrap()` write paths **panic** instead of exiting cleanly.

### Priority issues

> **Design note (phase 9):** the spec (123047-123048) processes input "while in
> **canonical input mode**", so the terminal driver consumes INTR/EOF/ERASE/KILL
> before write ever sees them — they never appear as data bytes. The redesign is
> therefore *per-character rendering of the canonical line content* (forward the
> alert, render other non-printables, pass print/space through), and the removal
> of the bogus byte-detection of driver-handled special chars — not raw mode.

#### Critical
- [x] **#WR1 — Alert and erase/kill characters written to sender stdout, not the recipient terminal.** ✓ fixed (phase 9) — the typed alert is now forwarded to the recipient via `render_line` (BEL byte passed through to the recipient file); the post-connect double-alert correctly stays on the sender's stdout. Erase/kill are the driver's job in canonical mode (removed). `users/write.rs`.
- [x] **#WR2 — Control-character detection model is wrong (cooked line input vs. termios special chars).** ✓ fixed (phase 9) — dropped `is_interrupt_or_eof`/`is_only_alert`/`is_only_erase_or_kill` (they detected bytes that never arrive in canonical mode). EOF is the read loop ending, INTR is the signal handler; each canonical line is rendered character-by-character. `users/write.rs` `render_line`.

#### Major
- [x] **#WR3 — No superuser override for recipient mesg permission.** ✓ fixed (phase 9) — `check_write_permission` short-circuits to true when `geteuid()==0`. `users/write.rs`.
- [x] **#WR4 — Write failures panic instead of clean diagnostic/exit.** ✓ fixed (phase 9) — all `.expect()`/`.unwrap()` on I/O are gone; `deliver()` diagnoses and exits 1 on write failure, and the read loop handles errors. `users/write.rs`.
- [x] **#WR5 — SIGHUP / SIGPIPE / SIGQUIT not handled.** ✓ fixed (phase 9; hardened in follow-up) — SIGINT, SIGHUP, and SIGPIPE install the EOT-and-exit-0 handler. (SIGQUIT keeps the permitted default action.) The handler is **async-signal-safe**: it reads the recipient fd from an `AtomicI32` and performs only a raw `write(2)` + `_exit` — no `Mutex` lock, file open, or buffered I/O (addresses a Copilot async-signal-safety review). `users/write.rs` `register_signal_handlers`/`handle_signal`.
- [x] **#WR6 — Multiple-login selection hard-excludes `console`.** ✓ fixed (phase 9) — picks the first reachable terminal (no console exclusion); the >1-login stdout notice is preserved. `users/write.rs` `select_terminal`.

#### Minor
- [x] **#WR7 — Tab/backspace mis-handled in printable test.** ✓ fixed (phase 9) — `render_line` sends `\t` as-is, renders other control bytes as `^X`/`^?`. `users/write.rs`.
- [x] ~~**#WR8 — Header banner uses locale-insensitive date.**~~ WON'T-FIX (phase 9) — the banner format is loosely specified (123041) and `LC_TIME`-formatted timestamps are not required; the `%Y-%m-%d %H:%M:%S` form is retained.
- [x] **#WR9 — `terminal` operand not validated; bare name blindly prefixed `/dev/`.** ✓ fixed (phase 9) — `validate_terminal` canonicalizes the path, requires it under `/dev`, and requires a character device (rejects `/dev/../etc/passwd`). `users/write.rs`.
- [x] **#WR10 — caret/printable logic is ASCII-only; ignores LC_CTYPE for multibyte.** ✓ addressed (phase 9) — high bytes (UTF-8 multibyte) pass through for the recipient terminal to interpret; control bytes are caret-rendered. Full per-wide-char `LC_CTYPE` isprint classification is a documented residual (byte-passthrough is correct for the common UTF-8 case). `users/write.rs`.

### Detailed conformance matrix

#### Options
- [x] N/A — spec OPTIONS "None"; no flags defined. write.rs:32-40. CONFORMS.

#### Operands / STDIN
- [x] `user_name` required — clap enforces, missing → exit 2 (`test_write_no_args`). CONFORMS (123075).
- [x] optional `terminal` — validated under `/dev` + char device (fixed #WR9, phase 9). write.rs `validate_terminal`.
- [x] STDIN read as canonical lines, rendered per-char to the recipient (fixed #WR2, phase 9). write.rs.
- [x] Multiple-login selection — first reachable terminal incl. console (fixed #WR6, phase 9). write.rs `select_terminal`.

#### Environment variables

| Variable | Impl | Status |
|---|---|---|
| LANG / LC_ALL | `setlocale(LcAll,"")` write.rs:263 | CONFORMS (via libc) |
| LC_CTYPE | ASCII classification only, write.rs:199-230 | PARTIAL (#WR7/#WR10) |
| LC_MESSAGES | `gettext`/`textdomain` write.rs:264 | CONFORMS (mechanism) |
| NLSPATH (XSI) | via gettextrs | CONFORMS (mechanism) |

#### Asynchronous events

| Signal | Impl | Status |
|---|---|---|
| SIGINT | `handle_sigint` writes `EOT\n`, exit(0) write.rs:235-260 | CONFORMS (tested `test_pty_sigint_handling`) |
| SIGHUP | EOT-and-exit-0 handler (fixed #WR5, phase 9) | CONFORMS ✓ |
| SIGPIPE | EOT-and-exit-0 handler; no panics (fixed #WR4/#WR5, phase 9) | CONFORMS ✓ |
| SIGQUIT | unhandled (default) | CONFORMS (default action permitted) |

#### STDOUT / STDERR
- [x] STDOUT informational message when recipient multiply-logged-in — write.rs:64-69, gated to >1 login. CONFORMS (123101-123103).
- [ ] STDERR diagnostics only — explicit diagnostics conform, but `.expect`/`.unwrap` panics emit backtraces. DIVERGES (#WR4).

#### Extended description / character rendering
- [x] alert → recipient — forwarded via `render_line` (fixed #WR1, phase 9). write.rs.
- [x] erase/kill via termios — handled by the driver in canonical mode; bogus byte-detection removed (fixed #WR2, phase 9). write.rs.
- [x] INTR/EOF → "EOT\n" and exit — INTR via signal handler, EOF via read-loop end (fixed #WR2, phase 9). write.rs.
- [x] print/space → recipient — `render_line` passes print/space + tab; high bytes pass through (fixed #WR7/#WR10, phase 9). write.rs.
- [x] other non-printable → `^X`/`^?` — write.rs:207-230. CONFORMS (123061-123062; tested). iexten N/A (impl-defined).

#### Exit status / consequences of errors
- [x] 0 success — EOF path / SIGINT exit(0). CONFORMS (123112).
- [x] >0 user not logged on / permission denied — write.rs:55,80,283. CONFORMS (123113; tested).
- [x] CONSEQUENCES OF ERRORS: Default — no panic paths; failures diagnose + exit (fixed #WR4, phase 9).

#### Sender identity
- [x] sender-login-id — `curuser::login_name()` write.rs:290 (note inherits the #LN1 `$USER` fallback). CONFORMS pragmatically.
- [x] sending-terminal — `curuser::tty()` write.rs:291. CONFORMS.

### Test coverage signal

The PTY suite is unusually thorough (real `posix_openpt`/`grantpt`/`unlockpt`/`ptsname` pairs, mutex-serialized): missing-arg exit 2, invalid user/terminal exit 1, header format, basic/multi-line copy + EOT + exit 0, SIGINT→EOT→exit 0, caret notation, alert passthrough, special/unicode chars. Not covered:
- [x] ✓ added (phase 9) — `test_write_rejects_path_traversal_terminal` (#WR9). Existing PTY tests (banner, basic/multi-line, EOT, SIGINT→0, caret, alert) still pass against the redesign.
- [ ] Residual: superuser / mesg-n permission (#WR3) and SIGHUP/SIGPIPE delivery aren't directly asserted (need a root / second-tty harness); covered by code inspection + behavioral spot-checks.
- [ ] PTY tests remain timing-based and self-skip if the binary is absent (pre-existing harness trait).

---

## `talk`

**Implementation:** `users/talk.rs` (1987 lines) + tests `users/tests/talk/mod.rs` (353 lines).
**Spec:** POSIX.1-2024, `talk` (pp. 3472–3475), lines 116790–116865.
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/talk.md`
**Date:** 2026-06-18

### TL;DR

A partial, largely non-conformant implementation that builds the ntalk
control-message machinery (binrw `CtlMsg`/`CtlRes`, the
LookUp/LeaveInvite/Announce/Delete handshake) but mis-handles nearly every
interactive POSIX requirement. Headline bug: **SIGINT must exit with status 0
(verified spec 116815-116816), but the handler exits `128 + SIGINT` = 130**
(verified `talk.rs:1853`). The full-screen UI is broken in ways that would
garble any real session — local echo and the peer window are written to
**stderr**, raw mode is only set inside a spawned reader thread (never for the
pre-connection phase), and `handle_character` does a debug `eprintln!` on every
keystroke (`talk.rs:843`). Several character-processing rules (alert, Ctrl-L
refresh, erase/kill via termios, print/space filtering, mutual termination) and
the recipient `mesg` permission path are absent, and the
**stdout-not-a-terminal requirement (spec 116820) is unimplemented**.

### Priority issues

> **Scope note (phase 10):** talk is a thread-heavy, full-screen interactive
> program with a hand-rolled curses-like engine that cannot be behaviorally
> exercised in CI (it needs two live peers + a daemon + a real terminal). The
> safe, high-confidence fixes below were applied and the existing tests kept
> green; the remaining items (#TK7, #TK9, #TK10, #TK14) require reworking the
> unverifiable UI/input engine and are deferred as documented residuals rather
> than changed blind. talk is therefore **not** promoted to Stage 6.

#### Critical
- [x] **#TK1 — SIGINT exits non-zero; spec mandates exit status 0.** ✓ fixed (phase 10) — `handle_signals` exits 0 for SIGINT (128+sig only for other trapped signals). `users/talk.rs`.
- [x] **#TK2 — Terminal echo / peer window written to stderr, corrupting the UI.** ✓ fixed (phase 10) — removed the per-keystroke debug `eprintln!`; `process_input_char`'s local echo now writes to stdout (the terminal). `users/talk.rs`.
- [x] **#TK3 — Raw mode set in the wrong scope and never restored on most exit paths.** ✓ fixed (phase 10) — original termios captured in a global; `restore_terminal()` runs in `handle_signals` and `handle_connection_close` before exit. `users/talk.rs`.

#### Major
- [x] ~~**#TK4 — SIGINT teardown only deletes invitations in network mode.**~~ Re-examined (phase 10): `talk_local` deletes both invitations explicitly on connect, the local talkd expires invitations (60 s) and is per-session, and network mode populates `DELETE_INVITATIONS` for the handler — no actionable defect.
- [x] **#TK5 — permission denial not surfaced; PermissionDenied & NotHere answers ignored.** ✓ fixed (phase 10) — `announce`/`announce_local` validate the daemon answer (`check_announce_answer`): `PermissionDenied`/`NotHere`/other → diagnostic + non-zero exit instead of ringing forever. `users/talk.rs`.
- [x] **#TK6 — stdout-not-a-terminal not checked.** ✓ fixed (phase 10) — `check_if_tty` also requires `io::stdout().is_terminal()` (spec 116820). `users/talk.rs`.
- [ ] **#TK7 — Required character-processing rules unimplemented (DEFERRED).** No `<alert>` forwarding / Ctrl-L refresh / LC_CTYPE print-space filtering in the raw-mode input engine. The typing/echo/backspace path works; the rest is enhancement to the unverifiable full-screen engine. **Deferred** — changing it blind risks silent regressions (see scope note). `users/talk.rs`.
- [x] **#TK8 — Mutual-termination semantics.** ✓ addressed (phase 10) — `handle_connection_close` restores the terminal and exits **0** on peer close; local EOF drops the TCP stream, which the peer observes and exits cleanly. Bilateral clean termination. `users/talk.rs`.

#### Minor
- [ ] **#TK9 — SIGWINCH not handled (no resize) (DEFERRED).** Resize needs re-querying `TIOCGWINSZ` + redrawing the split-screen engine; deferred with the UI-engine work. `users/talk.rs`.
- [ ] **#TK10 — Non-UTF-8 peer bytes dropped (DEFERRED).** Byte-wise peer handling is part of the deferred input-engine rework. `users/talk.rs`.
- [x] **#TK11 — IPv6 path panics.** ✓ fixed (phase 10) — `Osockaddr::from(&SocketAddrV6)` returns an empty (IPv4-only) address instead of `unimplemented!()`. `users/talk.rs`.
- [x] **#TK12 — `string_to_c_string`/`tty_to_c_string` panic on interior NUL.** ✓ fixed (phase 10) — shared `copy_to_c_buffer` stops at an interior NUL and truncates; never panics. `users/talk.rs`.
- [x] **#TK13 — Diagnostics not internationalized.** ✓ fixed (phase 10) — `plib::diag::init_locale` + top-level/`check_if_tty` diagnostics via `plib::diag`/`gettext`. (On-screen status strings stay literal — screen furniture, not `LC_MESSAGES` diagnostics.) `users/talk.rs`.
- [ ] **#TK14 — Long-line handling / buffer mismatch (DEFERRED).** Cosmetic; part of the deferred UI-engine rework. `users/talk.rs`.

### Detailed conformance matrix

#### Options
| Item | Status | Notes |
|---|---|---|
| No options defined by POSIX | PARTIAL | Spec OPTIONS "None." Impl adds non-standard `--local` (50-51) + clap `--help`/`--version`. `--local` materially changes transport. |

#### Operands / address forms
| Form | Status | Notes |
|---|---|---|
| bare `<user name>` | CONFORMS | `parse_address` → (address, local host). talk.rs:1175-1177 |
| `user@host` | CONFORMS | split on `@`. 1161-1166 |
| `host!user`, `host.user`, `host:user` | DIVERGES | all non-`@` delimiters treated as `host<delim>user`; `.`/`:` greedy `find` mis-splits FQDN-bearing users. 1153,1168-1173 |
| `terminal` operand | PARTIAL | stored/sent (475,1209) but never used to disambiguate multiple logins; no who-format validation. |

#### Environment variables
| Var | Status | Notes |
|---|---|---|
| LANG/LC_ALL | PARTIAL | only via `setlocale` 1941; no behavioral use |
| LC_CTYPE | MISSING | no print/space classification (#TK7); peer bytes forced through UTF-8 (#TK10) |
| LC_MESSAGES | MISSING | diagnostics not localized (#TK13) |
| NLSPATH (XSI) | PARTIAL | textdomain set (1942-1943) but unused for diagnostics |
| TERM | MISSING | no `TERM` read; hardcoded ANSI escapes ignore terminfo (spec 116812-116814 requires determining the terminal type) |

#### Asynchronous events
| Signal | Status | Notes |
|---|---|---|
| SIGINT | DIVERGES | handled (1840) but exits 130 not 0 (#TK1); local-mode teardown missing (#TK4) |
| SIGWINCH | MISSING | not registered (#TK9) |
| SIGQUIT | DIVERGES | custom clear-screen-exit-130 where spec says default action |
| SIGHUP | MISSING/N/A | default action; terminal left in raw mode on HUP (#TK3) |
| SIGPIPE | DIVERGES | custom handling; spec implies default for non-SIGINT |

#### STDIN / STDOUT / STDERR
| Item | Status | Notes |
|---|---|---|
| Copy stdin to recipient | PARTIAL | bytes sent over TCP. 949,964,971 |
| Non-terminal stdin → diag + non-zero | CONFORMS | `check_if_tty` → exit 1 (548-555,1968-1973); tested |
| Peer chars to stdout if terminal | DIVERGES | reader uses stdout (852-854) but local echo/status go to stderr (#TK2) |
| Non-terminal stdout → non-zero exit | MISSING | not checked (#TK6) |
| STDERR "None" | DIVERGES | heavy diagnostic + UI + debug `eprintln!` traffic (#TK2, 843) |

#### Talk-protocol / talkd interaction
| Item | Status | Notes |
|---|---|---|
| CtlMsg / CtlRes structures | PARTIAL | binrw big-endian (274-364); `to_bytes` uses `size_of::<CtlMsg>()` (322) — Rust struct size, fragile |
| LookUp / LeaveInvite / Announce / Delete | CONFORMS | all four for UDP (1678) and Unix-socket (1747) transports |
| Rendezvous handshake | CONFORMS | announce + invite + TCP accept. 1025-1094; connect to advertised addr 568-605 |
| Response-code handling | DIVERGES | only `Success` checked (#TK5) |
| Timeout/retry | PARTIAL | 5 s, then `process::exit(128)` deep in a helper (1714,1782), bypassing terminal restore |
| "ntalk" service lookup | CONFORMS | `getservbyname("ntalk","udp")` 1262,1355 |

#### Full-screen UI / character rendering
| Item | Status | Notes |
|---|---|---|
| Split two-window layout | PARTIAL | one divider line (1425-1447); raw ANSI, no curses |
| Connection status messages | PARTIAL | "[Waiting…]"/"[Ringing…]" (508,518,1043); wording differs from spec examples |
| `Message from …` announce text | MISSING | not generated (relies on talkd) |
| alert / Ctrl-L / erase / kill / print-space | MISSING | only `\x08`/`\x7f` backspace (714,951) (#TK7) |
| EOF / interrupt + notify peer | DIVERGES | (#TK8) |

#### Terminal restore on exit
| Path | Status | Notes |
|---|---|---|
| Normal reader-thread exit | CONFORMS | `RestoreTermOnDrop` 667 |
| SIGINT/SIGQUIT | MISSING | handler clears screen but no `tcsetattr` restore (1840-1854); guard in another thread (#TK3) |
| Hard `exit(128)` on timeout | MISSING | 1714,1782 exit without restore |
| `handle_connection_close` | MISSING | exit(130), no restore (1910-1915) |

#### Exit status / consequences of errors
| Item | Status | Notes |
|---|---|---|
| 0 on success | PARTIAL | `main` 0 on Ok (1958,1973), but SIGINT quit yields 130 (#TK1) |
| >0 on error | CONFORMS | error path exit 1 (1969); arg-parse failure exit 1 (1955) |
| Daemon-unreachable / not-a-tty | CONFORMS | non-zero; tested |
| CONSEQUENCES OF ERRORS: Default | DIVERGES | multiple deep `exit(128)`/`exit(130)` skip cleanup |

### Test coverage signal

Shallow — only startup/teardown plumbing, never interactive behavior. Six tests cover `--help` text, talkd socket create/cleanup, no-daemon error exit, not-a-TTY error exit, plus one `Osockaddr` byte-layout unit test. Not covered:
- [ ] SIGINT exit-status-0 (would catch #TK1); stdout-not-a-terminal (#TK6).
- [ ] Address-form parsing; the LookUp/Announce/Delete handshake against a live talkd.
- [ ] Response-code/permission handling (#TK5); terminal restore on signal (#TK3).
- [ ] All character-processing rules (#TK7/#TK8). No test connects two peers end-to-end, so the core exchange is unverified. Every test self-skips when the binary is missing.

---

## `talkd`

**Implementation:** `users/talkd.rs` (596 lines).
**Spec:** Not POSIX-specified — audited as **implicitly required by POSIX `talk`**, per the project's `crond` policy (see `cron/audit.md`). Judged against the implicit requirements of POSIX `talk` (`talk.md`), the BSD `ntalk` protocol it must serve, and secure-daemon practice.
**Tests:** in-file `#[cfg(test)]` unit tests for the in-memory registry only; **no** datagram/wire/loop coverage, no `users/tests/talkd/` module.
**Date:** 2026-06-18

### TL;DR

This is not really a talk daemon — it is a self-described "local-only
implementation for testing" (`talkd.rs:10-13`) that speaks the BSD `ntalk`
control protocol over a **Unix-domain datagram socket** at
`/var/run/talkd.sock`, not UDP port 518 (verified: grep finds no
`UdpSocket`/`518`/`ntalk`). It therefore cannot interoperate with any real
`talk` client, and even the bundled `talk` reaches it only via the
non-standard `--local` flag. Within that scope the protocol surface is
structurally complete and datagram parsing is panic-free, but the daemon
performs **no security checks whatsoever**: it never verifies the announced
callee exists or is logged in, never honors `mesg n`, **never actually
announces to any tty** (verified: `handle_announce` `talkd.rs:315-341` is a
stub that just stores the invitation and returns Success), never restricts the
socket mode, and never manages privilege. The invitation table is **unbounded**
(a trivial DoS). The headline risk is that the announce-to-tty path — the
entire reason a talk daemon exists — is unimplemented.

### Priority issues

#### Critical
- [x] **#TD1 — `ANNOUNCE` never writes to the recipient's terminal; the daemon's core function is a stub.** ✓ fixed (phase 11) — `handle_announce` calls `announce_to_tty`, which locates the callee's login terminal and writes the "Message from…/respond with: talk …" banner. `users/talkd.rs`.
- [x] **#TD2 — No validation that the announce target user exists or is logged in.** ✓ fixed (phase 11) — `announce_to_tty` validates the callee via `getpwnam` and scans utmpx for a login terminal, returning `NotHere` otherwise. `users/talkd.rs`.
- [x] **#TD3 — `mesg n` is never honored.** ✓ fixed (phase 11) — `mesg_permits` checks the callee tty's group/other write bit (root excepted); a denied tty yields `PermissionDenied`. `users/talkd.rs`.

#### Major
- [x] **#TD4 — Unbounded invitation table = trivial DoS.** ✓ fixed (phase 11) — `insert` is capped at `MAX_INVITATIONS` (sweeps expired first, then refuses with `Answer::Failed`); unit-tested. `users/talkd.rs`.
- [x] **#TD5 — Socket created with default (world-accessible) permissions; no ownership/path safety.** ✓ fixed (phase 11) — `daemon_loop` uses `symlink_metadata` to refuse replacing a symlink/non-socket path, binds under a tightened umask, and sets the socket to 0600 (verified `srw-------`). `users/talkd.rs`.
- [x] **#TD6 — No privilege model.** ✓ documented (phase 11) — the module header states the model: honor recipient `mesg`, write only to permitting ttys when unprivileged; a network deployment runs as root and is contained to tty writes. `users/talkd.rs`.
- [x] ~~**#TD7 — Not a network daemon: no UDP/port-518 binding.**~~ WON'T-FIX / documented divergence (phase 11, per the remediation decision) — talkd is deliberately a **hardened local-only** daemon (Unix-socket transport) serving `talk --local`, audited the way `uucp`'s SSH transport was. The module header documents that it is not interoperable with stock/remote `talk`. `users/talkd.rs`.

#### Minor
- [x] **#TD8 — `static mut SOCKET_PATH` read/written in a signal handler is unsound.** ✓ fixed (phase 11) — replaced with `OnceLock<PathBuf>` (+ `OnceLock<bool>` for the log sink). `users/talkd.rs`.
- [x] **#TD9 — `--foreground` flag is dead.** ✓ fixed (phase 11) — without `--foreground`, `daemonize()` double-forks, `setsid`s, `chdir("/")`s, and redirects std fds to `/dev/null` (verified the launcher detaches). `users/talkd.rs`.
- [x] **#TD10 — Diagnostics go to stderr, not syslog.** ✓ fixed (phase 11) — `log_info`/`log_err` write to stderr in the foreground and to `syslog` (LOG_DAEMON) once detached; messages are `gettext`-wrapped. `users/talkd.rs`.
- [x] **#TD11 — `LEAVE_INVITE` dedup keys only on (callee, caller), ignoring tty/addr.** ✓ fixed (phase 11) — `registry.refresh` updates the stored `tcp_addr`/`timestamp` on a matching re-invite; unit-tested. `users/talkd.rs`.
- [x] **#TD12 — No tests for the wire/loop path.** ✓ added (phase 11) — `test_ctlmsg_wire_round_trip` (serialize size + short/empty-datagram graceful parse errors), `test_invitation_table_is_bounded`, `test_leave_invite_refresh_updates_address`. `users/talkd.rs`.

### Detailed conformance matrix

#### Protocol message handlers
| Type (talk → talkd) | Code | Handler | Status |
|---|---|---|---|
| `LEAVE_INVITE` | 0 | `handle_leave_invite` :343 | PARTIAL — stores invite, stale-dedup bug (#TD11) |
| `LOOK_UP` | 1 | `handle_lookup` :294 | CONFORMS — matches (callee,caller), returns addr or `NotHere` |
| `DELETE` | 2 | `handle_delete` :379 | PARTIAL — deletes *all* invites by caller (:285), ignores echoed `id_num`; BSD deletes by id |
| `ANNOUNCE` | 3 | `handle_announce` :315 | MISSING (effect) — stores invite but never notifies callee (#TD1/#TD2/#TD3) |
| unknown type | — | :446-460 | CONFORMS — returns `Answer::UnknownRequest` |

- [x] All four request types `talk` sends are dispatched (:462-467); no dispatch gap that hangs the client.

#### Invitation table lifecycle
- [x] Stored — `insert` :273 (monotonic `next_id`, `wrapping_add` :275).
- [x] Matched on `LOOK_UP` — `find_for_callee` :262.
- [ ] Removed on `DELETE` — PARTIAL: `delete_by_caller` :285 removes by name, not the echoed `id_num` (:388).
- [ ] Expired on timeout — PARTIAL: `cleanup_expired` :255 (60 s) runs only on packet arrival (:420), never on a timer.
- [ ] Memory bounds — MISSING (#TD4).

#### Announce-to-tty / mesg permission
- [ ] Resolve callee / logged-in check — MISSING (#TD2).
- [ ] `mesg n` / tty group-write check — MISSING (#TD3).
- [ ] Write banner to recipient tty — MISSING (#TD1).
- [x] Attacker-controllable tty write — ✓ hardened (phase 11 + security follow-up): the announce target tty is validated as a char device under `/dev`, and the untrusted `caller` name is stripped of control characters before the banner is written (terminal-escape-injection guard, flagged by the commit security review; unit-tested). `r_tty` is only matched against utmpx entries, never written.

#### Datagram parsing safety
- [x] Short/truncated packet — graceful: `recv_from` into `[0u8;1024]` (:405,411); `CtlMsg::from_bytes` (:152) binrw `read_be` returns `Err` (not panic) on EOF; loop logs + `continue`s (:423-429). Verified no `unwrap`/`expect`/indexing on the network path (only `unwrap_or` :434, slice `buf[..len]` with `len≤1024`).
- [x] Oversized packet — truncated by `recv_from`; excess dropped (struct ~84 bytes).
- [ ] Version mismatch reply (:432-443) allocates a response with no rate limiting (ties to #TD4).

#### Privilege / security
- [ ] Drop privileges — MISSING (#TD6). Run as root — N/A (no privileged op attempted).
- [ ] Socket ownership/permission/symlink safety — MISSING (#TD5).
- [ ] Input validation on datagram fields — PARTIAL: type/version validated (:432,446); name/tty/addr accepted verbatim (#TD2).

#### Resource bounds
- [ ] Invitation table cap — MISSING (#TD4). Per-client cap — MISSING.
- [x] Loop bounds — `recv`-driven (:409-475), no busy-spin (blocking `recv_from`).

#### Logging / diagnostics
- [ ] syslog — MISSING (#TD10): `eprintln!` only.
- [x] Locale init — `setlocale`/`textdomain`/`bind_textdomain_codeset` present (:517-519); but operational strings not `gettext`-wrapped (only clap `about` :47). PARTIAL.

#### Asynchronous events / signals
- [ ] PARTIAL — `SIGINT`/`SIGTERM`/`SIGQUIT` handled (:494-509) to unlink the socket and `exit(0)`; uses unsound `static mut` (#TD8). `libc::signal` (not `sigaction`); no `SIGPIPE` (less relevant for datagrams); no `SIGHUP`-reload (acceptable — no config file).

### Test coverage signal

Weak. `users/talkd.rs:535-596` holds four unit tests covering only the in-memory `InvitationRegistry` (insert/find/delete) and two pure helpers. No coverage of `CtlMsg::from_bytes` round-trip, the `daemon_loop` dispatch, version/unknown-type rejection, `cleanup_expired` timing, the `LEAVE_INVITE` dedup branch (#TD11), or `DELETE`-by-caller semantics. `find_by_id`/`delete` are `#[cfg(test)]`-only methods (:268,280) existing solely for the unit tests — the production path is even thinner. Recommend:
- [ ] A datagram round-trip test (build `CtlMsg`, send over a `UnixDatagram` pair, assert `CtlRes`).
- [ ] A malformed/short-packet test asserting graceful drop.
- [ ] An unbounded-growth test that fails today and passes once #TD4 caps the table.

---

## Suggested PR groupings

Themed, small, landable units. Cross-utility shared-helper fixes are isolated
so a single change closes findings in several utilities.

- **PR A — "`plib::curuser` spec alignment"**: #LN1, #LN2, #LN3, #TY1. Give `login_name` a fail-able variant that does not consult `$USER`/`getpwuid`; make `tty()` query a caller-supplied fd (stdin-only for `tty`). Closes the shared-helper root causes.
- **PR B — "Exit-status ladders"**: #MG1, #MG2, #MG3 (mesg 0/1/>1), #TK1 (talk SIGINT→0), #TY3 (tty error→>1). Pure exit-code corrections, each with a small test.
- **PR C — "`logger` Issue-8 surface"**: #LG1, #LG2, #LG3, #LG4. Add clap option parsing (`-i`/`-f`/`-p`/`-t`), stdin/`-f` line reading, and the `user.notice` default.
- **PR D — "`newgrp` exec model + operand"**: #NG1, #NG2, #NG4, #NG7. Resolve+switch+`exec` (login arg0 for `-l`); make the operand optional with restore-on-absent. The structural rewrite.
- **PR E — "`newgrp` credential safety"**: #NG3, #NG5, #NG6. Group-DB + gshadow verification with fail-closed constant-time compare; checked `setgroups`; correct drop order.
- **PR F — "`write` recipient I/O + robustness"**: #WR1, #WR3, #WR4, #WR5, #WR6. Route alert/erase/kill to the recipient tty; superuser override; replace panics with clean exit; SIGHUP/SIGPIPE handling; console selection.
- **PR G — "`write`/`talk` canonical character processing"**: #WR2, #WR7, #WR10, #TK2, #TK7. The shared termios/print-space model (the largest design item).
- **PR H — "`talk` UI & protocol correctness"**: #TK3, #TK5, #TK6, #TK8, #TK9. Terminal-restore guard; Answer-enum handling; stdout-is-terminal; mutual termination; SIGWINCH.
- **PR I — "`talkd` becomes a real daemon"**: #TD1, #TD2, #TD3, #TD7. Announce-to-tty with passwd/utmp/`mesg` validation; UDP `ntalk` listener (or explicit local-only scoping).
- **PR J — "`talkd` hardening"**: #TD4, #TD5, #TD6, #TD8. Bounded invitation table; restrictive socket mode + symlink safety; privilege model; sound signal handler.
- **PR K — "`pwd` `-L`/`-P` precedence"**: #PW1, #PW2, #PW3, #PW5. Last-one-wins, byte-faithful output.
- **PR L — "`id` `-G` completeness"**: #I1, #I2, #I4. Union {gid,egid}∪getgroups(), effective-first, with name fallback.
- **PR M — "Crate-wide i18n"**: #I3, #LG (note), #MG5, #NG8, #NG9, #PW4, #TY2, #TK13, #TD10, #LN4. Route runtime diagnostics through `gettext()`; add `setlocale` to `logname`.
- **PR N — "Tests for the untested"**: #LN5, #PW6, #MG (tests), #NG (unit tests), #TD12. Close the zero-test gaps, prioritizing the security-sensitive `newgrp` pure functions.
