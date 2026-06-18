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

**Date:** 2026-06-18.

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
- [ ] **#LN1 — Falls back to the `$USER` environment variable; the spec forbids env-derived names.** `plib/src/curuser.rs:24-26` (consumed at `logname.rs:13`). The login name must be the `getlogin()` string; APPLICATION USAGE explicitly cautions against env-derived names. Fix: in the logname path, do not consult `$USER`; if `getlogin()` fails, error out.
- [ ] **#LN2 — Never fails on `getlogin()` failure; spec mandates non-zero exit + stderr diagnostic.** `logname.rs:12-16`, `curuser.rs:38` (`"unknown"` last resort). Spec 102992-102994: "Under the conditions where getlogin() would fail, the logname utility shall write a diagnostic message to standard error and exit with a non-zero exit status." Implementation always returns a string and exits 0. Fix: return `Option`/`Result` from a logname-specific helper; on `None`, diagnose + `exit(1)`.

#### Minor
- [ ] **#LN3 — `getpwuid()` fallback diverges from getlogin() semantics.** `curuser.rs:30-37`. `getlogin()` reflects the controlling-terminal/session name (can differ after `su`); folding to `getpwuid(getuid())` masks failure and can yield a different name. Same fix as #LN2.
- [ ] **#LN4 — No locale initialization.** `logname.rs` has no `setlocale`/`textdomain` (compare `pwd.rs:51-53`). Diagnostics, once added, should be localizable. Fix: add the standard locale-init wrapper.
- [ ] **#LN5 — No tests.** Add a module asserting output matches `getlogin()`/`id -un` with a trailing newline, and that the failure path errors.

### Detailed conformance matrix

| Area | Status | Notes (file:line) |
|---|---|---|
| Options (None) | CONFORMS `- [x]` | No args parsed. logname.rs:12 |
| Operands (None) | CONFORMS `- [x]` | None accepted. logname.rs:12 |
| STDIN (Not used) | CONFORMS `- [x]` | Never read. |
| Env — LOGNAME ignored | CONFORMS `- [x]` | Not consulted. curuser.rs:12-39 |
| Env — USER ignored | DIVERGES `- [ ]` | `$USER` IS consulted as fallback. curuser.rs:24-26 (#LN1) |
| Env — LC_*/LANG/NLSPATH | MISSING `- [ ]` | No `setlocale`/`textdomain`. logname.rs (#LN4) |
| Async events (Default) | N/A `- [x]` | |
| STDOUT `"%s\n"` | CONFORMS `- [x]` | `println!` emits name + `\n`. logname.rs:15 |
| STDERR (diagnostics only) | PARTIAL `- [ ]` | Nothing written; spec requires a diagnostic on failure, which never happens. (#LN2) |
| Login name = getlogin() | DIVERGES `- [ ]` | getlogin → `$USER` → getpwuid → `"unknown"`. curuser.rs:14-38 (#LN1/#LN3) |
| Exit status (0 ok / >0 err) | DIVERGES `- [ ]` | Always 0 even with no login name. curuser.rs:38 (#LN2) |

### Test coverage signal

Not covered:
- [ ] **Everything** — zero tests. The `$USER` fallback and missing failure path are exactly what a test (USER unset + no controlling tty → expect error, not `"unknown"`) would catch.

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
- [ ] **#LG1 — No option parsing; `-f`/`-i`/`-p`/`-t` are treated as message text.** `logger.rs:19-21`. `args.join(" ")` over all argv means `logger -t mytag hi` logs the literal string `-t mytag hi`. Spec 102862,102877-102905. Fix: parse options (clap) before building the body; strip recognized flags.
- [ ] **#LG2 — No STDIN / `-f file` message-body input.** `logger.rs:19-21`. Spec 102869-102871,102909-102915: with no operands and no `-f`, read message bodies from stdin (one non-empty line per message); `-f file` reads from a file. Current code logs an empty string when no operands are given (locked in by `test_logger_no_args`). Fix: when operands empty, read stdin/`-f` line-by-line, one syslog message per non-empty line.

#### Major
- [ ] **#LG3 — Default priority is `user.err`, not `user.notice`.** `logger.rs:35` calls `writer.err(&log_str)`. Spec 102902: "If the −p option is not specified, the priority shall be user.notice." `err` is a higher severity than `notice`. Fix: use the notice-level write and honor `-p facility.level`.

#### Minor
- [ ] **#LG4 — Default tag/PID hardcoded; `-i`/`-t` unsupported.** `logger.rs:23-28` sets `process:"logger"`, `pid:0`. Spec 102903-102905: default tag is the invoking user/effective name; `-t` overrides; `-i` adds the PID. Fix: default tag to the login/user name, wire `-t`, populate `pid` from `std::process::id()` under `-i`.
- [ ] **#LG5 — Operand-embedded newline sent as one multi-line record.** `logger.rs:21`. Spec treats each input *line* as a separate message; operand newline behavior is unspecified, so low severity, but ties to #LG2.

### Detailed conformance matrix

#### Options — DIVERGES (all four missing)
- [ ] `-f file` MISSING — no file reading. logger.rs:19-21.
- [ ] `-i` MISSING — `pid:0` hardcoded. logger.rs:27.
- [ ] `-p priority` MISSING — priority fixed at `err`, facility `LOG_USER`. logger.rs:24,35.
- [ ] `-t tag` MISSING — tag fixed `"logger"`. logger.rs:26.
- No `--` handling / XBD 12.2 conformance since no options are parsed.

#### Operands / STDIN
- [x] `string...` operands concatenated with single space — CONFORMS. logger.rs:21 (spec 102907-102909; RATIONALE "similar to echo"). Verified by `test_logger_multiple_args`.
- [ ] STDIN fallback when no operands — MISSING/DIVERGES. Spec 102909-102915; logs empty string instead. logger.rs:19-21 (grep: no `stdin`/`BufRead`).

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
- [ ] **#MG1 — Exit status does not reflect messaging state.** `users/mesg.rs:122-137`. Spec 106996-106998: exit 0 if receiving messages is allowed, 1 if not, >1 on error. The impl returns `Ok(())` (exit 0) for `show_mesg`, `mesg y`, and `mesg n` uniformly. So `mesg n` exits 0 and `mesg` on a locked tty exits 0 — both wrong. Fix: compute `allowed = (st_mode & (S_IWGRP|S_IWOTH)) != 0` from the state **before** any change, then `process::exit(if allowed {0} else {1})`.
- [ ] **#MG2 — Error paths exit 1, indistinguishable from "not messageable".** `users/mesg.rs:56,93,129,133`. "tty not found", invalid operand, and fchmod failure all bubble through `main() -> Result<…>` → exit 1. Spec reserves 1 for "not allowed" and requires `>1` for errors. Fix: handle errors explicitly and `process::exit(2)`.

#### Major
- [ ] **#MG3 — Exit must reflect the state *before* the command.** `users/mesg.rs:90-120` (depends on #MG1 fix). The pre-change `st` (mesg.rs:97/129) holds the old mode, so the #MG1 fix must read the bit from `st` (pre-change), not re-stat after `fchmod`. Ensure ordering.

#### Minor
- [ ] **#MG4 — Accepts `Y`/`N` beyond the POSIX `y`/`n` operands.** `users/mesg.rs:83-84`. Spec lists only lowercase `y`/`n` in the POSIX locale. Harmless extension; optionally restrict or document.
- [ ] **#MG5 — Diagnostics partially unlocalized.** `users/mesg.rs:56,85` use raw English (`"tty not found"`, `"invalid operand"`) not `gettext`-wrapped, while mesg.rs:64,115 are. Consistency fix.

### Detailed conformance matrix

| Area | Status | Notes (file:line) |
|---|---|---|
| Options (None) | CONFORMS `- [x]` | No options. |
| `y` grants permission | CONFORMS `- [x]` | adds S_IWGRP\|S_IWOTH. mesg.rs:99-104 |
| `n` denies permission | CONFORMS `- [x]` | clears the bits. mesg.rs:105-110 |
| No operand → report, no change | CONFORMS `- [x]` | `None => show_mesg`. mesg.rs:132 |
| Only `y`/`n` in POSIX locale | DIVERGES (minor) | also accepts `Y`/`N`. mesg.rs:83-84 (#MG4) |
| STDIN (Not used) | CONFORMS `- [x]` | stdin only inspected for is_terminal. |
| First tty among stdin,stdout,stderr | CONFORMS `- [x]` | find_tty. mesg.rs:33-43 |
| Operates on that device's mode bits | CONFORMS `- [x]` | fstat/fchmod on chosen fd. mesg.rs:53-69,113 |
| Env LANG/LC_*/LC_MESSAGES | PARTIAL | setlocale mesg.rs:123; some msgs unlocalized (#MG5) |
| Async events (Default) | N/A `- [x]` | |
| STDOUT (state, unspecified format) | CONFORMS `- [x]` | `is y`/`is n`, only on no-operand. mesg.rs:74,76,132 |
| STDERR (diagnostics only) | CONFORMS `- [x]` | mesg.rs:64,115 |
| Exit 0 = receiving allowed | MISSING `- [ ]` | always 0 on success (#MG1) |
| Exit 1 = not allowed | MISSING `- [ ]` | never produced for "denied" (#MG1) |
| Exit >1 = error | DIVERGES `- [ ]` | errors exit 1, not >1 (#MG2) |

### Test coverage signal

Not covered:
- [ ] **Everything** — no `users/tests/mesg/` module. Two Critical exit-status bugs that `mesg n; echo $?` on a PTY would catch immediately. Recommend PTY-backed round-trip tests for the bit change and the pre-change-based exit code, plus invalid-operand → exit 2 and no-tty → exit 2.

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
- [ ] **#PW1 — `-L`/`-P` "last one wins" not implemented; `-P` ignored.** `users/pwd.rs:24-29,57-64`. Spec 112743-112744: "If both −L and −P are specified, the last one shall apply." Clap stores two independent bools; the code reads only `args.env` and **never** `args.process` (verified: line 28 field is unused). So `-P` is a no-op and `-L -P` still behaves logically. Fix: track option order so the last of `-L`/`-P` decides; on physical, ignore `$PWD` and use `current_dir()`.
- [ ] **#PW2 — `-P` does not explicitly guarantee symlink-free resolution.** `users/pwd.rs:57`. `current_dir()` is canonical on Linux, so default/-P output is physically correct in practice, but because `-P` is never branched there is no explicit canonicalization. Fix: for `-P`, optionally `canonicalize()`, or document reliance on getcwd. (Tie to #PW1.)

#### Minor
- [ ] **#PW3 — `$PWD` length not bounded against {PATH_MAX}.** `users/pwd.rs:58-63`. Spec 112732-112734 makes behavior unspecified beyond PATH_MAX, permitting `-P` fallback. Fix: if `dir.len() >= PATH_MAX`, fall back to `current_dir()`.
- [ ] **#PW4 — Diagnostics not localized; default Rust error formatting.** `users/pwd.rs:50,57`. `Box<dyn Error>` from `?` yields an unlocalized message. Fix: catch and `eprintln!(gettext(...))`.
- [ ] **#PW5 — `to_string_lossy()` can corrupt non-UTF-8 paths.** `users/pwd.rs:66`. Invalid UTF-8 bytes become U+FFFD. Fix: write the raw `OsStr` bytes via `write_all`.
- [ ] **#PW6 — No tests.** Add: default output absolute & equals getcwd; `-L` with a valid `$PWD` symlink prints the logical path; `-L` with `$PWD` containing `..` is rejected; `-L -P` prints physical. The stale `pwd.rs:10` TODO (about `-L` "." normalization) is partly moot — `dirname_valid` rejects `.` — but unverified.

### Detailed conformance matrix

| Area | Status | Notes (file:line) |
|---|---|---|
| Options conform to XBD 12.2 | PARTIAL | clap handles `--`/`--help`; option relationship wrong (#PW1). pwd.rs:21-29 |
| Option −L (use valid `$PWD`) | CONFORMS `- [x]` | Validates absolute + no `.`/`..`. pwd.rs:31-48,58-64 |
| Option −P (physical, no symlinks) | DIVERGES `- [ ]` | `args.process` declared but never read. pwd.rs:28,57 (#PW1/#PW2) |
| Last −L/−P wins; default −L | DIVERGES `- [ ]` | No precedence logic; default is `current_dir()`. pwd.rs:57-64 (#PW1) |
| `$PWD` absolute / no `.`,`..` | CONFORMS `- [x]` | `dirname_valid`. pwd.rs:31-48 |
| `$PWD` > PATH_MAX handling | PARTIAL | No length check (unspecified per spec). pwd.rs:58 (#PW3) |
| Operands (None) | CONFORMS `- [x]` | clap errors on extras. pwd.rs:23-29 |
| STDIN (Not used) | CONFORMS `- [x]` | Never read. |
| Env PWD | CONFORMS `- [x]` | Read only under `-L`, validated first. pwd.rs:58-60 |
| Env LC_*/LANG/NLSPATH | CONFORMS `- [x]` | setlocale + textdomain. pwd.rs:51-53 |
| Async events (Default) | N/A `- [x]` | |
| STDOUT `"%s\n"` | PARTIAL | `println!` adds `\n`; lossy UTF-8 (#PW5). pwd.rs:66 |
| STDERR (diagnostics only) | PARTIAL | Errors via `?`; unlocalized (#PW4). pwd.rs:50,57 |
| Exit status (0 ok / >0 err) | CONFORMS `- [x]` | `?` → non-zero on error; 0 on success. pwd.rs:50,68 |
| No partial output on error | CONFORMS `- [x]` | Errors precede `println!`. pwd.rs:57-66 |

### Test coverage signal

Not covered:
- [ ] **Everything** — no tests. The unused `args.process` (#PW1) is exactly what a `-L -P` precedence test would expose; a `$PWD`-with-`..` test would confirm `dirname_valid`.

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
- [ ] **#TY1 — Terminal-name lookup falls back to stdout/stderr instead of being stdin-only.** `users/tty.rs:33` → `plib/src/curuser.rs:43-61`. The shared `tty()` helper loops over STDIN, STDOUT, STDERR and returns the first named one. Spec: "the name of the terminal that is open as standard input … equivalent to `ttyname()`." Fix: call `libc::ttyname(STDIN_FILENO)` directly (or pass an fd to the helper), never consulting fds 1/2.

#### Minor
- [ ] **#TY2 — `not a tty` not localized.** `users/tty.rs:29,36`. Hardcoded, not `gettext`-wrapped (functionally fine in the POSIX locale). Fix: wrap for consistency.
- [ ] **#TY3 — ttyname-failure-on-tty maps to exit 1, not >1.** `users/tty.rs:35-38`. After the `is_terminal` check at line 27, the `None` arm is only reachable when stdin *is* a terminal but `ttyname` fails — an error, which should be `>1`, not the "not a terminal" status 1. Rare race. Fix: treat that case as exit 2.

### Detailed conformance matrix

| Area | Status | Notes (file:line) |
|---|---|---|
| Options conform XBD 12.2; none defined | CONFORMS `- [x]` | Empty `Args`. tty.rs:18 |
| Obsolete `-s` absent | CONFORMS `- [x]` | Removed Issue 6; grep confirms no `-s`. |
| Operands (None) | CONFORMS `- [x]` | clap rejects extras (exit 2). tty.rs:18 |
| STDIN examined, not read | CONFORMS `- [x]` | `is_terminal()`. tty.rs:27 |
| Name reported = stdin | DIVERGES `- [ ]` | helper also consults stdout/stderr. tty.rs:33 (#TY1) |
| Env LANG/LC_*/LC_MESSAGES | PARTIAL | setlocale tty.rs:21; no localized strings (#TY2) |
| NLSPATH (XSI) | N/A | textdomain machinery. tty.rs:22-23 |
| Async events (Default) | N/A `- [x]` | |
| STDOUT `"%s\n"` name when tty | CONFORMS `- [x]` | tty.rs:34 |
| `"not a tty\n"` otherwise (to stdout) | CONFORMS `- [x]` | tty.rs:29 (informative output, correct channel) |
| STDERR (diagnostics only) | CONFORMS `- [x]` | clap errors → stderr |
| Exit 0 = stdin is a terminal | CONFORMS `- [x]` | implicit exit 0. tty.rs:34 |
| Exit 1 = stdin not a terminal | CONFORMS `- [x]` | tty.rs:30 |
| Exit >1 = error | PARTIAL | usage errors exit 2; ttyname-failure-on-tty wrongly exits 1 (#TY3) |

### Test coverage signal

Good: non-terminal stdin → `not a tty\n` + exit 1 (mod.rs:272-296), `--help`/`--version` exit 0, PTY-backed stdin → `/dev/...` + exit 0 (mod.rs:353-446). Not covered:
- [ ] The reported name equals stdin's tty *specifically* (would catch #TY1 if stdin/stdout point at different ttys).
- [ ] Extra-operand → exit 2 usage path.

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
- [ ] **#NG1 — `-l` login shell is exec'd with the OLD gid, before any group change.** `users/newgrp.rs:89-91` → `set_login_environment` (`753-787`) runs `Command::new(user_shell).status()` (773-779), **blocking until that shell exits**; only afterward (93-147) does the code resolve the group and call `setgid`/`setegid`. The interactive shell therefore has the unchanged GID, and the switch happens against a now-exited session. Fix: resolve group → verify membership/password → change real+effective GID and supplementary list → only then `exec` the (login or normal) shell (replace the process, not blocking `status()`).
- [ ] **#NG2 — Normal mode never creates a new shell.** Spec 108272-108279,108354-108355: newgrp shall create a *new shell execution environment* whose exit status is the shell's. The non-`-l` branch (`newgrp.rs:124-149`) changes the GID of the short-lived `newgrp` process then `return Ok(())` → exit 0. No shell is started. Fix: after the group change, `exec` `$SHELL` (or the passwd shell).
- [ ] **#NG3 — Group password verified against `/etc/gshadow` only, with weak comparison and silent fallthrough.** `check_perms` (`newgrp.rs:530-582`) does password verification only under `#[cfg(target_os="linux")]`; `get_shadow_password` (601-621) returns `Ok(String::new())` when the group has no gshadow entry, and the group-database `passwd` field (`/etc/group`) is never used. On macOS, non-root always errs (571-577), so a member can never switch. Fix: verify against the group DB password (and gshadow on Linux) via platform crypt, fail closed on any error, constant-time compare.

#### Major
- [ ] **#NG4 — `group` operand is mandatory; "no operand = restore login groups" unimplemented.** `users/newgrp.rs:55-60` declares the operand `required = true`. Spec 108297-108299: with **no operands** newgrp restores the effective group and supplementary list from the user's database entries. Running `newgrp` with no args errors. Fix: make the operand optional; when absent, reset GID to `pw_gid` and supplementary groups via `initgroups`.
- [ ] **#NG5 — `setgroups` return values ignored in supplementary-list mutators.** `add_gid_to_groups` (`newgrp.rs:170-172`) and `remove_gid_from_groups` (195-197) discard the `setgroups` result. A failed change is silently swallowed, after which `change_*_gid_and_uid` proceeds with an inconsistent credential state. Fix: check the return and propagate `io::Error::last_os_error()`, aborting on failure.
- [ ] **#NG6 — Privilege-drop ordering / uid manipulation is unsound.** `change_gid_and_uid`/`change_effective_gid_and_uid` (`newgrp.rs:296-379`) call `setgid`/`setegid`, then `setgroups`, then `setuid(getuid())`; the canonical order (setgroups → setgid → setuid, dropping supplementary groups first while privileged) is not followed, and `change_effective_gid_and_uid` re-`setgroups` the same list it just read (372-373). Fix: adopt the standard drop order; remove the redundant setgroups; don't call setuid unless actually dropping a setuid privilege.
- [ ] **#NG7 — SHELL/login-shell handling diverges.** `set_login_environment` (`newgrp.rs:753-787`) always uses the passwd `pw_shell` (ignoring `$SHELL`), spawns a **non-login** invocation (no `-`/`-name` arg0), and uses blocking `status()` instead of `exec`. Spec 2.13 (per 108272-108279) requires the environment re-initialized "as if logged in" for `-l`. Fix: for `-l`, exec with arg0 prefixed `-`; respect passwd shell for login, `$SHELL` otherwise; default `/bin/sh`.

#### Minor
- [ ] **#NG8 — Diagnostics not localized and inconsistently prefixed.** `newgrp.rs:97,106-109,174,510-512,563` use raw `eprintln!` literals though the catalog is initialized (790-792); some omit the `newgrp:` prefix (e.g. 174 `"Error: No room…"`). Fix: route through `gettext` and prefix `newgrp:`.
- [ ] **#NG9 — Final error printed without newline / odd channel.** `main` line 811 `eprint!("{}", err)` — no trailing newline, no prefix. Fix: `eprintln!("newgrp: {err}")`.
- [ ] **#NG10 — `logger` emits an informational stderr line on every success.** `newgrp.rs:501-513`. Spec STDERR is for diagnostics/prompts only (108345-108348). Fix: drop or gate behind verbose/syslog.
- [ ] **#NG11 — `-` first-arg handling unspecified-but-mishandled.** Spec 108300 says results are unspecified if the first arg is `-`; clap accepts `-` as the operand value → "group not found". Acceptable (unspecified), but worth a deliberate decision.
- [ ] **#NG12 — Numeric-GID operand that is also a group *name* not preferred.** Spec 108317-108320: if `group` is numeric *and* exists as a group **name**, the named group's GID is used. `find_matching_group` (`newgrp.rs:475-485`) matches numeric by GID first. Edge case.

### Detailed conformance matrix

#### Options

| Item | Status | Notes |
|---|---|---|
| `-l` (letter ell) | DIVERGES `- [ ]` | Parsed (49-53) but exec's shell with old GID, non-login, blocking (#NG1/#NG7). |
| XBD 12.2 conformance | PARTIAL | clap-based; `group` wrongly mandatory (#NG4). |
| `-` first-arg unspecified | N/A `- [x]` | Falls through to "group not found" (#NG11). |

#### Operands / STDIN

| Item | Status | file:line |
|---|---|---|
| `group` by name | CONFORMS `- [x]` | find_matching_group name match. newgrp.rs:483 |
| `group` by numeric GID | PARTIAL | GID match 475-480; numeric-is-also-a-name nuance not honored (#NG12). |
| No operand → restore login groups | MISSING `- [ ]` | Operand `required=true`. newgrp.rs:55-60 (#NG4) |
| STDIN not used | CONFORMS `- [x]` | Password read from `/dev/tty`. newgrp.rs:705 |

#### Environment variables

| Var | Status | Notes |
|---|---|---|
| LANG/LC_ALL/LC_CTYPE | PARTIAL | setlocale newgrp.rs:790; messages not gettext-routed (#NG8) |
| LC_MESSAGES | DIVERGES | catalog init'd but `eprintln!` literals bypass it (#NG8) |
| NLSPATH (XSI) | N/A | gettext/textdomain used |
| SHELL | DIVERGES `- [ ]` | Ignored; `pw_shell` used. newgrp.rs:771 (#NG7) |
| HOME | PARTIAL | set in `-l` from `pw_dir`. newgrp.rs:768 |

#### Asynchronous events
- [x] Default signal handling — no custom handling. (But the blocking `status()` model in `-l` is itself wrong — #NG1.)

#### STDOUT / STDERR

| Item | Status | file:line |
|---|---|---|
| STDOUT not used | CONFORMS `- [x]` | No stdout writes. |
| STDERR for diagnostics | PARTIAL | Used (97,563,802,811) but unlocalized, inconsistent prefix/newline (#NG8/#NG9). |
| Password prompt to stderr, read `/dev/tty` | CONFORMS `- [x]` | newgrp.rs:710,705 (spec 108325) |

#### Group-password / privilege handling

| Item | Status | file:line |
|---|---|---|
| Prompt only when non-member & pw required | PARTIAL | 538-547 conflates pw-needed with empty user pw |
| No prompt if member | CONFORMS `- [x]` | newgrp.rs:539 |
| Password verification path | DIVERGES `- [ ]` | gshadow-only, Linux-only, weak compare, empty-string fallthrough. 530-621 (#NG3) |
| crypt via libcrypt-rs | PARTIAL | `pw_encrypt` 654-682; `extract_salt` 633-641 brittle |
| Echo-off terminal read | CONFORMS `- [x]` | toggles ECHO, restores. 703-741 |
| setgid/setgroups/setuid order & return checks | DIVERGES `- [ ]` | core syscalls checked but add/remove ignore setgroups (#NG5); ordering unsound (#NG6); macOS errs for all non-root (571-577) |

#### Exit status / consequences of errors

| Item | Status | file:line |
|---|---|---|
| Success → shell's exit status | MISSING `- [ ]` | No shell exec in normal mode (#NG2); `-l` returns 0 after blocking shell. |
| Failure → >0 | PARTIAL `- [x]` | `main` exit_code=1 on Err (809-814); parse error exits 1 (806). |
| Does NOT exec shell after failed switch | PARTIAL | Normal-mode errors propagate before exit; but #NG1 makes `-l` exec the shell *before* the switch — the inverse hazard. |

### Test coverage signal

Not covered:
- [ ] **Everything** — zero tests, and their absence is itself a Major finding given #NG1–#NG3. Even without privilege, the pure functions are unit-testable and should be covered before any refactor: `find_matching_group` (name vs numeric vs numeric-that-is-a-name), `extract_salt`, the supplementary-group add/remove/effective-in-list decision matrix (spec 108282-108296), and operand parsing (no-operand restore, `-l`, `-`). Behavioral conformance ("does `newgrp grp` land you in an interactive shell with the new real+effective GID?") cannot be asserted today because, per #NG1/#NG2, it does not happen.

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

#### Critical
- [ ] **#WR1 — Alert and erase/kill characters written to sender stdout, not the recipient terminal.** `write.rs:168-176` (`alert_sender_terminal` writes to `io::stdout()`), `write.rs:128-155` (`process_erase_or_kill` writes BS/space/BS and other chars to `io::stdout()`), invoked at `write.rs:300,309,311`. The post-connect double-alert at line 300 is to the *sender's* terminal (correct per spec 123042-123044), but the typed-alert passthrough (309) and rendered erase/kill (311) must reach the *recipient* (spec 123051 "Typing `<alert>` shall write the `<alert>` character to the recipient's terminal"). Fix: route the typed-alert passthrough and rendered erase/kill through `write_to_terminal(&terminal, …)`.
- [ ] **#WR2 — Control-character detection model is wrong (cooked line input vs. termios special chars).** `write.rs:302-317` reads `stdin.lock().lines()` (canonical, NL-stripped) then classifies whole lines via `is_interrupt_or_eof`/`is_only_alert`/`is_only_erase_or_kill` (`write.rs:179-196`), each requiring the line to consist *entirely* of that char (`.all(...)`). In a real session INTR/EOF/ERASE/KILL are consumed by the driver and never appear in the data stream, and a mixed line like `hi\x03there` matches none of these branches. Spec 123047-123055 describes per-character processing in canonical input mode. Fix: redesign to process input bytewise honoring termios special chars (or at minimum handle embedded INTR/EOF anywhere in a line).

#### Major
- [ ] **#WR3 — No superuser override for recipient mesg permission.** `write.rs:83-120` `check_write_permission` returns true only if owner/group/other write bits are set; root (uid 0) is refused when the recipient has `mesg n`. Spec 123067-123069. Fix: short-circuit to `true` when `geteuid() == 0`.
- [ ] **#WR4 — Write failures panic instead of clean diagnostic/exit.** `write.rs:159-165` (`write_to_terminal` uses `.expect`), `write.rs:136-154` (`process_erase_or_kill` `.expect`), `write.rs:304` (`line.unwrap()`). A closed recipient tty (SIGPIPE/EIO) panics with a backtrace rather than writing EOT and exiting. Spec STDERR 123104-123105. Fix: replace `.expect`/`.unwrap` with graceful error → stderr → `exit(>0)`; best-effort EOT on broken pipe.
- [ ] **#WR5 — SIGHUP / SIGPIPE / SIGQUIT not handled.** Only `SIGINT` is trapped (`write.rs:235-260`). Spec 123099-123100 permits default for "all other signals", but combined with #WR4's `.expect`, a closed recipient tty kills write uncleanly. Fix: install the EOT-and-exit handler for SIGHUP/SIGPIPE too.
- [ ] **#WR6 — Multiple-login selection hard-excludes `console`.** `write.rs:58-73` skips `line == "console"` unconditionally; a user logged in only on console falls through to error exit even though reachable. The >1-login stdout notice (123063-123066) is otherwise correct. Fix: don't hard-exclude console; pick a reachable tty.

#### Minor
- [ ] **#WR7 — Tab/backspace mis-handled in printable test.** `write.rs:199-202` `contains_printable_or_space` uses `is_ascii_graphic() || is_ascii_whitespace()`, so `\t`,`\r`,`\x0c`,`\x0b` are sent verbatim rather than rendered. Fix: align the two predicates on a single LC_CTYPE-driven space/print definition.
- [ ] **#WR8 — Header banner uses locale-insensitive date.** `write.rs:294-297,122-126` use `%Y-%m-%d %H:%M:%S` ignoring LC_TIME. Spec format is loosely specified; no action required beyond noting.
- [ ] **#WR9 — `terminal` operand not validated; bare name blindly prefixed `/dev/`.** `write.rs:270-279`: `../../etc/passwd` → `/dev/../../etc/passwd`. Low severity (perms gate it). Fix: require the operand be under `/dev` and a character device.
- [ ] **#WR10 — caret/printable logic is ASCII-only; ignores LC_CTYPE for multibyte.** `write.rs:199-230`. Non-ASCII bytes pass through unconditionally (reasonable for UTF-8 but not an LC_CTYPE classification). Minor.

### Detailed conformance matrix

#### Options
- [x] N/A — spec OPTIONS "None"; no flags defined. write.rs:32-40. CONFORMS.

#### Operands / STDIN
- [x] `user_name` required — clap enforces, missing → exit 2 (`test_write_no_args`). CONFORMS (123075).
- [ ] optional `terminal` — write.rs:39,270. PARTIAL: no who-format validation (#WR9).
- [x] STDIN read as lines copied to recipient — write.rs:302-303. CONFORMS to "lines read from stdin" but DIVERGES on canonical control-char semantics (#WR2).
- [ ] Multiple-login selection — write.rs:44-81. PARTIAL: excludes console, may wrongly error (#WR6).

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
| SIGHUP | unhandled (default terminate) | PARTIAL/MISSING (#WR5) |
| SIGPIPE | unhandled; `.expect` panics on EPIPE | DIVERGES (#WR4/#WR5) |
| SIGQUIT | unhandled (default) | CONFORMS (default action permitted) |

#### STDOUT / STDERR
- [x] STDOUT informational message when recipient multiply-logged-in — write.rs:64-69, gated to >1 login. CONFORMS (123101-123103).
- [ ] STDERR diagnostics only — explicit diagnostics conform, but `.expect`/`.unwrap` panics emit backtraces. DIVERGES (#WR4).

#### Extended description / character rendering
- [ ] alert → recipient — handler exists (184,308-309) but routes to stdout. DIVERGES (#WR1, spec 123051).
- [ ] erase/kill via termios — write.rs:128-155,310-311 PARTIAL: only fires when the whole line is erase/kill chars (#WR2).
- [x] INTR/EOF → "EOT\n" and exit — write.rs:179-181,305-307,319-320. CONFORMS in spirit; whole-line `.all()` gating misses embedded chars (#WR2).
- [x] print/space → recipient — write.rs:199-202,312-313. CONFORMS for ASCII; LC_CTYPE not honored (#WR10).
- [x] other non-printable → `^X`/`^?` — write.rs:207-230. CONFORMS (123061-123062; tested). iexten N/A (impl-defined).

#### Exit status / consequences of errors
- [x] 0 success — EOF path / SIGINT exit(0). CONFORMS (123112).
- [x] >0 user not logged on / permission denied — write.rs:55,80,283. CONFORMS (123113; tested).
- [ ] CONSEQUENCES OF ERRORS: Default — DIVERGES via panic paths (#WR4, spec 123114-123115).

#### Sender identity
- [x] sender-login-id — `curuser::login_name()` write.rs:290 (note inherits the #LN1 `$USER` fallback). CONFORMS pragmatically.
- [x] sending-terminal — `curuser::tty()` write.rs:291. CONFORMS.

### Test coverage signal

The PTY suite is unusually thorough (real `posix_openpt`/`grantpt`/`unlockpt`/`ptsname` pairs, mutex-serialized): missing-arg exit 2, invalid user/terminal exit 1, header format, basic/multi-line copy + EOT + exit 0, SIGINT→EOT→exit 0, caret notation, alert passthrough, special/unicode chars. Not covered:
- [ ] The wrong-destination bug (#WR1) — `test_pty_alert_passthrough` only asserts BEL isn't rendered `^G`; it never checks BEL reaches the *recipient* PTY (it goes to the child's stdout, uncaptured, so the test passes vacuously).
- [ ] Embedded mid-line control chars (#WR2).
- [ ] Superuser / mesg-n permission (#WR3).
- [ ] SIGHUP/SIGPIPE / closed-recipient panic (#WR4/#WR5).
- [ ] Multiple-login selection / console-only (#WR6); tab/backspace rendering (#WR7).
- [ ] PTY tests are timing-based (fixed sleeps) and silently `return` if the binary is absent — they can pass without exercising anything.

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

#### Critical
- [ ] **#TK1 — SIGINT exits non-zero; spec mandates exit status 0.** `talk.rs:1853` (`process::exit(128 + signal_code)` → 130 for SIGINT). Spec 116815-116816: "When the talk utility receives a SIGINT signal, the utility shall terminate and exit with a zero status." Fix: in `handle_signals`, exit `0` for SIGINT; reserve `128+sig` only for signals taking the default action.
- [ ] **#TK2 — Terminal echo / peer window written to stderr, corrupting the UI.** `process_input_char` echoes via `eprint!` (`talk.rs:941,945,958,961`) and `handle_character` writes a per-keystroke `eprintln!("{}", output_buffer.len())` (`talk.rs:843`). STDERR section is "None"; screen regions go to the terminal. Fix: route all rendering to the controlling terminal; delete the debug `eprintln!` at 843.
- [ ] **#TK3 — Raw mode set in the wrong scope and never restored on most exit paths.** `tcsetattr` raw mode is applied inside the reader thread only (`talk.rs:650`), guarded by a thread-local `RestoreTermOnDrop` (`talk.rs:667`). The pre-connection phase, the SIGINT handler, and `handle_connection_close` run without restoring termios. Fix: set raw mode once in the main flow behind a guard whose Drop/signal path always restores; restore before every `process::exit`.

#### Major
- [ ] **#TK4 — SIGINT teardown only deletes invitations in network mode, never in `--local`.** `DELETE_INVITATIONS` is populated only in `handle_new_invitation` (`talk.rs:1069`); `talk_local` never sets it. Ctrl-C while ringing in local mode leaves stale invitations in talkd. Fix: populate the teardown state in `talk_local`.
- [ ] **#TK5 — `mesg`/permission denial not surfaced; PermissionDenied & NotHere answers ignored.** The daemon's `Answer::PermissionDenied`/`NotHere`/`BadVersion` are decoded (`talk.rs:118-134`) but only `Answer::Success` is ever tested (`421,503`); all others fall through to "wait/ring" indefinitely. Fix: match the full `Answer` enum after each request; diagnostic + non-zero exit on denial/not-here.
- [ ] **#TK6 — stdout-not-a-terminal not checked.** Only stdin is validated (`check_if_tty`, `talk.rs:548-555`). Spec 116820: "If standard output is not a terminal, talk shall exit with a non-zero status." Fix: also test `io::stdout().is_terminal()`.
- [ ] **#TK7 — Required character-processing rules unimplemented.** No `<alert>` forwarding, no Ctrl-L (`\x0c`) refresh, no kill/werase, no LC_CTYPE print/space filtering; non-printables pass through as `input_char as u8` (`talk.rs:971`). grep for `\x0c`/`iexten`/`VKILL`/`alert` → nothing. Fix: implement the DESCRIPTION bullets; honor erase/kill from termios.
- [ ] **#TK8 — Mutual-termination semantics absent.** On peer close this side calls `handle_connection_close` → `process::exit(130)` (`talk.rs:1910-1915`), and local EOF just breaks the loop (`890`) with no peer notification. Fix: on local EOF notify the peer; on peer close keep a "peer gone, you may only exit" state rather than killing with 130.

#### Minor
- [ ] **#TK9 — SIGWINCH not handled (no resize).** Registered set is `SIGINT,SIGQUIT,SIGPIPE` (`talk.rs:1820`); terminal size read once (`400`). Fix: trap SIGWINCH, re-query `TIOCGWINSZ`, redraw.
- [ ] **#TK10 — Non-UTF-8 peer bytes dropped.** Reader does `from_utf8(&buffer[..nbytes])` and `continue`s on error (`talk.rs:692-698`), discarding the whole datagram. Fix: process bytes individually.
- [ ] **#TK11 — IPv6 path panics.** `Osockaddr::from(&SocketAddrV6)` is `unimplemented!()` (`talk.rs:270`). Latent panic. Fix: return an error.
- [ ] **#TK12 — `string_to_c_string`/`tty_to_c_string` panic on interior NUL.** `CString::new(...).expect(...)` (`talk.rs:1217,1230`). Fix: handle the error.
- [ ] **#TK13 — Diagnostics not internationalized.** `gettext` used only for clap help/about (3 sites, 41-46), never for the many `eprintln!` diagnostics. Fix: wrap user-facing diagnostics.
- [ ] **#TK14 — Long-line handling / buffer mismatch.** stdin reads one byte at a time (`878`) while the peer reader uses a 128-byte buffer; line-buffer overflow drops the oldest char (`848`) rather than respecting terminal width. Cosmetic.

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
- [ ] **#TD1 — `ANNOUNCE` never writes to the recipient's terminal; the daemon's core function is a stub.** `users/talkd.rs:315-341` (`handle_announce`). The comment admits "in a real daemon this would notify the callee… we just store the invitation." A real `talkd` must locate the callee's tty, check write permission, and write the "Message from…/respond with: talk…" banner. As written, an announced party is never told to respond, so the POSIX `talk` handshake can never complete against a fresh peer. Fix: on `ANNOUNCE`, resolve callee via `getpwnam`/utmp, find their tty, honor group-write/`mesg`, write the banner to the tty.
- [ ] **#TD2 — No validation that the announce target user exists or is logged in.** `users/talkd.rs:315-341`. The callee name is taken verbatim from the untrusted datagram and stored; the daemon never calls `getpwnam` or scans utmp, so it cannot return `Answer::NotHere` for an unknown/absent user (the code at `:91` only produces `NotHere` on a `LOOK_UP` miss). Fix: validate the callee against the passwd DB and login records before accepting an announce.
- [ ] **#TD3 — `mesg n` is never honored.** grep confirms no `mesg`/tty-mode/group-write check anywhere. POSIX `talk` (talk.md DESCRIPTION) makes recipient consent mandatory; the server must enforce it because the client cannot. Fix: stat the callee's tty and return `Answer::PermissionDenied` when not group-writable / `mesg` off.

#### Major
- [ ] **#TD4 — Unbounded invitation table = trivial DoS.** `users/talkd.rs:242-288,315-377`. `InvitationRegistry::insert` has no cap; any client can issue unlimited unauthenticated `ANNOUNCE`/`LEAVE_INVITE` datagrams to grow the `HashMap` without limit. `cleanup_expired` (`:255`) only evicts after the 60 s timeout and only runs when the *next* packet arrives, so a fast sender outpaces expiry. Fix: cap total (and/or per-caller) invitations, reject with `Answer::Failed` when full, run a time-based sweep.
- [ ] **#TD5 — Socket created with default (world-accessible) permissions; no ownership/path safety.** `users/talkd.rs:397-403`. `daemon_loop` blindly `fs::remove_file`s any pre-existing `/var/run/talkd.sock` then `UnixDatagram::bind`s, with no `umask`/`fchmod` to restrict the mode and no `O_NOFOLLOW`-style check that the path isn't an attacker-planted symlink. The `crond` lens (mode-exact, no-symlink, owner-checked) is absent. Fix: set a restrictive umask before bind (or `chmod` the socket); refuse to unlink/bind through a symlinked or non-socket path.
- [ ] **#TD6 — No privilege model.** grep confirms no `setuid`/`setgid`/`getuid`/`initgroups`/`setsid`. A daemon that must write to arbitrary users' ttys (per #TD1) needs root and must contain that privilege carefully; this one neither acquires nor drops privilege. Fix: define and document the privilege model.
- [ ] **#TD7 — Not a network daemon: no UDP/port-518 binding, breaks interop.** `users/talkd.rs:403` binds only a `UnixDatagram`; the companion `talk.rs` resolves the real `ntalk`/udp service for its non-`--local` path (`talk.rs:1262,1288`), so stock `talk` and any BSD client cannot reach this daemon. Fix: either implement the UDP `ntalk` listener, or rename/scope it explicitly as a local test daemon in user-facing docs.

#### Minor
- [ ] **#TD8 — `static mut SOCKET_PATH` read/written in a signal handler is unsound.** `users/talkd.rs:482-510`. The handler dereferences a `static mut` (a data race under current Rust rules) and calls `fs::remove_file`. Fix: store the path in an `AtomicPtr`/`OnceLock`, or `unlink` a fixed C-string path.
- [ ] **#TD9 — `--foreground` flag is dead.** `users/talkd.rs:53-56`. Parsed but never read (grep shows no use); `main` always runs in the foreground. Fix: implement fork/`setsid` daemonization gated on `!foreground`, or remove the flag.
- [ ] **#TD10 — Diagnostics go to stderr, not syslog.** `users/talkd.rs:407,414,426,472,526`. A persistent daemon should log via `syslog`. Operational strings also aren't `gettext`-wrapped (only the clap `about` at `:47` is). Fix: route through syslog.
- [ ] **#TD11 — `LEAVE_INVITE` dedup keys only on (callee, caller), ignoring tty/addr.** `users/talkd.rs:343-356`. A second invite from the same pair returns the *stale* invitation's id and drops the new `addr`, so a later `LOOK_UP` hands the callee a dead address. Fix: refresh the stored `tcp_addr`/`timestamp` on a matching `LEAVE_INVITE`.
- [ ] **#TD12 — No tests for the wire/loop path.** The unit tests never touch `CtlMsg::from_bytes`, the datagram loop, version/type rejection, or expiry. Fix: add datagram round-trip and malformed-packet tests.

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
- [ ] Attacker-controllable tty path — N/A today (never writes a tty) — but `r_tty` is taken untrusted (:165) and would feed any future tty-write path unchecked.

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
