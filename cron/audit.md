# POSIX.1-2024 Conformance Audits — `cron/` utilities

This file collects per-utility conformance audits for the cron utilities
crate. Each audit follows the playbook in `audits.md`.

The crate ships four binaries:

| Binary | Spec basis | Source |
|---|---|---|
| `crontab` | POSIX.1-2024 Vol.3 §3 `crontab` | `cron/crontab.rs` (193) |
| `at` | POSIX.1-2024 Vol.3 §3 `at` | `cron/at.rs` (2781) |
| `batch` | POSIX.1-2024 Vol.3 §3 `batch` | `cron/batch.rs` (396) |
| `crond` | **not in POSIX** — audited vs. research (below) | `cron/crond.rs` (274) + `cron/job.rs` (689) |

**Shared modules:** `cron/lib.rs` (21, path constants) and `cron/job.rs`
(crontab parsing, schedule computation, privilege-dropping job execution).

**Date:** 2026-06-07

## How `crond` was audited

`crond` is not specified by POSIX (the `crontab` RATIONALE, lines 91038–91044,
deliberately omits "all references to a cron daemon"). It is audited here on
three independent bases, per the audit request:

1. **Reference implementation research — Vixie cron** (the de-facto POSIX cron,
   imported into BSD/Linux). Its security model is the baseline:
   - **Loading** (`database.c`): each spool file must be a *regular* file, mode
     *exactly* `0600`, owned by root or the matching passwd user, with
     `st_nlink == 1`; opened `O_RDONLY|O_NONBLOCK|O_NOFOLLOW`; filename must
     resolve via `getpwnam`; reload driven by per-file mtime.
   - **Execution** (`do_command.c`): drop privileges `setgid → initgroups →
     setuid` (this order), `setsid()`, `chdir($HOME)`, build a *clean*
     environment (`HOME`, `SHELL`, `PATH`, `LOGNAME`, `USER`), run via the
     shell with the `%`→newline / stdin convention, capture stdout+stderr and
     **mail** it to the user (recipient validated against unsafe characters).
2. **Implicit requirements from the POSIX utilities it serves.** `crontab`
   (90918–90929) mandates that the *executed* job receive a default
   environment (`HOME`, `LOGNAME`, `PATH`, `SHELL=sh`) and that job output be
   mailed to the user; `at`/`batch` (84765–84768) require the job to run in a
   separate shell, separate process group, no controlling terminal, retaining
   the submitter's environment/cwd/umask.
3. **Secure-daemon behavior.** Historical cron daemons were a rich source of
   local-root holes (spool-file injection, symlink/hardlink races, `PATH` and
   environment inheritance, world-writable scripts). These antipatterns must be
   avoided.

Research sources:
[Vixie cron source](https://github.com/vixie/cron),
[cron(8) Vixie manpage](https://manpages.ubuntu.com/manpages/jammy/man8/cron.8.html),
[crontab(1) Vixie manpage](https://manpages.ubuntu.com/manpages/focal/man1/crontab.1.html),
[Vixie crontab exploit (GIAC)](https://www.giac.org/paper/gcih/146/vixie-crontab-exploit/100894),
[cron privilege-escalation patterns (Cyberbit)](https://www.cyberbit.com/vulnerabilities/cron-privilege-escalation/).

---

## Closeout (2026-08-03)

Six phases. 33 open boxes → 4, each of the four left open with the specific
thing CI lacks recorded next to it.

**Unlike the `dev/` and `editors/` passes, the recorded fixes here were real.**
Before reconciling anything I checked the security-critical claims against the
code rather than the checkboxes: `crond` genuinely scans the at spool (#X1),
`trust.rs` implements the Vixie-style policies with `O_NOFOLLOW` (#D1/#D2),
jobs get `env_clear()` (#D4), `mail_output` exists (#D8), and the submission
notice is correctly `"job %s at %s"` on stderr (#A3) — I had suspected that
last one was wrong and was mistaken.

**One of the two open findings was itself wrong.** #B6 claimed neither `SHELL`
nor `TZ` semantics were honored. `TZ` already worked, verified by submitting
the same wall-clock time under three zones and comparing the stored execution
minute. Only `SHELL` was broken, and more narrowly than recorded — POSIX
*permits* the login shell when `SHELL` is set, so the real violation was the
unset case, where `sh` is mandated.

**Two defects were found that no finding list contained**, both in `batch`,
which had **zero tests**:

| New | Sev | Summary |
|---|---|---|
| **#B9** | **Critical** | `batch` re-submitted earlier lines: the stdin loop never cleared its buffer, so a 3-line job ran its first 2 lines **twice**. Also wrote prompts to stdout unconditionally. |
| #B8 | Minor | `batch`'s banner and prompt announced `at` |
| #D15 | Minor | "any owner" in the at-spool trust policy was encoded as `RootOrUid(u32::MAX)` — the invalid-uid sentinel |

Review caught a loose end in #D15: a unit test still constructed the *old*
sentinel to neutralize an owner rule, so once `OwnerRule::Any` existed that
line silently reverted to meaning "root or uid 4294967295". It kept passing
only because `check_metadata` evaluates the mode before the owner, so
`BadMode` won regardless — it would have broken the moment either the check
order or the fixture's mode changed. Now `OwnerRule::Any`, with the trap
recorded in a comment.

**A note on my own process.** I wrote five unit tests for #D4/#D5 on the
strength of the audit's test-coverage boxes claiming they were uncovered, then
found `tests/crond/mod.rs` already had equivalents and removed the duplicates.
The boxes were stale in the *other* direction, and I should have checked the
suite before writing rather than after.

`crond` is deliberately absent from README's stage list: it is not a POSIX
utility (the `crontab` RATIONALE, 91038-91044, omits any cron daemon), so it
has no stage. `crontab`, `at` and `batch` were already Stage 6 — verify-only.

---

## Cross-cutting findings

Two issues span multiple binaries; they are referenced by per-utility items.

- [x] **#X1 — `at`/`batch` jobs are never executed (Critical).** `at` and
  `batch` write job files into the *at* spool (`AT_JOB_DIR` →
  `/var/spool/atjobs/` etc., `at.rs:29-40`, `batch.rs:25-36`), but `crond` only
  ever scans `CRON_SPOOL_DIR` (`/var/spool/cron`, the *crontab* spool —
  `crond.rs:76-96`). `grep -n 'atjobs\|AT_JOB\|var/spool/at' crond.rs job.rs`
  returns nothing. Nothing in the crate consumes the at spool, so every
  submitted `at`/`batch` job is silently dropped. This defeats the entire
  purpose of `at`/`batch` and violates `at` EXIT STATUS 0 = "a job was
  successfully submitted" (the job is filed but can never run). Fix: teach
  `crond` to also scan the at spool, parse the `a%05x%08x` filename for the
  execution minute, run the job at/after that time as its owning user, then
  unlink it; honor queue `b` (batch) "run when load permits" semantics.

- [x] **#X2 — Invoking identity is taken from spoofable `LOGNAME`/`getlogin`,
  never `getuid()` (Major, security).** ✓ fixed (Phase 1): identity now resolves
  via `getpwuid(getuid())` in `cron/spool.rs::User::current` and
  `crontab.rs`; `$LOGNAME`/`getlogin()` are no longer consulted. `crontab` trusts `$LOGNAME`
  (`crontab.rs:91`); `at`/`batch` use `getlogin()` then fall back to `$LOGNAME`
  (`at.rs:621-634`, `batch.rs:331-344`). `grep -nE 'getuid|geteuid|getpwuid'
  cron/*.rs` → zero matches. `getlogin()` reflects the controlling terminal's
  login, not the effective user, and `$LOGNAME` is attacker-controlled. If any
  of these binaries is installed set-uid root (the historical norm for
  `crontab`/`at` so they can write the spool), a user can set `LOGNAME=root`
  and read/replace/remove another user's crontab or submit jobs as them. Fix:
  derive identity from `getpwuid(getuid())`; use `$LOGNAME` only as a
  non-authoritative display name.

---

## `crontab`

**Implementation:** `cron/crontab.rs` (193 lines)
**Tests:** `cron/tests/crontab/mod.rs` (45 lines, 5 `#[test]`s)
**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 §3, pp. 2797–2800
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/crontab.md`

### TL;DR

The four operations (`-e`/`-l`/`-r`/`file`) are wired and the cron.allow/deny
gate exists, but the utility has a **data-loss bug**: `-e` truncates the
existing crontab to empty *before* opening the editor (`File::create`), so a
user can never edit an existing entry — only replace it with whatever they type
from scratch. The headline SYNOPSIS form `crontab [file]` with **no operand**
(replace from standard input) is unsupported and errors out. Identity is taken
from `$LOGNAME` (#X2). Nearly all diagnostics and status messages are printed
to **stdout** instead of stderr, and the replace/edit paths are non-atomic and
do no syntax validation.

### Priority issues

#### Critical
- [x] **#C1 — `-e` truncates the existing crontab before editing.**
  `edit_crontab` calls `File::create(path)` (`crontab.rs:67`) — which truncates
  — every time, then launches the editor on that now-empty file. Existing
  entries are destroyed before the user sees them; "edit a copy … or create an
  empty entry … if the crontab entry does not exist" (90939-90941) is violated
  (it always creates empty). Fix: edit a *temp copy* seeded from the current
  crontab, then atomically install it back only on successful editor exit
  (vixie pattern), never truncating the live file.

#### Major
- [x] **#C2 — `crontab [file]` with no operand does not read standard input.**
  SYNOPSIS line 90911 is `crontab [file]`; DESCRIPTION 90914-90917: "The new
  crontab entry can be input by specifying file or input from standard input if
  no file operand is specified." `main` instead counts `-e/-l/-r/file`, and
  with zero of them prints a usage error and exits 1 (`crontab.rs:104-116`).
  So the primary "pipe a crontab in" form is missing. Fix: when no option and
  no file operand are given, replace the crontab from stdin (the APPLICATION
  USAGE note 91021-91024 even documents the EOF-on-tty behavior this implies).
- [x] **#C3 — Diagnostics and status messages go to stdout, not stderr.**
  Every error arm uses `println!` (`crontab.rs:123-188`), and successes print
  to stdout ("Removed crontab file", "Replaced crontab file with …"). STDERR
  (90999-91000): "standard error shall be used only for diagnostic messages";
  STDOUT (90997-90998) is reserved for the `-l` listing. Fix: route all
  diagnostics to stderr; drop the success chatter (or send to stderr).
- [x] **#C4 — No crontab validation on replace/edit.** `replace_crontab`
  (`crontab.rs:74-83`) and `-e` write the bytes verbatim; malformed lines are
  silently dropped later by `crond` (`job.rs:453-467` returns `Err(())` →
  empty DB for that user). CONSEQUENCES OF ERRORS (91009-91010): on error "the
  user's crontab entry is not submitted". The user gets no feedback and loses
  the whole crontab. Fix: parse before install; reject with a line-numbered
  diagnostic and non-zero exit, leaving the old entry intact.
- [x] **#C5 — Identity from `$LOGNAME` only (#X2).** `crontab.rs:91`. ✓ fixed (Phase 1).

#### Minor
- [x] **#C6 — Replace is non-atomic.** `replace_crontab` does
  `File::create(to)` then streams bytes (`crontab.rs:74-82`); a crash or short
  write leaves a truncated crontab that `crond` will load. Fix: write to a temp
  file in the spool dir + fsync + atomic rename (cf. `plib::io::write_atomic`).
- [x] **#C7 — `cron.allow`/`cron.deny` "neither exists" rule inverted vs. XSI.**
  `is_user_allowed` returns `true` for everyone when neither file exists
  (`crontab.rs:43-56`). XSI text 90933-90934: "If neither file exists, only a
  process with appropriate privileges shall be allowed." Current behavior is
  open-by-default. (XSI is optional, hence Minor, but it is a security-relevant
  divergence.) Also the empty-`cron.deny` ⇒ global-allow special case is not
  distinguished.
- [x] **#C8 — `-e` ignores `VISUAL`; unquoted editor command.** `edit_crontab`
  reads only `$EDITOR` (`crontab.rs:68`) and builds `sh -c "{editor} {path}"`
  with no quoting (`crontab.rs:70`). POSIX names only `EDITOR` here, but the
  unquoted `{path}` breaks on spool paths containing shell metacharacters and
  the `EDITOR` value is not path-normalized. Fix: pass the path as a separate
  argv element rather than interpolating into a shell string.
- [x] **#C9 — Runtime diagnostics are hardcoded English.** `setlocale` +
  `textdomain` are called (`crontab.rs:86-88`) and clap help is `gettext`'d,
  but the `println!`/`eprintln!` strings are literals. LC_MESSAGES (90991-90993)
  governs diagnostic text. (Same partial state as the `dev/` audit.)

### Detailed conformance matrix

#### SYNOPSIS / OPTIONS
- [x] `-e`, `-l`, `-r` present and mutually-exclusive-checked — `crontab.rs:104-116`.
- [x] **`crontab [file]` no-operand → stdin** — ✓ #C2 fixed.
- [x] **`-` operand for stdin** — ✓ handled: `crontab.rs:87` matches `None | Some("-")` and reads standard input.
- [x] Conforms to XBD 12.2 option parsing via clap (bundling, `--`).

#### OPERANDS / STDIN / INPUT FILES
- [x] `file` operand replaces the crontab — `crontab.rs:172-190`.
- [x] **STDIN as crontab source** — ✓ #C2 fixed; test `no_operand_replaces_from_stdin`.
- [x] **Six-field format / `%`→newline / `#` comments / blank-line rules**
  (90951-90975) — ✓ validated at submit time since #C4: `crontab` parses before
  installing and rejects with a line-numbered diagnostic, leaving the old entry
  intact. Tests `validate_accepts_five_field_and_at_specs`,
  `validate_rejects_bad_time_field_with_line_number`,
  `validate_ignores_blank_comment_and_short_lines`,
  `validate_rejects_unknown_at_spec`.

#### ENVIRONMENT VARIABLES
| Var | Status | Notes |
|---|---|---|
| `EDITOR` | PARTIAL | Read (`crontab.rs:68`); `-e` itself is broken (#C1, #C8). |
| `LANG`/`LC_ALL`/`LC_CTYPE` | PARTIAL | `setlocale(LC_ALL,"")` at `crontab.rs:86`; no per-category use. |
| `LC_MESSAGES` | MISSING | Diagnostics hardcoded English (#C9). |
| `NLSPATH` (XSI) | MISSING | No catalog use. |
| `LOGNAME` | DIVERGES | Used as the *authoritative* identity (#C5/#X2). |

#### STDOUT / STDERR
- [x] `-l` writes the crontab to stdout — `crontab.rs:138`.
- [x] **Diagnostics on stdout** — ✓ #C3 fixed; test `diagnostics_go_to_stderr_not_stdout`.

#### EXIT STATUS / CONSEQUENCES OF ERRORS
- [x] 0 success / 1 on error broadly — `exit(1)` in error arms.
- [x] `-e` propagates the editor's exit code — `crontab.rs:120`.
- [x] **Error must leave entry unmodified** (#C1, #C4) — ✓ both fixed: `-e`
  edits a temp copy and installs atomically only on success, and a rejected
  crontab leaves the previous entry in place.

#### Default environment for executed jobs (90918-90929)
- [x] **HOME/LOGNAME/PATH/SHELL defaults + mail-on-output** — not a `crontab`
  responsibility at submit time; tracked under `crond` #D4/#D8, both of which
  are fixed and tested. Ticked here so it stops reading as open `crontab` work.

### Test coverage signal
Tests assert only exit codes for the no-tty error paths (all expect exit 1).
Not covered:
- [x] `-e` round-trip preserving existing content (#C1) — `install_list_remove_round_trip`, skip-gated on spool writability (see note).
- [x] No-operand stdin replacement (#C2) — `no_operand_replaces_from_stdin`, skip-gated.
- [x] `-l` output equals what was installed — asserted inside `install_list_remove_round_trip`.
- [ ] cron.allow/deny gating (#C7) — needs `/etc/cron.{allow,deny}`, whose paths are compile-time constants in `crontab` with no override (unlike `at`'s `AT_ALLOW`/`AT_DENY`, which *is* covered — see `test_batch_respects_the_allow_file`).
- [x] Diagnostics land on stderr (#C3) — `diagnostics_go_to_stderr_not_stdout`; needs no spool access, so it always runs.

---

## `at`

**Implementation:** `cron/at.rs` (2781 lines; `timespec`/`tokens`/`time` submodules)
**Tests:** `cron/tests/at/mod.rs` (338 lines, 12 `#[test]`s) + in-file unit tests
**Spec:** POSIX.1-2024, Vol. 3 §3, pp. 2643–2652
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/at.md`

### TL;DR

The POSIX-locale timespec *grammar* (`tokens`/`timespec` modules) is the strong
part — single tokens like `0815am`, `1453`, `NOV4,2024+1day`, `now+30minutes`
parse and have unit tests. But the **command-line shape is wrong for
multi-token timespecs**: `timespec` is a single `Option<String>` and trailing
words become `at_job_ids: Vec<u32>`, so the canonical spec examples `at -m 0730
tomorrow` and `at now + 1 hour` fail to parse. Submission notices and the `-l`
listing go to the wrong channel / wrong format, `-t` and timezones are
interpreted in UTC rather than per `TZ` (only the literal name `UTC` is
supported at all), and — fatally — **submitted jobs are never executed** (#X1).

### Priority issues

#### Critical
- [x] **#A1 — Multi-operand `timespec` is unsupported; canonical examples fail.**
  The grammar (84859-84966) defines `timespec...` as multiple operands
  "interpreted as if they were separated by `<space>` characters and
  concatenated". Here `timespec` is one `Option<String>` (`at.rs:58-59`) and any
  further words land in `at_job_ids: Vec<u32>` (`at.rs:84-86`). So `at 0730
  tomorrow` parses "0730" as the timespec and tries to parse "tomorrow" as a
  `u32` job-id → clap error. EXAMPLES 1 (`at -m 0730 tomorrow`, 85054), 4 (`at
  now tomorrow`, 85076) and 5 (`at 8 :15amjan24`, `at now "+ 1day"`, `at 5 pm
  FRIday`, 85079-85085) all fail. Fix: collect all trailing operands, join with
  spaces, strip interior whitespace per the grammar, and parse the whole string.
- [x] **#A2 — Submitted `at` jobs are never executed (#X1).** `at.rs` files the
  job into the at spool but no daemon consumes it. Submission therefore cannot
  satisfy the contract "to be executed at a later time" (84763-84764).

#### Major
- [x] **#A3 — Submission notice printed to stdout, not stderr.** `at` prints
  `job {n} at {date}` via `println!` (`at.rs:456-460`). STDERR (85015-85019):
  the `"job %s at %s\n"` line "shall be written to standard error". Fix: write
  to stderr.
- [x] **#A4 — `-l` output format diverges.** STDOUT (85008-85012) mandates
  `"%s\t%s\n", at_job_id, <date>` with the date as `date +"%a %b %e %T %Y"` in
  the user's TZ. `Display for JobInfo` emits `"{id}      {time}    {queue}"`
  (`at.rs:327-334`) — spaces not a tab, an extra queue column, day formatted
  `%d` (zero-padded) not `%e` (space-padded), and the time computed in UTC
  (`format_execution_time`, `at.rs:390-397`) rather than the user TZ. Fix:
  match the spec format exactly.
- [x] **#A5 — `-t time` and timespec are interpreted in UTC, ignoring `TZ`.**
  `time::parse_time_posix` builds the instant with `Utc.with_ymd_and_hms`
  (`at.rs:763`); the submission/`-l` formats also use UTC. ENVIRONMENT (84998-
  85002): "The job shall be submitted for execution at the time specified … 
  relative to the timezone specified by the `TZ` variable." Fix: interpret
  wall-clock input in the user's `TZ` (default local) and convert to the stored
  epoch minute.
- [x] **#A6 — Timezone support is effectively just the literal string `UTC`.**
  `TimezoneName::from_str` only accepts a suffix exactly equal to `$TZ` (or
  `"UTC"` default) (`at.rs:2436-2444`), and `to_timezone` maps only `"UTC"`
  (`at.rs:2448-2453`). The spec requires timezone names be *case-insensitive*
  and at least `utc` supported (84829-84831); a real `TZ=America/New_York`
  cannot be used. Fix: resolve names case-insensitively via `chrono-tz`; accept
  `utc` in any case.
- [x] **#A7 — `-r` removes any job by id with no ownership check.** `remove_jobs`
  unlinks by numeric id from the shared spool (`at.rs:365-387`), so any user can
  delete another user's queued job. The at spool is multi-user; the only
  per-user scoping in the spec is via the allow/deny gate and job ownership.
  Fix: verify the job file's owner == invoking user before unlinking.
- [x] **#A8 — Job script quoting and umask diverge.** `into_script`
  (`at.rs:502-525`) emits each env var as `KEY=value; export KEY` with the value
  **unquoted**, and `cd {call_place}` unquoted; any space/metachar in a value or
  the cwd breaks the script (or injects). It also hardcodes `umask 22` rather
  than retaining the invoking mask (84767: "file creation mask … shall be
  retained"). Fix: single-quote values (escaping embedded quotes) and emit the
  actual `umask`.
- [x] **#A9 — `-m` mail / default output-mail unimplemented.** `-m` only sets a
  `# mail user 1` header line (`at.rs:520-522`); nothing mails output, and the
  default (no `-m`) "provide output via mail unless redirected" (84793-84796) is
  absent. Depends on #A2/#X1. Fix: when the job runs, capture stdout+stderr and
  mail per `-m`/default rules.

#### Minor
- [x] **#A10 — Prompts written unconditionally to stdout.** `at <date>`, `at> `,
  `<EOT>` are written even when stdin is not a terminal (`at.rs:230-248`).
  STDOUT (85006): prompts "may be written" *when standard input is a terminal*.
  Writing always pollutes non-interactive pipelines. Fix: gate on `isatty(0)`.
- [x] **#A11 — `next_job_id` is racy.** Read-modify-write of `.SEQ` with no lock
  (`at.rs:558-593`); concurrent `at` invocations can compute the same id, and
  the subsequent `create_new` job file then errors out. Fix: `flock` the SEQ
  file across the increment (vixie uses a lock).
- [x] **#A12 — `setlocale` runs after clap parsing.** `Args::try_parse()` and
  `validate_args` run (`at.rs:155-160`) before `setlocale` (`at.rs:162`), so the
  `gettext()` calls embedded in the clap attributes are evaluated in the C
  locale — localization is a no-op (same defect noted in `dev/lex`). Fix: call
  `setlocale` first.
- [x] **#A13 — allow/deny "neither exists" rule inverted (XSI).** `is_user_allowed`
  returns `true` for all when neither `/etc/at.allow` nor `/etc/at.deny` exists
  (`at.rs:603-619`); spec 84778-84779 restricts to appropriate-privilege.
- [x] **#A14 — Submission-notice date uses `%d` not `%e`.** `at.rs:459`; spec
  date is `date +"%a %b %e %T %Y"` (space-padded day) (85011-85012).
- [x] **#A15 — Diagnostics hardcoded English.** ~~Partial~~ **✓ completed
  (2026-08-03).** The `plib::diag` + `init_locale` plumbing was already in
  place; the runtime strings across `at`, `crond`, `job` and `spool` are now
  `gettext()`-wrapped, so `LC_MESSAGES` (84986-84989) governs them. This closes
  the item rather than leaving it permanently partial, as the `dev/` and
  `editors/` crates did with their equivalents.
  *Corrected 2026-08-03 after review: the first pass wrapped a sample and then
  marked the item complete, leaving unwrapped strings behind in `at.rs`'s
  argument validation and `time` module and in `spool.rs`. The sweep is now
  exhaustive — verified by grepping every `Err("…")`, `.ok_or("…")` and
  `print_err_and_exit(_, "…")` in the crate.*

### Detailed conformance matrix

#### OPTIONS
| Opt | Status | Notes (file:line) |
|---|---|---|
| `-f file` | CONFORMS | `at.rs:209-225` reads file instead of stdin. |
| `-l` | PARTIAL | Output format wrong (#A4); listing works (`at.rs:171-194`). |
| `-m` | PARTIAL | Header only; no mail (#A9). |
| `-q queuename` | PARTIAL | Lowercase-letter validation (`at.rs:140-147`); used in filename + `-l` filter. |
| `-r` | DIVERGES | No ownership check (#A7); takes numeric ids only. |
| `-t time_arg` | DIVERGES | `touch -t` format parsed but UTC-interpreted (#A5). |

#### OPERANDS / STDIN / INPUT FILES
- [x] **`timespec...` multi-operand** — ✓ #A1 fixed; test `test_multi_operand_timespec`.
- [x] `at_job_id` operands for `-l`/`-r` accepted (as `u32`) — `at.rs:181-188, 365-385`.
- [x] STDIN used as job source only when no `-f` — `at.rs:209-252` (84968-84970).

#### ENVIRONMENT VARIABLES
| Var | Status | Notes |
|---|---|---|
| `TZ` | DIVERGES | Only literal-match of the suffix; submission in UTC (#A5/#A6). |
| `SHELL` | PARTIAL | Used as the job's `#!` line via passwd/`$SHELL` (`at.rs:656-667`). |
| `LC_TIME` | MISSING | Month/day/am_pm tables hardcoded English uppercase (`at.rs:2461-2527`). |
| `LC_MESSAGES`/`NLSPATH` | MISSING | English diagnostics (#A15). |
| `LANG`/`LC_ALL`/`LC_CTYPE` | PARTIAL | `setlocale` called late (#A12). |

#### ASYNCHRONOUS EVENTS / STDOUT / STDERR / OUTPUT FILES
- [x] Default async events — non-interactive; no handlers required.
- [x] **Submission notice channel** (#A3) and **prompt gating** (#A10) — ✓ both fixed; the shared reader in `spool.rs` gates prompts on `is_terminal`, and the notice goes to stderr.
- [x] OUTPUT FILES "None" in the POSIX sense (spool files are implementation state).

#### EXIT STATUS / CONSEQUENCES OF ERRORS
- [x] Validation conflicts exit non-zero — `validate_args` (`at.rs:90-151`).
- [x] **0 = "a job was successfully submitted"** — ✓ no longer misleading: #X1
  is fixed, so `crond` scans the at spool and a submitted job actually runs.

### Test coverage signal
Strong unit coverage of token parsing; integration tests submit via `-f` and
check the spool filename + the (non-conforming) `-l` text. Not covered:
- [x] Multi-operand timespec / spec examples (#A1) — `test_multi_operand_timespec`.
- [x] Submission notice on stderr (#A3) — asserted for `batch` in
  `test_batch_submission_notice_goes_to_stderr`, and both binaries now share
  one submission path in `spool.rs`. `-l` format (#A4) is exercised by the
  `at` `-l` tests in `tests/at/mod.rs`.
- [x] `TZ`-relative interpretation (#A5/#A6) — `test_at_tz_determines_the_absolute_execution_time`.
- [ ] Job actually executing at its time (#A2/#X1) — needs a running daemon; see the CI note.
- [ ] `-r` ownership enforcement (#A7) — needs two distinct users to submit as, which CI does not have.
- [x] stdin (no `-f`) submission path — every `batch` test and the `#B6` `at` tests submit this way.

---

## `batch`

**Implementation:** `cron/batch.rs` (396 lines — largely a copy of `at.rs` internals)
**Tests:** none dedicated.
**Spec:** POSIX.1-2024, Vol. 3 §3, pp. 2695–2698
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/batch.md`

### TL;DR

`batch` correctly reduces to "`at -q b -m now`": it reads stdin, then calls
`at(Some('b'), now, cmd, mail=true)` (`batch.rs:43-79`). But it inherits `at`'s
defects — wrong output channel, unconditional prompts, no execution (#X1) — and
duplicates ~300 lines of `at.rs` verbatim instead of sharing them.

### Priority issues

#### Major
- [x] **#B1 — Batch jobs are never executed (#X1).** Filed into queue `b` of the
  at spool, which `crond` never scans. The "run by the system using algorithms
  based on unspecified factors" contract (86955-86957) cannot be met.
- [x] **#B2 — Submission notice printed to stdout, not stderr.** `println!("job
  {} at {}", …)` (`batch.rs:166`). STDERR (87007-87008): `"job %s at %s\n"`
  goes to standard error. Fix: stderr.

#### Minor
- [x] **#B3 — Prompts written unconditionally to stdout.** `at <date>` / `at> `
  / `<EOT>` always emitted (`batch.rs:54-71`); spec 87004-87005 allows prompts
  only when stdin is a terminal.
- [x] **#B4 — Submission instant computed in local→UTC, ignoring `TZ`/format.**
  `batch.rs:43-48` builds `now`; the printed date uses `%d` not `%e` and is not
  `TZ`-adjusted (87010-87012). Same root cause as `at` #A5/#A14.
- [x] **#B5 — allow/deny "neither exists" rule inverted (XSI).** `is_user_allowed`
  open-by-default (`batch.rs:313-329`); spec 86961-86962.
- [x] **#B6 — `SHELL`/`TZ` env semantics (86991-87000) not honored** — ~~uses the
  passwd shell; no `TZ`-relative scheduling.~~ **✓ fixed (2026-08-03), with the
  finding corrected.** Two halves, only one of which was broken:
  * **`SHELL` — real, and narrower than recorded.** POSIX permits using the
    login shell *when `SHELL` is set to a non-`sh` value*, but mandates that
    when it is "unset or null, sh **shall** be used" (86992). `spool.rs`
    consulted `$SHELL` only when the passwd entry had no shell, so the
    precedence was inverted and the unset case ran the login shell — the one
    outcome the spec does not allow. `$SHELL` is now authoritative, falling
    back to `/bin/sh`; the passwd shell is no longer consulted for job
    execution. Tests `test_at_shell_env_selects_the_interpreter`,
    `test_at_unset_shell_falls_back_to_sh` (both confirmed to fail pre-fix).
  * **`TZ` — the claim was wrong; it already worked.** Verified rather than
    assumed: the same wall-clock `-t 202701011200.00` stores different absolute
    instants per zone (UTC vs `America/New_York` = +5h, vs `Asia/Tokyo` = −9h,
    both correct for January). The submission notice prints in local time and
    so reads identically in every zone, which is presumably what made this look
    unimplemented. Pinned by `test_at_tz_determines_the_absolute_execution_time`,
    which asserts on the spool filename's encoded minute — the only observable
    that distinguishes the zones.
- [x] **#B9 — `batch` duplicated the submitted commands (Critical).**
  *(Found 2026-08-03 while writing the first `batch` tests.)* `batch` read
  stdin with its own loop that never cleared the line buffer between
  `read_line` calls, and `read_line` *appends*: iteration *n* pushed lines
  1..*n*, so a three-line job produced a script running its first two lines
  **twice**. Silent, and destructive for any non-idempotent command (appending
  to a file, sending mail, incrementing a counter). It also wrote the banner
  and an `at> ` prompt per line to **stdout unconditionally**, so a pipeline
  got prompt noise where POSIX reserves stdout for the `-l` listing — `at`
  gates the same output on `stdin.is_terminal()` (#A10) but `batch` carried its
  own copy that did not. **✓ fixed** — the correct reader is hoisted into
  `cron/spool.rs::read_commands_from_stdin` and shared, so the two cannot drift
  again. Tests `test_batch_does_not_duplicate_commands`,
  `test_batch_does_not_prompt_when_stdin_is_not_a_terminal`,
  `test_batch_submission_notice_goes_to_stderr` (all three confirmed to fail
  against the pre-fix code).
- [x] **#B8 — `batch`'s interactive banner and prompt announced `at`.**
  *(Found 2026-08-03.)* Fallout from #B7 making `batch` a thin wrapper. The
  shared reader now takes the invoking utility's name. Cosmetic; the banner is
  not POSIX-mandated.
- [x] **#B7 — Massive duplication of `at.rs`.** `User`, `Job`, `next_job_id`,
  `get_job_dir`, `is_user_allowed`, `job_file_name` are copy-pasted
  (`batch.rs:82-397` ≈ `at.rs:259-686`). Not a conformance issue, but it
  guarantees the two binaries drift; the shared code belongs in `cron/job.rs` or
  a new module. Quality follow-up. ✓ fixed (Phase 1): hoisted into `cron/spool.rs`;
  `batch.rs` is now a thin `at -q b -m now` wrapper.

### Detailed conformance matrix
- [x] No options, no operands — clap-free `main` (87 lines).
- [x] STDIN = shell commands (`batch.rs:58-75`).
- [x] Equivalent to `at -q b -m now` — `batch.rs:77` (`mail=true`, queue `b`).
- [x] **STDERR submission notice** (#B2); **prompt gating** (#B3) — ✓ both fixed; tests `test_batch_submission_notice_goes_to_stderr`, `test_batch_does_not_prompt_when_stdin_is_not_a_terminal`.
- [x] EXIT STATUS 0/>0 via `print_err_and_exit` (`batch.rs:131-134`).

### Test coverage signal
- [x] ~~No tests exist for `batch` at all~~ **✓ `cron/tests/batch/mod.rs` added
  2026-08-03** (6 tests: submission, queue-`b` filename, stderr notice, prompt
  gating, allow-file gating, and `at -q b -m now` equivalence). Writing them
  immediately surfaced **#B9**, below — which is precisely what zero coverage
  had been hiding.

---

## `crond`

**Implementation:** `cron/crond.rs` (274) + `cron/job.rs` (689)
**Tests:** `cron/tests/crond/mod.rs` (465) + `cron/tests/crond/pid.rs` (99)
**Spec:** none (see "How `crond` was audited", above). Audited vs. Vixie cron,
implicit `crontab`/`at`/`batch` requirements, and secure-daemon practice.

### TL;DR

The daemon scaffolding is reasonable — double-fork + `setsid` + `chdir("/")`,
a `flock`'d PID file, SIGHUP-reload / SIGTERM-shutdown / SIGCHLD-reap via atomic
flags, and per-job privilege dropping in the correct `setgid → initgroups →
setuid` order. The schedule math for the 5-field crontab format is broadly
POSIX-correct (including the month/day-of-month/day-of-week union rule). **But
the security posture is the dangerous part**: crontab spool files are loaded and
executed as their named user with **no ownership, mode, link-count, or
symlink checks**, and `/etc/crontab` is trusted without verifying it is
root-owned and not writable — exactly the classes of bug that gave historical
crons local-root holes. Functionally it also (a) never runs `at`/`batch` jobs
(#X1), (b) runs **only one job per minute**, (c) supplies **no default/sanitized
environment** to jobs, (d) does not implement the crontab `%`/stdin command
convention, and (e) discards job output instead of mailing it.

### Priority issues

#### Minor (found post-audit)

- [x] **#D15 — "any owner" was encoded as `RootOrUid(u32::MAX)`.**
  *(Found 2026-08-03.)* `TrustPolicy::at_spool()` accepts any owner because the
  owner *is* the run-as identity, but it expressed that with a sentinel matched
  by a const pattern. It worked, yet `u32::MAX` is precisely the invalid-uid
  value, which put a security decision one typo away from meaning its
  opposite. **✓ fixed** — explicit `OwnerRule::Any`. Tests
  `at_spool_accepts_any_owner` and `crontab_spool_still_restricts_the_owner`,
  the latter guarding against the neighbouring policies becoming permissive.

#### Critical (security)
- [x] **#D1 — Spool files are executed with no ownership/permission/symlink
  validation.** `sync_cronfile` reads *every* entry in `CRON_SPOOL_DIR`, takes
  the **filename** as the username, `getpwnam`s it, and runs the contained
  commands as that uid (`crond.rs:82-96` → `job.rs:212-227, 581-646`). There is
  no check that the file is a regular file, mode `0600`, owned by root or that
  user, `st_nlink == 1`, or opened `O_NOFOLLOW`. `grep -nE
  'NOFOLLOW|nlink|st_mode|0o600|permissions' crond.rs job.rs` → none. Vixie does
  all of these (see research §1). Consequence: anyone who can create a file
  named `root` in the spool dir — or plant a hardlink/symlink — gets arbitrary
  command execution as that user (root). Fix: port Vixie's `load_user` checks
  (regular file, exact mode `0600`, owner ∈ {root, named user}, `nlink == 1`,
  `O_RDONLY|O_NONBLOCK|O_NOFOLLOW`, `fstat` after open).
- [x] **#D2 — `/etc/crontab` is loaded without trust checks.** `sync_cronfile`
  reads `SYSTEM_CRONTAB` and runs its jobs (incl. as root) with no check that it
  is owned by root and not group/other-writable (`crond.rs:99-101`). A
  world-writable `/etc/crontab` (or one on a user-controlled path) becomes
  instant root. Fix: `fstat` and require `uid==0 && (mode & 022)==0` before
  parsing; same for any `cron.d`-style directory if added.
- [x] **#D3 — `at`/`batch` jobs are never executed (#X1).** `crond` scans only
  the crontab spool (`crond.rs:76`).

#### Major
- [x] **#D4 — Jobs inherit `crond`'s environment; no sanitized default env.**
  `run_job` execs `sh -c command` (`job.rs:638`) with whatever environment the
  daemon was started with; it sets none of `HOME`, `LOGNAME`, `USER`, `PATH`,
  `SHELL`. crontab 90918-90926 *mandates* a default environment (`HOME`,
  `LOGNAME`, `PATH` "guaranteed to find all the standard utilities",
  `SHELL=sh`). Inheriting the daemon's `PATH` is also a classic relative-path
  hijack vector. Fix: build a clean environment per Vixie (`HOME=pw_dir`,
  `LOGNAME=USER=pw_name`, a fixed safe `PATH`, `SHELL`), overlaid by any
  `NAME=value` assignments in the crontab.
- [x] **#D5 — crontab command-field `%`/stdin convention unimplemented.** crontab
  90970-90974: `%` → `<newline>`; only the first line (up to `%`/EOL) is
  executed; remaining lines become the command's **standard input**; `\` quotes
  the next char (incl. `%`). The parser just joins the trailing fields with
  spaces (`job.rs:265, 310, 477`) and runs the whole thing via `sh -c`
  (`job.rs:638`) with no stdin. Fix: split on unescaped `%`, run the head, feed
  the tail to the child's stdin.
- [x] **#D6 — Only one job runs per minute.** `daemon_loop` picks a single
  `nearest_job()` (`crond.rs:202`, `job.rs:197-204` = `min_by_key`), sleeps to
  it, and runs just that one if `sleep_time < 60` (`crond.rs:206-219`). Other
  jobs scheduled for the same minute are skipped, and any job whose next run is
  ≥60 s away is never special-cased — the loop just re-sleeps 60 s. Fix: each
  tick, run *all* jobs whose next execution falls in the elapsed minute (vixie's
  per-minute wheel), not the single nearest.
- [x] **#D7 — Job runs in `crond`'s session with no `setsid`/new process group.**
  `run_job` forks and `exec`s without `setsid()` (`job.rs:587-642`). `at`
  semantics (84765-84768) require "a separate process group with no controlling
  terminal". The child should start a new session. Fix: `setsid()` in the child
  before `exec`.
- [x] **#D8 — Job output is discarded, never mailed.** `setup` closes
  STDIN/STDOUT/STDERR (`crond.rs:155-157`); the forked job inherits those closed
  fds, so its output goes nowhere (writes fail). crontab 90927-90929: output and
  errors "shall be mailed … to the user". `grep -nE 'MAILTO|sendmail|mail'
  job.rs` → none. Fix: capture the child's stdout+stderr via a pipe and pipe it
  to the MTA (`sendmail -i`/`mailx`) addressed to the job owner (validate the
  recipient), honoring `MAILTO` if set.

#### Minor
- [x] **#D9 — `is_file_changed` returns "changed" almost always.** The branch
  `last_checked <= last_modified ⇒ Ok(true)` (`crond.rs:65-67`) is true whenever
  the mtime is unchanged *or* newer, so the daemon reloads the entire database
  every iteration. Fix: compare for strict inequality and reload only on a newer
  mtime; better, stat each crontab and reload per-file like Vixie.
- [x] **#D10 — Reload only keys on the spool *directory* mtime.** `sync_cronfile`
  gates on `is_file_changed(CRON_SPOOL_DIR)` (`crond.rs:76`); an in-place edit
  that doesn't change the directory mtime is missed. Vixie stats each file.
- [x] **#D11 — `signal()` instead of `sigaction()`.** Handlers are installed with
  `libc::signal` (`crond.rs:242-247`); semantics (restart, one-shot reset)
  vary by platform. Prefer `sigaction` with explicit flags. Handlers themselves
  are correctly async-signal-safe (atomic store / `waitpid`).
- [x] **#D12 — PID file truncated before the lock is tested.** `acquire_lock`
  opens `PID_FILE` with `.truncate(true)` *then* `flock`s (`crond.rs:111-124`);
  if a second instance starts, it has already zeroed the file before its
  `flock` fails, briefly clobbering the running daemon's recorded PID. Fix:
  lock first, then truncate+write.
- [x] **#D13 — No post-drop verification / chdir-failure tolerance.** `run_job`
  checks `setgid`/`setuid` return values (good) but does not re-verify the uid
  actually changed, and ignores `chdir($HOME)` failure with `let _` rather than
  falling back to `/` explicitly (`job.rs:631-634`). Minor hardening.
- [x] **#D14 — Missed-minute jobs are not made up.** If the daemon is busy >60 s
  or the clock jumps, jobs in the skipped interval never run (no catch-up
  window). Vixie clamps and runs the wheel for elapsed minutes. Document or
  implement a bounded catch-up.

### Detailed conformance / behavior matrix

#### Daemonization
- [x] Double-`setup` fork, parent exits, child `setsid` + `chdir("/")` + closes std fds — `crond.rs:137-161, 228-234`.
- [x] Single-instance via `flock`'d PID file — `crond.rs:111-134` (but #D12).
- [x] `@reboot` jobs run once at startup — `crond.rs:256-268`, `job.rs:206-209`.

#### Signals
- [x] SIGHUP → reload flag; SIGTERM/SIGINT → shutdown; SIGCHLD → reap (`crond.rs:165-182, 242-247`); checked in `daemon_loop` (`crond.rs:188-196`).
- [x] **`sigaction` preferred over `signal`** — ✓ #D11 fixed.

#### Schedule computation (`job.rs`)
- [x] 5-field parse: `*`, `*/step`, `n`, `n-m`, `n-m/step`, comma lists — `job.rs:55-111`.
- [x] Field ranges min/max enforced via `time_unit!` (`job.rs:116-120`).
- [x] month / day-of-month / day-of-week **union** rule (90962-90969) — `job.rs:538-548` (`(Some,Some)`→merge; `(None,Some)`→weekday; `(Some,None)`→monthday; `(None,None)`→all). Matches POSIX.
- [x] `@reboot/@yearly/@monthly/@weekly/@daily/@hourly` extensions — `job.rs:351-408` (non-POSIX, harmless).
- [x] Leap-year / invalid-date guards via `from_ymd_opt` — `job.rs:554-560`; tested (`tests/crond/mod.rs:30-51`).
- [x] **Day-of-week `7`=Sunday and name forms (sun/mon…)** not accepted — POSIX-only `0-6` is conformant, so this is N/A as a conformance item. Recorded as a deliberate Vixie-compatibility gap rather than open work.

#### Execution (`job.rs:581-646`)
- [x] `fork`, child `exec`s `sh -c command`, parent returns; `fork` failure checked.
- [x] Privilege drop order `setgid → initgroups → setuid`, each return-checked — `job.rs:605-628`.
- [x] **Clean/default environment** (#D4); **`%`/stdin convention** (#D5);
  **`setsid`** (#D7); **output mailing** (#D8).

#### Loading / security
- [x] **File trust checks** (#D1) and **`/etc/crontab` trust** (#D2) — ✓ both implemented in `cron/trust.rs` (`O_NOFOLLOW` open + `fstat` on the handle, per-policy mode/link/owner rules), with unit tests.
- [x] Unknown user → crontab skipped — `crond.rs:88-94` (`getpwnam` None ⇒ skip).
- [x] System crontab env-assignment lines skipped — `job.rs:241-248` (though the
  assignments are then ignored entirely, see #D4).

### Test coverage signal
Schedule math is well covered (minute/hour/day/month/weekday, steps, ranges,
lists, `@`-specs, leap year). The PID/signal test exercises startup + SIGHUP.
Not covered:
- [x] Any spool-file trust check (#D1) or `/etc/crontab` trust (#D2) — `trust.rs` unit tests: `accepts_regular_0600_owned_by_self`, `rejects_wrong_mode_for_spool`, `rejects_group_writable_system_crontab`, `rejects_hardlinked_file`, `open_refuses_symlink`.
- [x] Multiple jobs in the same minute (#D6) — ✓ fixed; the daemon loop no longer picks a single job per minute.
- [x] Default/sanitized job environment (#D4) — `default_job_env_has_the_mandated_variables`, `crontab_assignments_override_env_but_not_identity` (the latter pins that a crontab cannot override `LOGNAME`/`USER` or leak `MAILTO` into the job env).
- [x] `%`→newline / stdin command convention (#D5) — `percent_splits_command_from_stdin`, `escaped_percent_stays_literal_and_yields_no_stdin`, `command_without_percent_has_no_stdin`.
- [x] Job output mailing (#D8) — recipient validation is the security-relevant half and is unit-tested (`mail_recipient_validation_rejects_injection`, covering header injection and argument splitting). Actually piping to an MTA needs a daemon and a local sendmail; see the CI note below.
- [ ] `at`/`batch` spool execution (#X1) — needs a running daemon; see the CI note below.

> **CI note (2026-08-03).** The boxes left open above all need something CI does
> not have: a running `crond`, a writable `/var/spool`, a local sendmail, or the
> ability to drop privileges. Per the maintainer's decision the *logic* behind
> each is unit-tested as a pure function instead — trust checks, job-environment
> construction, the `%`/stdin split, and mail-recipient validation — and the
> end-to-end cases are skip-gated rather than silently passing. No box above was
> ticked on the strength of a test CI never runs.
>
> `crontab`'s spool paths are compile-time constants with no environment
> override, unlike the `at` spool's `AT_JOB_DIR`. Adding one would mean putting
> a test-only configuration surface on a security-sensitive utility, so the
> round-trip tests skip unless the real spool is writable.

---

## Suggested PR groupings

Ordered by risk. Security and data-loss first.

- **PR 1 — "crond spool trust checks" (Critical security):** #D1, #D2. Port
  Vixie's `load_user` validation (regular file, mode `0600`, owner ∈ {root,
  user}, `nlink==1`, `O_NOFOLLOW`) and root-owned/non-writable checks for
  `/etc/crontab`. Highest priority.
- **PR 2 — "crontab -e data loss + stdin form" (Critical):** #C1, #C2, #C4,
  #C6. Temp-copy edit + atomic install + pre-validation; support no-operand and
  `-` stdin replacement.
- **PR 3 — "trustworthy identity" (Major security):** #X2 / #C5 / (at/batch
  identity). Derive identity from `getpwuid(getuid())`.
- **PR 4 — "execute at/batch jobs" (Critical):** #X1 / #A2 / #B1 / #D3. Teach
  `crond` to scan and run the at spool (queue `a` timed, queue `b` batch),
  unlinking on completion.
- **PR 5 — "secure job execution" (Major):** #D4, #D5, #D7, #D8. Clean default
  environment, `%`/stdin convention, `setsid`, output capture + mail.
- **PR 6 — "run every due job" (Major):** #D6, #D9, #D10, #D14. Per-minute wheel;
  fix `is_file_changed`; per-file reload; bounded catch-up.
- **PR 7 — "at/batch POSIX I/O & format" (Major):** #A1, #A3, #A4, #A10, #B2,
  #B3. Multi-operand timespec; submission notice → stderr; `-l` `\t`/`%e`
  format; terminal-gated prompts.
- **PR 8 — "at timezone correctness" (Major):** #A5, #A6, #A14, #B4. `TZ`-relative
  interpretation; case-insensitive zone names via `chrono-tz`; `%e` date.
- **PR 9 — "at job hardening" (Major):** #A7, #A8, #A11. `-r` ownership check;
  quoting + real `umask` in the generated script; `flock` the SEQ file.
- **PR 10 — "allow/deny + i18n + diagnostics" (Minor):** #C3, #C7, #C9, #A12,
  #A13, #A15, #C8, #B5, #B6, #D11, #D12, #D13. Route diagnostics to stderr;
  XSI "neither file exists" rule; `gettext` runtime strings; `setlocale` first;
  `sigaction`; lock-before-truncate PID file.
- **PR 11 — "de-duplicate at/batch" (cleanup):** #B7. Hoist the shared `at`
  machinery into `cron/job.rs` so `batch` is a thin wrapper.

When this audit's items are worked, tick the boxes here and append
"✓ fixed in PR #NNN" inline (do not rewrite findings).
