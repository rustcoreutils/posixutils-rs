# POSIX.1-2024 Conformance Audits — `sys/` utilities

This file collects per-utility POSIX conformance audits for the system-information
utilities crate. Each audit follows the playbook in `audits.md`.

The `sys/` crate ships six POSIX utilities: `getconf`, `ipcrm`, `ipcs`, `ps`
(with platform back-ends `pslinux.rs` / `psmacos.rs`), `uname`, and `who`.

**Audit method:** static spec-vs-code against the sliced POSIX.1-2024 tree
(`~/tmp/posix.2024/sliced/`), with every Critical/Major "absent" or
"miscompiles" claim confirmed by reading the cited code + spec lines (and the
`libc-0.2.180` crate source for portability claims). Several agent-proposed
findings were **refuted** on verification and are recorded inline. No fixes were
applied — this is an audit only.

**Date:** 2026-06-18

## Cross-cutting observations

Four patterns recur across the crate and are not repeated in every section:

- **Locale init is wired everywhere, translations are not.** All six utilities
  call `setlocale(LC_ALL, "")` + `textdomain("posixutils-rs")` +
  `bind_textdomain_codeset` in `main` and wrap user-facing strings in
  `gettext()`. But the crate ships no `.mo` catalogs, so `LANG`/`LC_ALL`/
  `LC_MESSAGES` select a non-existent catalog and diagnostics stay English. This
  is the same "diag plumbing in place; string-level catalogs deferred" status as
  the `dev/` audit — recorded once here, Minor per utility.
- **Time/locale honoring is incidental, not deliberate.** `ipcs`/`who` format
  timestamps through `chrono::Local` / libc `strftime`, which transitively honor
  `TZ` (and partly `LC_TIME`) because they call `localtime_r`. `ps` does **not**
  — its time code is hand-rolled and ignores `TZ`/`LC_TIME` entirely.
- **The Linux/`/proc` vs macOS/syscall split is real and uneven.** `ipcs` and
  `ps` carry separate back-ends; `ipcrm` and `who` rely on libc. The audits flag
  back-end divergences (e.g. `ps` `start_time` units differ between platforms).
- **i18n category coverage is structurally complete** (every var the spec lists
  is reachable through `setlocale`); the gaps are catalog content, not wiring.

---

## `getconf`

**Implementation:** `sys/getconf.rs` (767 lines)
**Tests:** `sys/tests/getconf/mod.rs`
**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 §3
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/getconf.md`
**Date:** 2026-06-18

### TL;DR

The golden path — `getconf VAR` and `getconf PATH_VAR pathname` for sysconf /
confstr / pathconf names — works and is well-mapped (both Linux and macOS
confstr tables are `#[cfg]`-branched). The conformance gaps are coverage and
currency: the `-v` specification gate only accepts the obsolescent `POSIX_V6_*`
/ `POSIX_V7_*` names and **rejects the POSIX.1-2024 `POSIX_V8_*` names the spec
now mandates** (Issue 8 / Austin Group Defect 1330), the `<limits.h>`
Maximum/Minimum compile-time constants (`LONG_BIT`, `MB_LEN_MAX`, `NL_*`, …) are
not accepted as operands at all, and `confstr` results are passed through
`.trim_end()` which can corrupt a value the spec says to print verbatim.

### Priority issues

#### Major

- [ ] **#G1 — `-v POSIX_V8_*` specifications are rejected.** `sys/getconf.rs:544-556` (`is_valid_specification`). The `matches!` arm lists only `POSIX_V6_*` and `POSIX_V7_*`. The spec (slice 99382–99406, CHANGE HISTORY 99566–99571: "Austin Group Defect 1330 is applied … changing `_V7_` to `_V8_`") makes `POSIX_V8_ILP32_OFF32`, `POSIX_V8_ILP32_OFFBIG`, `POSIX_V8_LP64_OFF64`, `POSIX_V8_LPBIG_OFFBIG` the current `-v` argument set. `getconf -v POSIX_V8_LP64_OFF64 ARG_MAX` exits 1 "invalid specification". Fix: add the four `POSIX_V8_*` arms (accept as no-op like V6/V7).
- [ ] **#G2 — `<limits.h>` Maximum/Minimum value names not accepted as operands.** Whole file — no table. Spec (99441–99455) requires names from the `<limits.h>` Maximum/Minimum tables (`LONG_BIT`, `WORD_BIT`, `MB_LEN_MAX`, `NL_ARGMAX`, `NL_LANGMAX`, `NL_MSGMAX`, `NL_NMAX`, `NL_SETMAX`, `NL_TEXTMAX`, `NZERO`, `CHARCLASS_NAME_MAX`, `COLL_WEIGHTS_MAX`, `RE_DUP_MAX`, `LINE_MAX`, …) to resolve as `system_var`. These are compile-time constants, not `sysconf` lookups, so a separate constant table is needed. Fix: add a `&str → i64` constant map seeded from `libc`/std values.
- [ ] **#G3 — `POSIX_V8_*` confstr entries missing on both platforms.** `sys/getconf.rs:186-528` (`load_confstr_mapping`): Linux branch stops at `_CS_POSIX_V7_*`, macOS branch at `_CS_POSIX_V6_*`. Per Issue 8, the `_CS_POSIX_V8_*_CFLAGS`/`_LDFLAGS`/`_LIBS` family should be queryable. Fix: add V8 confstr entries (gated per platform availability).

#### Minor

- [ ] **#G4 — `confstr` value passed through `.trim_end()`.** `sys/getconf.rs:140`. After stripping the NUL terminator the code trims trailing whitespace, but the spec output format is `"%s\n"` (verbatim). A confstr value with significant trailing spaces would be altered. Fix: keep `.trim_end_matches('\0')` only; drop the general `.trim_end()`.
- [ ] **#G5 — Several POSIX.1-2024 sysconf names absent.** `sys/getconf.rs:558-706` (`load_sysconf_mapping`). Missing the Issue-8 `NPROCESSORS_CONF`/`NPROCESSORS_ONLN` (Defect 339, spec 99567) plus a long tail (`_SC_BARRIERS`, `_SC_CLOCK_SELECTION`, `_SC_CPUTIME`, `_SC_MONOTONIC_CLOCK`, `_SC_READER_WRITER_LOCKS`, `_SC_SPIN_LOCKS`, `_SC_THREAD_CPUTIME`, `_SC_TIMEOUTS`, `_SC_TYPED_MEMORY_OBJECTS`, `_SC_XOPEN_REALTIME[_THREADS]`, `_SC_TRACE`, the `_SC_V7_*` programming-env queries). Fix: extend the map, `#[cfg]`-gating names not present on every platform.
- [ ] **#G6 — A few pathconf names absent.** `sys/getconf.rs:708-732` (`load_pathconf_mapping`): no `_PC_SYNC_IO` (peer of the mapped `_PC_ASYNC_IO`), `_PC_2_SYMLINKS`, `_PC_TIMESTAMP_RESOLUTION`. Fix: add entries; `#[cfg]`-gate the ones not on all platforms.

### Detailed conformance matrix

#### SYNOPSIS / OPTIONS / OPERANDS
- [x] `getconf [-v spec] system_var` and `getconf [-v spec] path_var pathname` forms — `getconf.rs:24-36, 752`. CONFORMS.
- [x] `--` end-of-options handled by clap. CONFORMS.
- [ ] **`-v` accepts only V6/V7** (#G1). `getconf.rs:544-556`.
- [x] `pathname` operand passed to `pathconf` via `CString` — `getconf.rs:164`. CONFORMS.

#### STDIN / INPUT FILES / OUTPUT FILES
- [x] None used. CONFORMS / N/A.

#### Environment variables
- [x] `LANG` / `LC_ALL` / `LC_CTYPE` / `LC_MESSAGES` / `NLSPATH` reachable via `setlocale(LC_ALL,"")` + textdomain — `getconf.rs:735-737`. CONFORMS (catalog content deferred, see cross-cutting).

#### STDOUT / STDERR
- [x] Numeric vars `"%d\n"`, undefined → `undefined\n`, invalid → nothing on stdout + stderr diagnostic + exit 1 — `getconf.rs:77/85/113/141/171`. CONFORMS.
- [ ] **confstr `.trim_end()`** (#G4) — `getconf.rs:140`.

#### Exit status / consequences of errors
- [x] 0 on success (incl. valid-but-`undefined` → exit 0 per RATIONALE 99532–99535); >0 on any error via `process::exit(1)`. CONFORMS — `getconf.rs:77/113/171` (undefined), error paths.

#### Cross-cutting
- [x] i18n wiring present (`getconf.rs:735-737`); catalogs deferred (Minor, crate-wide).
- [x] Portability: confstr tables are `#[cfg(target_os)]`-branched (`getconf.rs:187,305`); sysconf/pathconf maps are shared but use only constants present on both platforms.

### Refuted findings

- [x] ~~"`libc::_SC_PASS_MAX` (`getconf.rs:581`) and `libc::_PC_FILESIZEBITS` (`getconf.rs:725`) are Linux-only → macOS build break (proposed Critical)."~~ **Refuted 2026-06-18.** Verified in `libc-0.2.180/src/unix/bsd/apple/mod.rs`: `_PC_FILESIZEBITS = 18` (line 1962) and `_SC_PASS_MAX = 131` (line 3117) are both defined for Apple targets. The shared maps compile on macOS; no build break.

### Test coverage signal

Not covered:
- [ ] `getconf -v POSIX_V8_*` acceptance (#G1).
- [ ] `<limits.h>` constant operands (#G2).
- [ ] confstr trailing-whitespace preservation (#G4).
- [ ] `NPROCESSORS_CONF` / `NPROCESSORS_ONLN` (#G5).

---

## `ipcrm`

**Implementation:** `sys/ipcrm.rs` (319 lines)
**Tests:** `sys/tests/ipcrm/mod.rs`
**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 §3, pp. 3059–3061
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/ipcrm.md`
**Date:** 2026-06-18

### TL;DR

The cleanest utility in the crate. All six options (`-q -Q -s -S -m -M`) accept
repetition (`ArgAction::Append`), removal routes to the correct `*ctl(IPC_RMID)`
syscall, per-operand errors set exit 1 but continue (correct CONSEQUENCES-OF-
ERRORS behavior), and macOS's lack of SysV message queues is handled with a
`#[cfg]` guard + runtime error. The only real divergences are minor: a
fixed internal processing order that ignores argv order (Guideline 11), and
decimal key parsing capped at signed `i32`.

### Priority issues

#### Minor

- [ ] **#IR1 — Options processed in fixed internal order, not argv order.** `sys/ipcrm.rs:192-306`. The removal loops run semkey→semid→shmkey→shmid→msgkey→msgid regardless of command-line order. XBD 12.2 Guideline 11 says repeated option/option-argument pairs should be interpreted in the order specified. Harmless here (removals are independent) but a strict-conformance gap. Fix: collect a single `Vec<(Kind, value)>` in argv order during parsing and iterate it once.
- [ ] **#IR2 — Decimal keys limited to signed `i32` range.** `sys/ipcrm.rs:31`. `parse_ipc_key` parses hex via `u32::from_str_radix(...) as i32` (full 32-bit space) but decimal directly as `i32`, so a decimal key > 2147483647 fails to parse while the same value in hex succeeds. `key_t` is a 32-bit type. Fix: parse decimal as `u32` then cast `as i32`, matching the hex path.
- [ ] **#IR3 — macOS message-queue support omitted wholesale.** `sys/ipcrm.rs:284-304`. `-q`/`-Q` on macOS print an error and exit 1. This is acceptable (macOS does not provide working SysV message queues — `msgget` is effectively stubbed), but the POSIX SYNOPSIS lists `-q`/`-Q` unconditionally. Recorded as a documented platform limitation, not an actionable defect.
- [ ] **#IR4 — No `.mo` catalogs (crate-wide).** `sys/ipcrm.rs:310-312`. `LC_MESSAGES` has no runtime effect. See cross-cutting.

### Detailed conformance matrix

#### Options
- [x] `-q msgid` / `-m shmid` / `-s semid` (by id) → `*ctl(IPC_RMID)` — `ipcrm.rs:169-180` and per-type loops. CONFORMS (msg: Linux-only).
- [x] `-Q msgkey` / `-M shmkey` / `-S semkey` (by key) → key lookup then remove — `ipcrm.rs` key-lookup helpers. CONFORMS (msg: Linux-only).
- [ ] **Fixed processing order** (#IR1); **decimal `i32` cap** (#IR2).

#### Operands / STDIN / input / output files
- [x] None accepted / used. CONFORMS.

#### Environment variables
- [x] `LANG`/`LC_ALL`/`LC_CTYPE`/`LC_MESSAGES`/`NLSPATH` reachable via `setlocale` — `ipcrm.rs:310-312`. CONFORMS (catalogs deferred).

#### Asynchronous events / STDOUT
- [x] Default; no stdout. CONFORMS.

#### STDERR / exit status / consequences of errors
- [x] Diagnostics to stderr only; exit 0/1 via accumulator; per-operand continue-on-error — `ipcrm.rs:189-318`. CONFORMS.

#### Cross-cutting
- [x] Portability: msg-queue paths `#[cfg(not(target_os="macos"))]`; sem/shm portable. `semctl(IPC_RMID, semun{val:0})` portable. CONFORMS.

### Test coverage signal

Not covered:
- [ ] argv-order processing (#IR1).
- [ ] large decimal key parsing (#IR2).

---

## `ipcs`

**Implementation:** `sys/ipcs.rs` (755 lines)
**Tests:** `sys/tests/ipcs/mod.rs`
**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 §3, pp. 3062–3068
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/ipcs.md`
**Date:** 2026-06-18

### TL;DR

The option surface (`-q -m -s` selectors; `-a`/`-bcopt` modifiers), the title
line, facility ordering, OWNER/GROUP truncation, and the `T`/`KEY` columns are
correct. But the **permission MODE column is wrong on every Linux invocation**:
the kernel prints the mode field in `/proc/sysvipc/*` as octal (`%4o`) and the
implementation parses it with decimal `.parse()`, so the displayed `rwx` bits
are garbage. Two further golden-path columns never carry real data — message-
queue `QBYTES` always prints `-`, and the MODE status flags (`S`/`R` waiters,
`C` = SHM_DEST) are hard-coded `-`. I/O errors reading `/proc` are silently
swallowed (`unwrap_or_default()`), so a permission failure yields empty output
with exit 0 instead of a diagnostic.

### Priority issues

#### Critical

- [x] **#IS1 — `/proc/sysvipc/*` mode field parsed as decimal, but the kernel writes it in octal.** `sys/ipcs.rs:228, 262, 296` (the `perms: fields[2].parse().unwrap_or(0)` sites for msg/shm/sem). The Linux kernel's `*_proc_show` routines print the `ipc_perm.mode` field with `%4o`. Parsing `"644"` as decimal yields `644` (`0o1204`), so `format_mode` (`ipcs.rs:118-138`) reads the wrong bits and the MODE column is incorrect on every run. Fix: `u16::from_str_radix(fields[2], 8)`. ✓ **fixed in Phase 1** — all three `read_proc_*` sites now parse base 8; behaviorally verified against `/usr/bin/ipcs` (0640→`-rw-r-----`, 0600→`-rw-------`, 0644→`-rw-r--r--`); unit test `proc_mode_field_is_octal`.

#### Major

- [x] **#IS2 — Message-queue `QBYTES` always prints `-`.** `sys/ipcs.rs:548-550` (acknowledged in a code comment). `q_qbytes` is not in `/proc/sysvipc/msg`; obtaining it needs `msgctl(msqid, IPC_STAT, &buf)`. Spec (the `-b` column for message queues) requires the value. Fix: call `msgctl(IPC_STAT)` per entry. ✓ **fixed in Phase 2** — new `get_msg_qbytes()` helper + `MsgQueueInfo.qbytes: Option<u64>`; renders `-` only when the stat fails. Behaviorally verified (`ipcs -q -b` shows 16384, the `msgmnb` default).
- [x] **#IS3 — MODE status flags never set + wrong char count.** `sys/ipcs.rs:118-123` (`c1`/`c2` hard-coded `-`). ✓ **fixed in Phase 1.** Re-reading the spec (IEEE Std 1003.1-2024, ipcs MODE column 100916–100940) corrected the finding: POSIX mandates **11** characters — a single leading flag char (`C` if a shared memory segment is marked for clearing/`SHM_DEST`, else `-`), then 9 permission chars, then 1 ACL char. There is **no** `S`/`R` msgsnd/msgrcv flag in POSIX.1-2024 (that is a historical SysV-ism), and there is no `/proc` data source for waiters anyway. The old code emitted **12** chars (two bogus `-` flags) — itself a divergence. `format_mode` now returns the 11-char form and sets the `C` flag from `SHM_DEST`; unit tests `mode_string_is_eleven_chars_and_decodes_perms` + `shm_dest_sets_c_flag`.
- [x] **#IS4 — `-t` time column zero-pads the hour; spec format is `%d:%2.2d:%2.2d`.** `sys/ipcs.rs:165`. Uses `%H:%M:%S` (e.g. `09:30:00`); spec 100977–100985 mandates an unpadded hour (`9:30:00`). Fix: use `%-H:%M:%S`. ✓ **fixed in Phase 2**; unit test `time_no_entry_and_format`.
- [x] **#IS5 — `/proc` read errors silently swallowed; "facility not in system" never emitted on Linux for shm/sem.** `sys/ipcs.rs:500, 579, 653` (`.unwrap_or_default()`). If a facility is compiled out (file missing) or unreadable (EACCES), the code emits a header with no rows and exits 0, instead of `"%s facility not in system.\n"` (spec 100886–100889) or a stderr diagnostic + non-zero exit. Fix: branch on the `Err` from `read_proc_*`; emit the not-in-system line for missing facilities and a diagnostic for hard errors. ✓ **fixed in Phase 2** — `display_*` now return `io::Result<()>`; `NotFound` (and an empty facility, per the spec's "not used since the last reboot") emits the singular `"<facility> facility not in system."`; any other error propagates to `main` (diagnostic + non-zero exit). Behaviorally verified on a clean host (all three report "not in system").

#### Minor

- [x] **#IS6 — `get_current_date` fallback uses `%z` instead of `%Z`.** `sys/ipcs.rs:185, 200`. The primary `strftime` path correctly uses `%a %b %e %H:%M:%S %Z %Y`; only the chrono fallback (used when `localtime_r`/`strftime` fail) emits numeric `%z`. Fix: change the fallback format to `%Z`. ✓ **fixed in Phase 2** (both fallback format strings).
- [x] **#IS7 — macOS slot iteration capped at 256.** `sys/ipcs.rs` (`MAX_IPC_SLOTS_TO_CHECK = 256`). IDs beyond slot 256 are missed on busy systems. Fix: drive the bound from `kern.sysv.shmmni`/`semmni`. ✓ **fixed in Phase 2** (macOS, code-only — compiled, not run on this Linux host): the slot dimension is now bounded by `MACOS_MAX_SLOTS` (2048), separate from the per-slot sequence probe `MACOS_SEQ_PROBE`; `read_macos_sem` no longer caps slots at the sequence budget. Documented as a best-effort heuristic (macOS lacks an IPC-enumeration API).
- [ ] **#IS8 — No `.mo` catalogs (crate-wide).** See cross-cutting.

### Detailed conformance matrix

#### Options / operands / STDIN
- [x] `-q -m -s` selectors; default = all three; `-a` = `-bcopt`; `-b -c -o -p -t` modifiers — `ipcs.rs:24-48, 742-749`. CONFORMS.
- [x] No operands; stdin unused. CONFORMS.

#### Environment variables
- [x] `LANG`/`LC_ALL`/`LC_CTYPE`/`LC_MESSAGES`/`NLSPATH` via `setlocale` — `ipcs.rs:735-737`. CONFORMS.
- [x] `TZ` honored incidentally (`localtime_r` via chrono/`strftime`) — `ipcs.rs:164,189`. CONFORMS.
- [x] **`-t` hour format** (#IS4 ✓ Phase 2 — unpadded hour per `%d:%2.2d:%2.2d`). `LC_TIME`-localized digits remain a deferred crate-wide i18n item.

#### STDOUT / STDERR
- [x] Title line `IPC status from <source> as of <date>` — `ipcs.rs:719`. CONFORMS.
- [x] Facility order msg→shm→sem; `T`/`KEY`/OWNER/GROUP columns — `ipcs.rs:721-731`, truncation `ipcs.rs:98`. CONFORMS.
- [x] **MODE octal-parse bug** (#IS1 ✓ Phase 1); **MODE flags / 11-char form** (#IS3 ✓ Phase 1); **QBYTES** (#IS2 ✓ Phase 2); **shm/sem "not in system"** (#IS5 ✓ Phase 2).
- [x] **Errors propagated (not swallowed)** (#IS5 ✓ Phase 2).

#### Exit status / consequences of errors
- [x] 0 success / non-zero on hard `/proc` errors (#IS5 ✓ Phase 2 — `display_*` propagate `io::Result` to `main`; `NotFound`/empty → "not in system" at exit 0).

#### Cross-cutting
- [x] Portability: Linux `/proc/sysvipc`; macOS `shmctl`/`semctl(IPC_STAT)` slot scan (#IS7 cap raised, Phase 2); msg queues "not in system" on macOS. CONFORMS.

### Test coverage signal

Tests are structural smoke tests. Not covered:
- [x] MODE string correctness (#IS1, #IS3) — unit tests + behavioral cross-check (Phase 1).
- [x] QBYTES value (#IS2) — gated real-IPC test + behavioral check (Phase 2).
- [x] `-t` hour format (#IS4) — unit test `time_no_entry_and_format` (Phase 2).
- [x] facility-not-in-system path (#IS5) — behavioral check on a clean host (Phase 2).

---

## `ps`

**Implementation:** `sys/ps.rs` (776) + `sys/pslinux.rs` (169) + `sys/psmacos.rs` (189)
**Tests:** `sys/tests/ps/mod.rs`
**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 §3, pp. 3... (slice lines 112432–112719)
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/ps.md`
**Date:** 2026-06-18

### TL;DR

The selection options (`-A -a -d -e -g -G -p -t -u -U`), the `-l` long format,
and the `-o format` engine (field=header overrides, multiple `-o`, all-null
header suppression) are largely correct. But **`etime` is wrong on every Linux
run** — `format_etime` subtracts the process `start_time` (clock-ticks-since-
boot on Linux) from the current Unix epoch seconds, mixing incommensurable
units; the code comment even calls itself "a simplified version". CPU/elapsed
time also hardcodes 100 ticks/second instead of `sysconf(_SC_CLK_TCK)`, the
macOS PID enumeration uses a fixed 1024-entry buffer that silently truncates the
process list, and the SYNOPSIS options `-w` and `-n` (plus the `COLUMNS` env
var) are absent. `TZ`/`LC_TIME` are ignored for all time output.

### Priority issues

#### Critical

- [x] **#P1 — `etime` mixes time bases (always wrong on Linux).** `sys/ps.rs:491-517` (`format_etime`) computes `now_epoch_secs - start_time`, but `pslinux.rs:28` documents `start_time` as "clock ticks since boot". The subtraction is meaningless; the in-code comment admits "proper implementation would need boot time". Fix: read boot time (`/proc/stat` `btime` or `/proc/uptime`), convert `start_time / CLK_TCK` to an epoch instant, then take the delta. ✓ **fixed in Phase 3** — `pslinux.rs` reads `/proc/stat` `btime` once and normalizes `start_time` to epoch seconds; `format_etime(start_epoch, now_epoch)` is now pure (`now` injected from `main`) and `saturating_sub`s for skew. Behaviorally verified: `ps -p 1 -o etime` == `/usr/bin/ps` (`2-12:52:14`). Unit test `format_etime_elapsed`.
- [x] **#P2 — macOS PID buffer fixed at 1024 → silent process-list truncation.** `sys/psmacos.rs:40` (`let mut pids = vec![0; 1024];`). `proc_listallpids` with an undersized buffer returns a truncated list with no error. Fix: call `proc_listallpids(NULL, 0)` to get the count, then allocate. ✓ **fixed in Phase 7** (macOS, code-only) — count-then-allocate with +64 slack. Verified via `cargo check`/`clippy --target x86_64-apple-darwin`; runtime macOS verification pending CI.

#### Major

- [x] **#P3 — CPU/elapsed time hardcodes 100 ticks/second.** `sys/ps.rs:474-488` (`format_time`: `ticks / 100`). POSIX requires `sysconf(_SC_CLK_TCK)`; Linux `CONFIG_HZ` may be 250/1000. Fix: divide by `sysconf(_SC_CLK_TCK)`. ✓ **fixed in Phase 3** — the Linux backend divides `utime+stime` by `sysconf(_SC_CLK_TCK)` (read once); `format_time` now takes whole seconds. Behaviorally verified `TIME` == `/usr/bin/ps` (`00:00:38` for PID 1). Unit test `format_time_seconds`.
- [x] **#P4 — `-w` option and `COLUMNS` env var absent; output never width-limited.** `sys/ps.rs:92-144` (no `-w` field); `COLUMNS` never read. Spec 112481–112498 require lines to contain no more than the greater of `{LINE_MAX}` and `COLUMNS` bytes; a single `-w` behaves as if `COLUMNS` ≥ 132; repeated `-w` removes the limit. ✓ **fixed in Phase 5** — added `-w` (repeatable `Count`); `resolve_line_limit` reads `_SC_LINE_MAX` + `COLUMNS`; each row/header is clipped by `truncate_line` at a UTF-8 boundary. Behaviorally verified: a 3041-byte argv caps at 2048 by default, full under `-ww`, and `COLUMNS` raises the cap. Unit test `line_limit_and_truncation`.
- [x] **#P5 — `-n namelist` option absent.** `sys/ps.rs:92-144`. In the SYNOPSIS (XSI); the namelist format is "unspecified", so a parsed no-op is conforming. ✓ **fixed in Phase 5** — added `-n namelist`, accepted and ignored (this implementation reads live state). Integration test `ps_namelist_accepted`.
- [x] **#P6 — `stime` column always `-` in `-f` listing.** `sys/ps.rs:573`. The `-f` full format mandates STIME; the field returned `"-"`. ✓ **fixed in Phase 4** — new pure `format_stime(start_epoch, now_epoch, tz)` renders `HH:MM` if started today else `MmmDD` (matching historical `ps`), `"-"` when the start time is unknown. Behaviorally verified `ps -p 1 -o stime` == `/usr/bin/ps` (`Jun15`). Unit test `format_stime_today_vs_date`.
- [x] **#P7 — `TZ` / `LC_TIME` ignored for all time output.** `sys/ps.rs:474-517`. ✓ **fixed in Phase 4** — the only absolute-time field (`stime`) now formats through `chrono::Local`, which honors `$TZ` (verified: `TZ=UTC`→`10:16` vs `TZ=America/New_York`→`06:16`). `etime`/`time` are elapsed durations (timezone-independent by definition). `LC_TIME`-localized month/digit glyphs remain a deferred crate-wide i18n item.
- [x] **#P8 — macOS `args` is the executable path, not argv; no bracketed fallback.** `sys/psmacos.rs:171`. Spec 112547–112549: under `-f`, reconstruct argv, else write `[comm]` in brackets. Linux does this (`pslinux.rs:146`); macOS set `args = full_path` always. Fix: use `sysctl KERN_PROCARGS2` to reconstruct argv; bracket kernel/threadless procs. ✓ **fixed in Phase 7** (macOS, code-only) — new `get_process_args` parses the `KERN_PROCARGS2` `[argc][exec_path][argv...]` buffer; falls back to `[comm]` when unavailable. `cargo check`/`clippy --target x86_64-apple-darwin` clean; runtime macOS verification pending CI.

#### Minor

- [x] **#P9 — `start_time` semantics diverge between back-ends.** `pslinux.rs:28` (ticks since boot) vs `psmacos.rs` (epoch seconds). The shared `format_etime` cannot be correct for both. Fix: normalize both back-ends to epoch seconds before handing to `ps.rs`. ✓ **fixed in Phase 3** — the `ProcessInfo` contract is now documented as `start_time` = epoch seconds, `time` = whole CPU seconds; Linux converts (ticks→seconds, +btime) and macOS converts CPU ns→seconds (`start_time` already epoch). The shared formatters are unit-agnostic.
- [x] **#P10 — Defunct (zombie) processes not marked `<defunct>`.** No marking anywhere. Spec 112545–112546. Fix: append `<defunct>` to `args`/`comm` when state == `Z`. ✓ **fixed in Phase 6** — `mark_defunct()` appends ` <defunct>` to the command column for state `Z`. Behaviorally verified against a real zombie (`perl <defunct>`). Unit test `defunct_marking`.
- [x] **#P11 — Controlling-terminal detection via `isatty(STDIN)`; TTY match by substring.** `sys/ps.rs:521-537` (uses stdin, breaks under redirected stdin) and `ps.rs:714-717` (`contains()` so `pts/0` spuriously matches `pts/00`). ✓ **fixed in Phase 6** — `get_current_tty` now probes stdin→stdout→stderr (a redirected stdin no longer hides the terminal); the default filter compares TTY names by **equality** (`ptty == ctty`).
- [x] ~~**#P12 — `-f` args header is `CMD`; spec default header for `args` is `COMMAND`.**~~ **Refuted (Phase 6).** Re-reading the spec: line 112543 makes `CMD` the command-column header for the full/long listing (`-f`/`-l`), while line 112604 maps only the `-o args`/`-o comm` *format specifiers* to the default header `COMMAND`. The code already matches both (`get_full_fields` → `CMD`; `get_posix_fields` `args`/`comm` → `COMMAND`). No change needed.
- [x] **#P13 — macOS `get_tty_name` scans all of `/dev` per process; no `tty_dev==0` guard.** `sys/psmacos.rs:176-189`. O(procs × /dev). Fix: build a one-time `dev→name` map; guard `tty_dev==0 → None`. ✓ **fixed in Phase 7** (macOS, code-only) — `build_dev_name_map` runs once; `get_tty_name` is now a map lookup that returns None for `e_tdev` 0/`NODEV`. `cargo check`/`clippy --target x86_64-apple-darwin` clean.
- [ ] **#P14 — No `.mo` catalogs (crate-wide).** See cross-cutting.

### Detailed conformance matrix

#### Options
| Opt | Status | Notes (file:line) |
|---|---|---|
| `-A` / `-e` | CONFORMS | `ps.rs:94,98`. |
| `-a` | PARTIAL | `ps.rs:103` selects tty-bearing procs; optional session-leader exclusion not done (XSI "may"). |
| `-d` | CONFORMS | `ps.rs:106`, `pid==sid` check `ps.rs:702`. |
| `-f` | CONFORMS | login-name UID OK; `stime` ✓ Phase 4 (#P6); args header `CMD` is correct per spec 112543 (#P12 refuted). |
| `-g` | CONFORMS | filters by session id — `ps.rs:118,656`. |
| `-G` / `-U` / `-u` / `-p` | CONFORMS | `ps.rs:122,138,134,127`. |
| `-l` | CONFORMS | F S UID PID PPID C PRI NI ADDR SZ WCHAN TTY TIME CMD — `ps.rs:398-471`. |
| `-o` | CONFORMS | append + `name=header` + null-header suppression — `ps.rs:260-319, 739`. |
| `-t` | PARTIAL | substring match; XSI two-form (`tty04`/`04`) not handled — `ps.rs:131`. |
| `-n namelist` | CONFORMS | accepted no-op (#P5 ✓ Phase 5). |
| `-w` | CONFORMS | `-w`/`-ww` + `COLUMNS` line limit (#P4 ✓ Phase 5). |

#### Operands / STDIN / input files
- [x] None accepted / used. CONFORMS.

#### Environment variables
- [x] **`COLUMNS` read for line limit** (#P4 ✓ Phase 5). **`TZ` honored for `stime`** (#P7 ✓ Phase 4).
- [x] `LANG`/`LC_*`/`NLSPATH` via `setlocale` — `ps.rs:580-584`. CONFORMS (catalogs deferred).

#### STDOUT / STDERR
- [x] Default columns PID TTY TIME CMD (default format unspecified by spec) — `ps.rs:326-349`. CONFORMS.
- [x] `-o` all-null-header suppression correct (`any(non-empty)`) — `ps.rs:739`. CONFORMS (agent's "inverted logic" claim self-refuted; verified correct).
- [x] **`etime`/CLK_TCK** (#P1/#P3 ✓ Phase 3); **`stime`/TZ** (#P6/#P7 ✓ Phase 4).

#### Exit status / consequences of errors
- [x] 0 / 1 via `ExitCode` — `ps.rs:592,728`. CONFORMS.

#### Cross-cutting / portability
- [x] **Back-end divergences resolved:** `start_time`/`time` units (#P9 ✓ Phase 3), macOS `args` (#P8 ✓ Phase 7), macOS PID buffer (#P2 ✓ Phase 7), macOS `/dev` scan (#P13 ✓ Phase 7).

### Test coverage signal

Not covered:
- [x] `etime` numeric correctness (#P1) — unit test `format_etime_elapsed` + behavioral cross-check vs `/usr/bin/ps` (Phase 3).
- [x] `COLUMNS`/`-w` truncation (#P4) — unit test + behavioral 3041-byte argv check (Phase 5).
- [x] `-n` accepted (#P5, Phase 5); defunct marking (#P10 — unit test + real-zombie check, Phase 6). [ ] header suppression content (`ps_empty_header` only checks exit).

---

## `uname`

**Implementation:** `sys/uname.rs` (89 lines)
**Tests:** `sys/tests/uname/mod.rs`
**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 §3 (slice lines 119026–119120)
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/uname.md`
**Date:** 2026-06-18

### TL;DR

Conforming. All six options (`-a -m -n -r -s -v`), the default-to-`-s` behavior,
the fixed `-a` field order (sysname, nodename, release, version, machine) with
trailing-blank suppression, exit status, and locale init are correct. The
`uname = "0.1"` crate wraps libc `utsname` and exposes all five fields on both
Linux and macOS. No Critical or Major defects. Only Minor notes are clap-
extension hygiene (non-POSIX long options) and the unmaintained dependency.

### Priority issues

#### Minor

- [ ] **#U1 — Non-POSIX long options exposed in `--help`.** `sys/uname.rs:17-33`. clap exposes `--all`/`--machine`/… and the awkward `--osversion` (renamed to dodge clap's built-in `--version`). POSIX defines only the short letters. Harmless extensions; flagged for surface hygiene. Fix: hide long options or document them as extensions.
- [ ] **#U2 — Unmaintained `uname = "0.1"` dependency.** `sys/Cargo.toml:15`. The crate is a thin `libc::uname()` wrapper, last released years ago. Fix (optional): inline a ~5-line direct `libc::uname()` call and drop the dep, per the crate's minimal-deps principle.
- [ ] **#U3 — No `.mo` catalogs (crate-wide).** See cross-cutting. (uname field values are implementation-defined and legitimately not translated.)

### Detailed conformance matrix

#### Options / operands / STDIN / input files
- [x] `-a -m -n -r -s -v` present; `-a` = `-mnrsv`; default = `-s` — `uname.rs:17-33, 42-55, 68-76`. CONFORMS.
- [x] No operands; stdin/files unused. CONFORMS.

#### Environment variables
- [x] `LANG`/`LC_ALL`/`LC_CTYPE`/`LC_MESSAGES`/`NLSPATH` via `setlocale` — `uname.rs:62-64`. CONFORMS.

#### STDOUT / STDERR
- [x] Default `"%s\n"` sysname; `-a` five-field space-separated; partial-option ordering with no trailing blanks — `uname.rs:42-58`. CONFORMS.
- [x] Errors to stderr — `uname.rs:83`. CONFORMS.

#### Exit status / consequences of errors
- [x] 0 / 1 via `process::exit` — `uname.rs:78,85,88`. CONFORMS.

#### Cross-cutting / portability
- [x] `libc::utsname` provides all five fields on Linux and macOS. CONFORMS.

### Test coverage signal

Covered well (option order, five-field output). No gaps mapping to actionable findings.

---

## `who`

**Implementation:** `sys/who.rs` (380 lines) + `plib/src/utmpx.rs`
**Tests:** `sys/tests/who/mod.rs`
**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 §3 (slice lines 122888–122992)
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/who.md`
**Date:** 2026-06-18

### TL;DR

The option surface (`-a -b -d -H -l -m -p -q -r -s -t -T -u`), the `am i` operand
form, `-T` writable markers (`+`/`-`/`?`), `-q` count format, `-b` "system boot"
line, default and `-T` output formats, utmpx access (libc `setutxent`/
`getutxent`/`utmpxname`, portable to Linux and macOS), and exit status are
correct. Gaps: the `Utmpx` struct omits `ut_exit`, so the `-d` `<exit>` field is
never printed; `-l` prints the record's name instead of the literal `LOGIN`; and
extra operands beyond `file` are silently ignored rather than diagnosed.

### Priority issues

#### Major

- [x] **#W1 — `-d` `<exit>` field never emitted.** `plib/src/utmpx.rs:15-23` (struct had no exit field) + `sys/who.rs:247`. Spec 122908–122910 requires the termination/exit values column for dead processes. ✓ **fixed in Phase 8** — added `Utmpx.exit_status: Option<(i16,i16)>` populated from `ut_exit.e_termination`/`.e_exit` (`#[cfg(not(macos))]`; `None` on macOS, which lacks `ut_exit`); `who` emits a `term=<t> exit=<e>` field for `DEAD_PROCESS`. Unit test `dead_process_exit_field`.
- [x] **#W2 — `-l` does not print the literal `LOGIN` as `<name>`.** `sys/who.rs:248` + print functions. Spec 122912–122914 mandates the name field be `LOGIN` for login lines; the code printed `entry.user`. ✓ **fixed in Phase 8** — `display_name()` substitutes `"LOGIN"` when `typ == LOGIN_PROCESS`. Unit test `login_process_name_is_login`.
- [x] ~~**#W3 — Extra operands beyond `file` silently ignored.**~~ **Refuted (Phase 8).** Behaviorally tested: `who foo bar baz` already exits **2** with clap's `error: unexpected argument 'bar'` — the single `file: Option<PathBuf>` positional rejects a second operand. The `who am i`/`am I` form is intercepted before clap. No code change needed.

#### Minor

- [x] **#W4 — `LC_TIME` not honored for time rendering.** `sys/who.rs:69-76`. `dt.format("%b %e %H:%M")` was the hardcoded POSIX-locale shape; month names didn't localize. ✓ **fixed in Phase 9** — `fmt_timestamp` now calls `plib::locale::strftime("%b %e %H:%M", …)`, which formats via libc `localtime_r`+`strftime` so both `LC_TIME` (month/day names) and `TZ` take effect.
- [x] **#W5 — `--userproc` internal flag leaks into `--help`.** `sys/who.rs:58-59`. Non-POSIX; an implementation detail for the default `USER_PROCESS` case. ✓ **fixed in Phase 9** — marked `#[arg(hide = true)]` so it no longer appears in `--help` (verified) while remaining functional for the internal default-selection logic.
- [x] **#W6 — Negative idle time possible on clock skew.** `sys/who.rs:111`. `idle_secs` was not clamped at 0. ✓ **fixed in Phase 9** — `(now - atime).max(0)`.
- [x] **#W7 — `-s`/`-T` clap group is fragile.** `sys/who.rs:49,55`. `-s` had `default_value_t=true` (always-on, unread) conflicting with `-T`'s group. ✓ **fixed in Phase 9** — removed `default_value_t`; `-s` is now a normal opt-in flag (short remains the default; the output dispatch keys off `terminals`). The `-s`/`-T` mutual exclusion is preserved (behaviorally verified: `who -s -T` exits 2).
- [ ] **#W8 — No `.mo` catalogs (crate-wide).** See cross-cutting.

### Detailed conformance matrix

#### Options
| Opt | Status | Notes (file:line) |
|---|---|---|
| `-a` | PARTIAL | expands to sub-flags — `who.rs:359-371`. |
| `-b` | CONFORMS | "system boot" — `who.rs:245,262`. |
| `-d` | CONFORMS | `<exit>` field `term=N exit=M` (#W1 ✓ Phase 8). |
| `-H` | CONFORMS | localized headings — `who.rs:274-313`. |
| `-l` | CONFORMS | name forced to `LOGIN` (#W2 ✓ Phase 8). |
| `-m` / `am i` | CONFORMS | `who.rs:230-242, 349-354`. |
| `-p` / `-r` / `-t` | CONFORMS | `who.rs:250,249,251-252`. |
| `-q` | CONFORMS | names + `# users=N` — `who.rs:326-341`. |
| `-s` | CONFORMS | opt-in flag, mutually exclusive with `-T` (#W7 ✓ Phase 9). |
| `-T` | CONFORMS | `+`/`-`/`?` via group-write bit `0o020` — `who.rs:180-216`. |
| `-u` | CONFORMS | idle from `/dev/<line>` atime; `<pid>` column; idle clamped at 0 (#W6 ✓ Phase 9). |

#### Operands / STDIN / input files (utmpx)
- [x] `am i`/`am I` → `-m`; `file` operand via `utmpxname` — `who.rs:349-354,317`. CONFORMS.
- [x] **Extra operands diagnosed** (#W3 refuted — clap exits 2 on a second operand).
- [x] Default db path via libc (`/var/run/utmp` Linux, `/var/run/utmpx` macOS). CONFORMS (portable; macOS db may be sparse at runtime — data issue, not a bug).

#### Environment variables
- [x] `LANG`/`LC_ALL`/`LC_CTYPE`/`LC_MESSAGES`/`NLSPATH` via `setlocale` — `who.rs:344-346`. CONFORMS.
- [x] `TZ` honored (chrono `Local`). CONFORMS.
- [x] **`LC_TIME`/`TZ` honored** via `plib::locale::strftime` (#W4 ✓ Phase 9).

#### STDOUT / STDERR / exit status
- [x] Default/`-T`/`-q`/`-b`/`-H` formats — `who.rs:171,209,326-341,262,274-313`. CONFORMS.
- [x] **`-d` `<exit>`** (#W1 ✓ Phase 8); **`-l` LOGIN** (#W2 ✓ Phase 8).
- [x] 0 / non-zero via `?`. CONFORMS.

#### Cross-cutting / portability
- [x] utmpx functions present on glibc, musl (`plib/src/platform.rs` extern block), and Apple targets. CONFORMS.

### Test coverage signal

Most tests check exit code only. Not covered:
- [x] `-d` exit field (#W1) + `-l` name=`LOGIN` (#W2) — unit tests (Phase 8); extra-operand rejection (#W3 — behavioral, exit 2). [ ] `-T` state chars, `-b` "system boot" content.

---

## Suggested PR groupings

Ordered roughly by user-visible impact. Each is a small, themed unit.

- **PR A — "ipcs: correct the MODE/permission display"**: #IS1 (octal parse — the headline correctness bug), #IS3 (status flags `S`/`R`/`C`). Both touch the mode pipeline.
- **PR B — "ps: fix elapsed/CPU time"**: #P1 (etime time-base), #P3 (`_SC_CLK_TCK`), #P6 (`stime`), #P9 (normalize back-end `start_time`). The Critical cluster.
- **PR C — "ps: macOS back-end robustness"**: #P2 (PID buffer count-then-alloc), #P8 (argv reconstruction), #P13 (`/dev` map + tty guard).
- **PR D — "ps: width control + missing SYNOPSIS options"**: #P4 (`-w`/`COLUMNS`), #P5 (`-n`), #P11 (controlling-tty detection), #P12 (`COMMAND` header).
- **PR E — "ipcs: error propagation + facility reporting"**: #IS2 (`QBYTES` via `msgctl`), #IS5 (don't swallow `/proc` errors; emit "facility not in system"), #IS4/#IS6 (time format), #IS7 (macOS slot cap).
- **PR F — "who: dead/login record fields"**: #W1 (`ut_exit`), #W2 (`LOGIN`), #W3 (extra-operand diagnostic).
- **PR G — "getconf: POSIX.1-2024 currency"**: #G1 (`-v POSIX_V8_*`), #G3 (V8 confstr), #G2 (`<limits.h>` constants), #G5/#G6 (sysconf/pathconf coverage).
- **PR H — "getconf/who/ipcrm small fixes"**: #G4 (confstr trim), #W4/#W6/#W7 (who locale/clamp/group), #IR1/#IR2 (ipcrm order + key range).
- **PR I — "crate-wide i18n catalogs"**: the deferred `.mo`-catalog item shared by all six utilities (#G-/#IR4/#IS8/#P14/#U3/#W8). Mirrors the `dev/` audit's "string-level gettext deferred" status — wire actual catalogs or document the gap.
- **PR J — "uname hygiene (optional)"**: #U1 (long-option surface), #U2 (drop unmaintained dep).
