# POSIX.1-2024 Conformance Audits — `process/` utilities

This file collects per-utility POSIX conformance audits for the process-control
utilities crate. Each audit follows the playbook in `audits.md` and was
spec-vs-code verified: every Critical/Major claim below was confirmed by reading
the cited implementation lines and the cited POSIX.1-2024 slice
(`~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/<util>.md`).
Several agent-proposed findings were **refuted** on verification; those are
recorded inline (struck through) so the next auditor does not re-chase them.

**Crate:** `process/` — `env`, `fuser`, `kill`, `nice`, `nohup`, `renice`,
`timeout`, `xargs` (8 binaries) plus the shared `signal.rs` module.
**Date:** 2026-06-17

---

## Cross-cutting findings (2026-06-17)

Three themes recur across every utility in the crate. Fixing them once closes a
column of Minor items below.

### `#C1` — Runtime diagnostics are hardcoded English (i18n inert)

Every binary calls `setlocale(LC_ALL, "")` + `textdomain("posixutils-rs")` +
`bind_textdomain_codeset` in `main`, and the clap `about`/`help` strings are
wrapped in `gettext()`. But **runtime diagnostic strings** (the `eprintln!`
error/warning paths, and the `&'static str` error returns) are *not* wrapped in
`gettext()`. POSIX requires `LC_MESSAGES` to determine "the format and contents
of diagnostic messages written to standard error" for each utility. So the
locale machinery is initialized but has no effect on actual error text.
- [ ] **`#C1` Major** — wrap runtime diagnostics in `gettext()`. Sites:
  `env.rs:113` (raw OS error via `?`); `kill.rs:99,114` + `signal.rs` `&str`
  returns; `nice.rs:63`; `nohup.rs:65,74,88,94-101,110,126,135,139`;
  `renice.rs:89,95`; `timeout.rs` `eprintln!` sites; `xargs.rs:25,180,183,534`;
  `fuser.rs:648,652,901,1487,1521`.

### `#C2` — Shared `signal.rs` module

`kill` (`list_signals`/`lookup_signum`/`signum_to_name`) and `timeout`
(`parse_signal`) share `process/signal.rs` (148 lines). It holds a per-OS
`SIGLIST` table (32 entries Linux, 31 macOS). It is sound; the only blemish is
the Linux table lists the historical alias `IOT` (signo 6) *in addition to*
`ABRT` (signo 6) — harmless, see the `kill` section (`#K-refuted`).

### `#C3` — `exec`-failure exit-status mapping is inconsistent across the crate

POSIX mandates **126** (utility found, cannot invoke) / **127** (utility not
found) for the utility-launcher family (`env`, `nice`, `nohup`, `timeout`,
`xargs`). The crate is split: `nohup` (`nohup.rs:133-142`), `timeout`, and
`xargs` map these correctly; **`env` and `nice` do not** — they propagate the
exec error through `main() -> Result<…>`'s `?`, so Rust's default harness prints
the error and exits **1** for every case (see `#E1`, `#NC2`).

---

## `env`

**Implementation:** `process/env.rs` (116 lines)
**Tests:** none (no `process/tests/env/`)
**Spec:** POSIX.1-2024, Vol. 3 §3, `env` (slice lines 94028–94100)

### TL;DR

Core behavior is correct: inherit env, `-i` clear, `name=value` merge, utility
exec with the modified environment, and the no-utility stdout dump. One genuine
Critical: exec failures never produce exit 126/127 (always 1). The clap layer
leaks a non-POSIX `--ignore-env` long option and intercepts `--help`/`--version`.
No tests exist.

### Priority issues

#### Critical
- [x] **`#E1` — exec failure never yields 126/127.** ✓ fixed in Phase 1. `env.rs:88-97,113`.
  `exec_util` returns `io::Result<()>`; the `?` at `env.rs:113` propagates the
  error out of `main() -> Result<…>`, so Rust prints it and exits **1**. POSIX
  `env` EXIT STATUS mandates **126** (found, not invocable) and **127** (not
  found). Fix: match `ErrorKind::NotFound` → `exit(127)`,
  `PermissionDenied`/other → `exit(126)`, internal errors → 1-125.

#### Major
- [x] **`#E2` — `--ignore-env` long option is non-POSIX.** ✓ fixed in Phase 1. `env.rs:22-29`.
  `#[arg(short, long)]` synthesizes `--ignore-env`; POSIX `env` SYNOPSIS
  (slice 94031) is `env [-i] [name=value]... [utility [argument...]]` — only
  `-i`. Fix: drop `long`, keep `short`.
- [x] **`#E3` — `--help`/`--version` intercepted by clap.** ✓ fixed in Phase 1. `env.rs:18-20`.
  `env --help foo` is eaten by clap instead of being treated as a utility named
  `--help`. Fix: `disable_help_flag`/`disable_version_flag`, or document.

#### Minor
- [x] **`#E4` — stdout dump order is nondeterministic.** ✓ fixed in Phase 1 (BTreeMap). `env.rs:80-85`.
  `print_env` iterates a `HashMap`, so `name=value` order randomizes per run.
  POSIX does not mandate order, but determinism is expected; sort by key.
- [x] **`#E5` — `name=value` split uses bare `contains('=')`.** ✓ fixed in Phase 1. `env.rs:43`.
  A leading operand whose name is not a valid env-var name (e.g.
  `1abc=x`) is still treated as an assignment. Fix: validate the pre-`=` token
  against `[A-Za-z_][A-Za-z0-9_]*`.

#### Refuted on verification
- [x] ~~`-u name` missing~~ — re-examined: POSIX.1-2024 `env` SYNOPSIS
  (slice 94031) is `env [-i] …` with **no `-u`**. `-u` is a GNU extension, not a
  POSIX gap. Not actionable.

### Conformance matrix

| Area | Status | Notes (file:line) |
|---|---|---|
| `-i` clear environment | CONFORMS | `env.rs:22-29,61-65,94` (`.env_clear().envs()`) |
| `name=value` merge | CONFORMS | `env.rs:35-57,72-75` |
| `utility [argument…]` exec | CONFORMS | `env.rs:88-96` (PATH-searched via `Command::exec`) |
| No-utility → dump env to stdout | CONFORMS | `env.rs:109-110,80-85` |
| `--` end-of-options | CONFORMS | clap default |
| `-` operand (unspecified) | N/A | spec 94038 leaves it unspecified |
| EXIT 0 / utility status | CONFORMS | `exec` replaces image |
| EXIT 126 / 127 | MISSING | `#E1` |
| ASYNCHRONOUS EVENTS | CONFORMS | "Default"; no handlers needed |
| STDERR i18n | PARTIAL | `#C1` |
| `setlocale`/`textdomain` | CONFORMS | `env.rs:100-102` |

### Test coverage signal (none today)
- [ ] no-arg dump; `-i` empties; `-i FOO=bar` exact; `name=value` override
- [ ] `env /nonexistent` → 127; `env /etc/passwd` (non-exec) → 126 (proves `#E1`)
- [ ] `--` delimiter; `A=B` after utility name treated as utility arg
- [ ] exit-status passthrough

---

## `fuser`

**Implementation:** `process/fuser.rs` (1538 lines; Linux + macOS backends)
**Tests:** `process/tests/fuser/` (basic/tcp/udp/unix/with_user, ~377 lines)
**Spec:** POSIX.1-2024, Vol. 3 §3, `fuser` (XSI; slice lines 98770–98910)

### TL;DR

The XSI base form (`-c`, `-f`, `-u`) and the stdout/stderr channel split are
implemented. The standard does **not** include `-k`/`-M`/`-s` (those are
extensions) and they are correctly absent. Real defects: the stdout PID format
does not match the spec's `" %1d"`, the `/proc/<pid>/maps` device-number formula
uses obsolete 8-bit minor encoding, and a `/proc` scan error hard-`exit`s past
the error path. The widely-assumed "exit 1 when nothing matches" is **not** a
POSIX requirement and is correctly not implemented.

### Priority issues

#### Major
- [ ] **`#F1` — stdout PID format diverges from `" %1d"`.** `fuser.rs:1435-1438`.
  Spec (slice 98794-98795): "On standard output, the process ID in the format:
  `" %1d", <process ID>`" — exactly **one** leading space. The code uses
  `let width = if pid.to_string().len() > 4 { " " } else { "  " }` → **two**
  spaces for ≤4-digit PIDs. Fix: `print!(" {}", pid)` unconditionally.
- [ ] **`#F2` — `/proc/<pid>/maps` device number uses 8-bit minor encoding.**
  `fuser.rs:752` (`tmp_maj * 256 + tmp_min`). Modern Linux uses the split
  `makedev` encoding (minors > 255); this yields wrong device IDs on LVM / high
  minors → false negatives. Fix: `libc::makedev(tmp_maj, tmp_min)`.
- [ ] **`#F3` — `scan_procs` error hard-exits, bypassing the error path.**
  `fuser.rs:368-380` calls `std::process::exit(1)` directly instead of
  propagating the `Err` to `main`. Exit is incidentally >0 (POSIX-OK) but the
  abrupt exit skips flush/cleanup. Fix: propagate with `?`.

#### Minor
- [ ] **`#F4` — `read_proc_mounts` can panic on a malformed line.**
  `fuser.rs:833` indexes `parts[1]` without a bounds check. Kernel-generated
  `/proc/mounts` is normally well-formed, but a short line panics. Use
  `parts.get(1)` and skip.
- [ ] **`#F5` — macOS `pid.try_into().unwrap()`.** `fuser.rs:1351`. `.unwrap()`
  on `/proc`-style data is a code smell; use `pid as i32`.
- [ ] **`#F6` — `Access::File`/`Access::Filewr` use-chars dropped.**
  `fuser.rs:1421` (`_ => ()`). POSIX mandates only `c` and `r`; `f`/`F` are
  "may", so this is a behavioral gap vs GNU `fuser`, not a spec violation.
- [ ] **`#F7` — detached timeout thread leak.** `fuser.rs:1484`. The NFS-stall
  guard thread is detached and keeps running on timeout.
- [ ] **`#C1` — diagnostics hardcoded English.** `fuser.rs:648,652,901,1487,1521`.

#### Refuted on verification
- [x] ~~exit status must be >0 when no process uses any file~~ — re-examined:
  POSIX `fuser` EXIT STATUS (slice 98857-98860) is **only** "0 Successful
  completion / >0 An error occurred." There is no "1 if nothing matched"
  requirement; that is GNU/BSD behavior. `process::exit(0)` at `fuser.rs:1537`
  on a no-match success is POSIX-conformant. Not actionable.

### Conformance matrix

| Area | Status | Notes (file:line) |
|---|---|---|
| SYNOPSIS `fuser [-cfu] file...` | CONFORMS | clap; `file` required `fuser.rs:1376` |
| `-k`/`-M`/`-s` absent | CONFORMS | not in POSIX.1-2024 base form |
| `-c` mount-point scan | CONFORMS | `fuser.rs:1129-1132` |
| `-f` named-files | CONFORMS | `fuser.rs:1129` |
| `-u` `(username)` + uid fallback | CONFORMS | `fuser.rs:1442-1456` |
| PIDs → stdout; rest → stderr | CONFORMS | `print!` vs `eprint!` split |
| PID format `" %1d"` | DIVERGES | `#F1` |
| stderr write-order (98810: info first, then all PIDs) | PARTIAL | interleaved per-pid; correct only for same-file case |
| EXIT 0 / >0 | CONFORMS | (no-match=0 is OK, see refuted) |
| device-number decode | DIVERGES | `#F2` |
| ASYNCHRONOUS EVENTS | CONFORMS | "Default" |
| i18n diagnostics | PARTIAL | `#C1` |

### Test coverage signal
Present: open-fd detect, `-u` username, TCP/UDP/unix (compare vs system `fuser`).
- [ ] `-c` mount-point; `-f` block-device; multiple operands
- [ ] PID format (`" %1d"`) assertion (proves `#F1`)
- [ ] `r` (root-dir) use-char; high-minor device file (proves `#F2`)

---

## `kill`

**Implementation:** `process/kill.rs` (137 lines) + shared `process/signal.rs`
**Tests:** `process/tests/kill/mod.rs` (392 lines, ~25 tests)
**Spec:** POSIX.1-2024, Vol. 3 §3, `kill` (slice incl. lines 101500–101560)

### TL;DR

The cleanest utility in the crate. All four invocation forms work, the full
mandatory signal-name set is present on both platforms, `pid 0` / negative-pid /
signal-0 semantics are delegated correctly to `libc::kill`, per-pid failures
continue the loop, and the EXIT STATUS is right. The agent's headline
"`kill -l 6` prints IOT not ABRT" finding was **refuted**. Only Minor polish
items remain (i18n, list-format, a `-l -opt` edge case).

### Priority issues

#### Minor
- [ ] **`#K1` — `kill -l` emits one space-joined line.** `signal.rs:13-15`
  (`names.join(" ")` + one `println!`). POSIX formats each name `"%s%c"` with a
  space/newline separator, last = newline. The current output (all spaces, final
  newline) is *within* the letter of "separator shall be a `<space>` or
  `<newline>`", but a multi-line per-signal emit is the conventional form.
  Cosmetic.
- [ ] **`#K2` — `-l` followed by an option mis-parses.** `kill.rs:36-51`.
  `kill -l -s TERM …` treats `-s` as the `exit_status` operand →
  `"-s".parse::<i32>()` fails → "Invalid exit status". Fix: stop treating the
  next token as `exit_status` if it begins with `-`.
- [ ] **`#C1` — diagnostics hardcoded English.** `kill.rs:99,114`; `signal.rs`
  `&'static str` returns (`"Invalid PID"`, `"Unknown signal name"`).

#### Refuted on verification
- [x] ~~`kill -l 6` prints `IOT`; Linux table is missing `ABRT`~~ —
  re-examined: the Linux `SIGLIST` **does** contain `("ABRT", 6)` at
  `signal.rs:121`, *before* `("IOT", 6)` at `signal.rs:122`. `signum_to_name`
  returns the first match, so `kill -l 6` → `ABRT`. The `IOT` alias is a
  harmless extra entry. Not a defect.
- [x] ~~`-l exit_status` 128+N boundary at exactly 128~~ — re-examined: shell
  status for signal N is `128+N` for N≥1, so `signum > 128` (`signal.rs:21`)
  matches the spec example. Conformant.
- [x] ~~negative-pid operand rejected~~ — re-examined: `"-165".parse::<i32>()`
  succeeds (`kill.rs:57,74`); negative pids parse fine. Conformant.

### Conformance matrix

| Area | Status | Notes (file:line) |
|---|---|---|
| `-s signal_name` pid… | CONFORMS | `kill.rs:47`; SIG-prefix/case-insensitive `signal.rs:45-46,60-61` |
| `-l [exit_status]` | CONFORMS (fmt cosmetic `#K1`) | `kill.rs:36-51,108-110` |
| `-signal_name` / `-signal_number` (XSI) | CONFORMS | `kill.rs:55-60` |
| `--` end-of-options | CONFORMS | `kill.rs:52-54` |
| Mandatory signal-name set (ABRT…USR2 + RT) | CONFORMS | `signal.rs:80-148` (both OSes) |
| signal `0` (existence check only) | CONFORMS | `signal.rs:57-59`; `libc::kill(pid,0)` |
| pid 0 / negative pid (process group) | CONFORMS | delegated to `libc::kill` |
| Per-pid failure continues; EXIT >0 | CONFORMS | `kill.rs:95-103` |
| EXIT 0 success | CONFORMS | `kill.rs:93,129` |
| i18n diagnostics | PARTIAL | `#C1` |

### Test coverage signal
Strong (delivery, `-l` numeric/128+N, signal 0, `--`, multiple pids). Gaps:
- [ ] `-l` exact `%s%c` format; actual negative-pid (process-group) delivery
- [ ] `-l -s …` edge case (proves `#K2`)

---

## `nice`

**Implementation:** `process/nice.rs` (70 lines)
**Tests:** none
**Spec:** POSIX.1-2024, Vol. 3 §3, `nice` (slice lines 108426–108540)

### TL;DR

Two genuine Criticals: a failed `nice()` aborts instead of still invoking the
utility (the spec's central guarantee), and exec failures never produce 126/127.
The `nice()` return-value check (`res < 0`) is also wrong — `-1` is a legal new
nice value — and the increment is artificially clamped to `[-30, 29]` by clap.

### Priority issues

#### Critical
- [x] **`#NC1` — a failed `nice()` must NOT prevent invoking the utility.** ✓ fixed in Phase 2.
  `nice.rs:60-65`. Spec (slice 108435-108437): if the user lacks privilege the
  nice value "shall not [be] affect[ed]… but this shall not prevent the
  invocation of utility or affect the exit status." The code does
  `if res < 0 { eprintln!; return Err(...) }` → exits without exec. Fix: on
  `EPERM`, optionally warn, then fall through to `exec_util`.
- [x] **`#NC2` — exec failure never yields 126/127.** ✓ fixed in Phase 2. `nice.rs:44-51,67`.
  Same `?`-propagation defect as `#E1`: `Command::exec()` error bubbles out of
  `main`, exiting **1**. Spec (slice 108484-108486 + the 108504 rationale)
  mandates 127/126. Fix: match `NotFound`→127, other exec err→126, internal→1-125.

#### Major
- [x] **`#NC3` — increment clamped to `[-30, 29]` by clap.** ✓ fixed in Phase 2. `nice.rs:27`
  (`.range(-30..30)`). POSIX places no fixed range on the `-n` argument — the
  kernel's `nice(3)` clamps the result. `nice -n 40 cmd` (historical) and
  `nice -n -20 cmd` are rejected at parse time. Fix: drop `.range()`.
- [x] **`#NC4` — `nice()` return checked as `res < 0`.** ✓ fixed in Phase 2. `nice.rs:60-61`.
  `nice(3)` returns the *new nice value*; `-1` is legal (root, increment to -1)
  yet trips the error path. Fix: clear `errno`, call, then test `errno`.

#### Minor
- [x] **`#NC5` — `--niceval` long option is non-POSIX.** ✓ fixed in Phase 2. `nice.rs:23-35`. `-n`
  is correct; the derived `--niceval` long form is an unspecified extension.
- [ ] **`#C1` — diagnostic at `nice.rs:63` not gettext-wrapped.**

### Conformance matrix

| Area | Status | Notes (file:line) |
|---|---|---|
| `-n increment`, default 10 | CONFORMS (range bug `#NC3`) | `nice.rs:23-27` |
| `utility [argument…]` exec | CONFORMS | `nice.rs:44-51` |
| Failed nice still execs utility | DIVERGES | `#NC1` |
| EXIT utility status | CONFORMS (success path) | `exec` replaces image |
| EXIT 126 / 127 | MISSING | `#NC2` |
| `setlocale`/`textdomain` | CONFORMS | `nice.rs:54-56` |
| i18n diagnostics | PARTIAL | `#C1` |
| ASYNCHRONOUS EVENTS | CONFORMS | "Default" |

### Test coverage signal (none today)
- [ ] default increment runs; `-n 5` applied; unprivileged negative increment
  still execs (proves `#NC1`); `nonexistent`→127; non-exec file→126;
  `-n 40` accepted (proves `#NC3`)

---

## `nohup`

**Implementation:** `process/nohup.rs` (145 lines)
**Tests:** none
**Spec:** POSIX.1-2024, Vol. 3 §3, `nohup` (slice lines 108886–108996)

### TL;DR

SIGHUP is ignored before exec, the `nohup.out` (cwd → home fallback) path
exists, and the 126/127 exit mapping is correct. But the created file is **not**
mode 0600 (uses umask), the home fallback reads `dirs::home_dir()` instead of
`$HOME`, the stderr-redirect logic misses the spec's "redirect to the same open
file description as stdout" case, and a file-open failure `panic!`s (exit 101)
instead of the controlled error path.

### Priority issues

#### Critical
- [x] **`#NH1` — `nohup.out` not created mode 0600.** ✓ fixed in Phase 4. `nohup.rs:26-40`.
  `OpenOptions::new().create(true).append(true)` honors umask (→ 0644/0666).
  Spec (slice 108896) mandates `S_IRUSR | S_IWUSR` (0600) on creation. Fix:
  `OpenOptionsExt::mode(0o600)`.
- [x] **`#NH2` — home fallback uses `dirs::home_dir()`, not `$HOME`.** ✓ fixed in Phase 4.
  `nohup.rs:34`. Spec (slice 108916) names the **`HOME`** environment variable.
  `dirs::home_dir()` consults `getpwuid` and may ignore a set `$HOME`. Fix:
  `std::env::var("HOME")`.

#### Major
- [x] **`#NH3` — stderr redirect misses the "follow stdout fd" case.** ✓ fixed in Phase 4.
  `nohup.rs:106-113`. Spec (slice 108897-108900): "If standard error is a
  terminal and standard output is open but is not a terminal, all output … to
  its standard error shall be redirected to **the same open file description as
  the standard output**." The code only redirects stderr to a freshly-opened
  `nohup.out`; when stdout is a non-terminal pipe/file it should `dup2` stdout's
  fd onto stderr instead. Fix: branch on stdout's terminal/open state per spec.
- [x] **`#NH4` — file-open failure panics (exit 101) instead of erroring.** ✓ fixed in Phase 4.
  `nohup.rs:81-82` (`.expect(...)`). Spec APPLICATION USAGE expects an internal
  failure to exit 127 with a diagnostic, not a Rust panic backtrace. Fix:
  handle the `Err` and `exit(127)`.

#### Minor
- [x] **`#NH5` — `signal(SIGHUP, SIG_IGN)` return value unchecked.** ✓ fixed in Phase 4.
  `nohup.rs:57-60`. A `SIG_ERR` return goes undetected. Check and `exit(127)`.
- [x] **`#NH6` — "appending output" notice wording/format.** ✓ fixed in Phase 4. `nohup.rs:94-101`.
  Functional and correctly gated on `stdout().is_terminal()`, but the message
  text is bespoke (not gettext-wrapped — `#C1`).

### Conformance matrix

| Area | Status | Notes (file:line) |
|---|---|---|
| `nohup utility [argument…]` | CONFORMS | `nohup.rs:70-77` (missing-operand → 127) |
| SIGHUP set SIG_IGN before exec | CONFORMS (unchecked `#NH5`) | `nohup.rs:57-60` |
| stdout terminal → append nohup.out | CONFORMS | `nohup.rs:84-90` (`O_APPEND`) |
| nohup.out mode 0600 | MISSING | `#NH1` |
| HOME fallback via `$HOME` | DIVERGES | `#NH2` |
| stderr → same fd as stdout (stdout open non-tty) | DIVERGES | `#NH3` |
| "appending" notice → stderr | CONFORMS (wording `#NH6`) | `nohup.rs:94-101` |
| EXIT 127 not found / 126 cannot-invoke | CONFORMS | `nohup.rs:133-142` |
| EXIT utility status | CONFORMS | `nohup.rs:118` |
| File-open failure handling | DIVERGES (panic) | `#NH4` |
| i18n diagnostics | PARTIAL | `#C1` |

### Test coverage signal (none today)
- [ ] stdout-tty → nohup.out created mode 0600 in cwd (proves `#NH1`)
- [ ] cwd unwritable → `$HOME/nohup.out` honoring a set `$HOME` (proves `#NH2`)
- [ ] stdout=pipe, stderr=tty → stderr follows stdout fd (proves `#NH3`)
- [ ] SIGHUP ignored in child; not-found→127; non-exec→126; status passthrough

---

## `renice`

**Implementation:** `process/renice.rs` (137 lines) + `plib::priority`
**Tests:** none
**Spec:** POSIX.1-2024, Vol. 3 §3, `renice` (slice lines 113200–113330)

### TL;DR

The core algorithm is right: relative increment (`getpriority` + add + clamp +
`setpriority`), `-g`/`-p`/`-u` mutually exclusive, username→uid via `getpwnam`.
But it accepts only a **single** `ID` where POSIX requires `ID...`, so per-ID
error continuation is structurally impossible; numeric ID `0` is wrongly
rejected; and the `-n` range excludes `+20`.

### Priority issues

#### Critical
- [x] **`#RN1` — only one `ID` accepted; POSIX requires `ID...`.** ✓ fixed in Phase 3.
  `renice.rs:67` (`id: String`). The SYNOPSIS is
  `renice [-g|-p|-u] -n increment ID...` — one or more. A second operand makes
  clap error "unexpected argument". Fix: `id: Vec<String>` + loop.
- [x] **`#RN2` — no per-ID error continuation.** ✓ fixed in Phase 3. `renice.rs:125-134`. CONSEQUENCES
  OF ERRORS = "Default": a failure on one ID must not stop the others, and exit
  must be >0 if any failed. The `?` chain aborts on the first error (and only
  one ID exists anyway). Fix: loop, accumulate failures, exit non-zero at end.

#### Major
- [x] **`#RN3` — numeric ID `0` rejected.** ✓ fixed in Phase 3. `renice.rs:85`
  (`Ok(0) => Err("Invalid ID")`). `0` is valid: PID/PGID 0 = caller's
  process/group, UID 0 = root. Fix: remove the `Ok(0)` reject arm.
- [x] **`#RN4` — `-n` range excludes `+20`.** ✓ fixed in Phase 3. `renice.rs:27` (`.range(-20..20)`,
  exclusive upper). Historical max increment is `+20` (spec EXAMPLES cite 19/20).
  Fix: `.range(-20..=20)`.

#### Minor
- [ ] **`#C1` — diagnostics at `renice.rs:89,95` not gettext-wrapped.**

#### Refuted / N/A
- [x] ~~`-n` is `--niceval` not `-n`~~ — re-examined: `#[arg(short)]` on field
  `niceval` derives `-n` correctly (`renice.rs:23-34`); the `--niceval` long form
  is an unspecified extension only. Folded into `#NC5`-style note, not a defect.
- [x] ~~obsolescent positional `renice priority … ID…` form missing~~ — that
  form is **no longer specified** by POSIX.1-2024 (RATIONALE); N/A, not a gap.

### Conformance matrix

| Area | Status | Notes (file:line) |
|---|---|---|
| `-n increment` (relative) | CONFORMS (range `#RN4`) | `renice.rs:23-34,131` (`prio + niceval`) |
| `-g`/`-p`/`-u` mutually exclusive | CONFORMS | `group="mode"` `renice.rs:38,47,58` |
| `-p` default | CONFORMS | `renice.rs:113-121` |
| `-u` name or numeric uid | CONFORMS | `renice.rs:83-101` (`getpwnam`) |
| `ID...` (≥1) | DIVERGES | `#RN1` |
| ID `0` accepted | DIVERGES | `#RN3` |
| clamp to PRIO_MIN/MAX | CONFORMS | `renice.rs:131` |
| per-ID continue + EXIT >0 | MISSING | `#RN2` |
| EXIT 0 success | CONFORMS | `renice.rs:136` |
| i18n diagnostics | PARTIAL | `#C1` |

### Test coverage signal (none today)
- [ ] single PID; multiple PIDs in one call (proves `#RN1`); `-g`; `-u name`;
  `-u 0`; partial-failure continuation (proves `#RN2`); `-n 20` accepted
  (proves `#RN4`); ID `0` accepted (proves `#RN3`)

---

## `timeout`

**Implementation:** `process/timeout.rs` (489 lines) + shared `signal.rs`
**Tests:** `process/tests/timeout/mod.rs` (320 lines, broad)
**Spec:** POSIX.1-2024, Vol. 3 §3, `timeout` (slice lines 117540–117670)

### TL;DR

The most complete utility here: duration grammar (suffix + fraction), the full
124/125/126/127 exit map, `-k`/`-s`/`-p`/`-f`, and signal forwarding all work
for the common case. Three real defects: `alarm(2)` truncates sub-second
durations to whole seconds; the child unconditionally resets SIGTTIN/SIGTTOU to
`SIG_DFL` (spec: inherit timeout's own disposition); and only 5 signals are
forwarded where the spec says any terminate-default signal must be.

### Priority issues

#### Critical
- [x] **`#T1` — fractional/sub-second duration truncated by `alarm()`.** ✓ fixed in Phase 5.
  `timeout.rs:95` (`libc::alarm(duration.as_secs() as c_uint)`). `as_secs()`
  drops the fraction, so `timeout 0.5s …` arms a **0-second** alarm (never fires)
  and `timeout 1.9s …` fires at 1s. The parser correctly accepts fractions
  (slice 117549-117556), but the timer discards them. Fix: `setitimer(ITIMER_REAL)`
  (or `timerfd`) with microsecond precision. Also affects `-k` (`timeout.rs:136`).
- [x] **`#T2` — child resets SIGTTIN/SIGTTOU to `SIG_DFL` unconditionally.** ✓ fixed in Phase 5.
  `timeout.rs:384-385` (in `pre_exec`). Spec (slice 117593-117598): the child's
  dispositions "shall be the same as the disposition that timeout inherited,"
  except the `-s` signal. If timeout was launched with SIGTTIN/SIGTTOU ignored
  (e.g. under another timeout/nohup), the child must inherit *ignored*, not
  default. Fix: save the original disposition before the `SIG_IGN` install at
  `timeout.rs:363-364` and restore *that* in `pre_exec`.

#### Major
- [x] **`#T3` — only 5 signals forwarded to the child.** ✓ fixed in Phase 5. `timeout.rs:212-246`
  forwards SIGALRM/SIGINT/SIGQUIT/SIGHUP/SIGTERM (+ `-s`). Spec (slice
  117587-117591): forward the `-s` signal *or any signal whose default action is
  to terminate*. SIGPIPE/SIGUSR1/SIGUSR2/SIGXCPU/SIGVTALRM/SIGPROF etc. are
  omitted — a SIGPIPE to timeout kills it silently, orphaning the child. Fix:
  register the full terminate-default set.
- [x] **`#T4` — `WCOREDUMP` branch exits 125 instead of re-raising.** ✓ fixed in Phase 5.
  `timeout.rs:436-439`. On a core-dumping child, timeout returns 125 rather than
  mirroring the child's signal death (with core suppressed). Fix: fall through to
  the `raise(signal)` path.

#### Minor
- [x] **`#T5` — SIGCONT after stop sent only to child PID.** ✓ fixed in Phase 5. `timeout.rs:420-422`
  — in non-foreground mode it should also reach the process group.
- [x] **`#T6` — race: signal before `MONITORED_PID` is set.** ✓ fixed in Phase 5. `timeout.rs:131`
  exits `128+signal` if a signal lands between handler setup (`:361`) and PID
  store (`:405`). Use a `-1` sentinel.
- [ ] **`#C1` — diagnostics not gettext-wrapped.**

### Conformance matrix

| Area | Status | Notes (file:line) |
|---|---|---|
| `[-fp] [-k time] [-s sig] duration utility …` | CONFORMS | `timeout.rs:32-50` |
| duration suffix s/m/h/d + fraction parse | CONFORMS | `timeout.rs:68-85` |
| duration timer honors fraction | DIVERGES | `#T1` |
| `0` disables timeout | CONFORMS | `timeout.rs:94` |
| `-k` second-stage SIGKILL | CONFORMS (truncation `#T1`) | `timeout.rs:135-136` |
| `-s` (case/prefix-insensitive, default TERM) | CONFORMS | `timeout.rs:41`, `signal.rs:45-46` |
| `-f`/`-p` semantics | CONFORMS | `timeout.rs:351-358,440-458` |
| Forward terminate-default signals | PARTIAL | `#T3` |
| Briefly ignore signal during group-kill | CONFORMS (narrow race) | `timeout.rs:107,141-148` |
| Child SIGTTIN/SIGTTOU disposition inherited | DIVERGES | `#T2` |
| EXIT 124/125/126/127, status passthrough | CONFORMS | `timeout.rs:346,394,399,401,432,455` |
| `WCOREDUMP` re-raise | DIVERGES | `#T4` |
| i18n diagnostics | PARTIAL | `#C1` |

### Test coverage signal
Broad (124/125/126/127, suffixes, `-p`/`-f`, kill). Gaps:
- [ ] sub-second duration precision (proves `#T1`); `-k` fractional time
- [ ] child inherits ignored SIGTTIN/SIGTTOU (proves `#T2`)
- [ ] forwarding SIGUSR1/SIGPIPE to the timeout process (proves `#T3`)

---

## `xargs`

**Implementation:** `process/xargs.rs` (701 lines)
**Tests:** `process/tests/xargs/mod.rs` (545 lines, broad)
**Spec:** POSIX.1-2024, Vol. 3 §3, `xargs` (slice lines 123150–123320)

### TL;DR

The option surface (`-ptx -E -I -L -n -s -0`), the quote/backslash word-splitter,
and the 126/127 exit map are largely implemented and well-tested. Two genuine
defects: the POSIX.1-2024 **`-r`** option is missing *and* the empty-input case
runs the utility **zero** times where POSIX mandates **exactly once**; and the
byte-to-`char` cast in the word-splitter corrupts multibyte UTF-8 arguments.

### Priority issues

#### Critical
- [x] **`#X1` — `-r` missing; empty input invokes 0× (POSIX: exactly 1×).** ✓ fixed in Phase 6.
  `xargs.rs` (no `-r` field) + `xargs.rs:680` (`else if !state.args.is_empty()`).
  Spec (slice 123180-123182): "If no arguments are supplied on standard input,
  the utility … shall be executed **exactly once** if the −r option is not
  specified," and `-r` (slice 123225) suppresses that. The impl has no `-r` and
  *never* runs the utility on empty input — the inverse of the default. Fix: add
  `-r`; when absent and input is empty, invoke once with the fixed args.
  (Test `xargs_empty_input` currently encodes the wrong expectation.)

#### Major
- [x] **`#X2` — `c8 as char` corrupts multibyte UTF-8.** ✓ fixed in Phase 6. `xargs.rs:367,452`.
  Casting a `u8` ≥ 0x80 to `char` yields the Latin-1 scalar, not the decoded
  multibyte codepoint, so `é` (0xC3 0xA9) becomes two wrong chars. POSIX requires
  `LC_CTYPE`-aware splitting. Fix: decode buffers via `str::from_utf8`/`chars()`.
- [x] **`#X3` — newline inside quotes silently accepted.** ✓ fixed in Phase 6. `xargs.rs:369-378`.
  Spec allows only "non-double-quote non-`<newline>`" chars inside double-quotes
  (likewise single). A quoted newline falls through to the generic push instead
  of erroring. Fix: error on `in_quote && ch == '\n'`.
- [x] **`#X4` — `-E` EOF string ignored in `-I` insert mode.** ✓ fixed in Phase 6.
  `xargs.rs:616-620` — `postprocess` (the `eofstr` check) runs only on the
  non-insert path. Fix: apply the EOF check in insert mode too.

#### Minor
- [x] **`#X5` — `-p` prompt format.** ✓ fixed in Phase 6. `xargs.rs:133` fuses command + `?...` in
  one `eprint!`; the affirmative test is ASCII `y`/`Y` rather than the locale
  `yesexpr`. (`/dev/tty` read at `xargs.rs:137` is correct.)
- [ ] **`#C1` — error strings at `xargs.rs:25,180,183,534` not gettext-wrapped.**

#### Refuted on verification
- [x] ~~signal-killed child must exit 125 and stop~~ — re-examined: POSIX
  `xargs` EXIT STATUS (slice 123300-123306) defines only 0 / 1-125 / 126 / 127,
  with no 125-for-signal rule and no stop-on-signal mandate. `exit_code_from_status`
  computes `128+sig` (`xargs.rs:196`) but `handle_exec_result!` (`xargs.rs:550-552`)
  folds any non-zero non-255 code into `any_failed`, and `main` returns **1**
  (`xargs.rs:688`) — inside the conformant 1-125 band. No out-of-range code
  leaks. Not a defect.
- [x] ~~exit-255 must print a diagnostic~~ — POSIX has no 255/124 rule; returning
  1 (`xargs.rs:549`) is conformant. Not actionable.

### Conformance matrix

| Area | Status | Notes (file:line) |
|---|---|---|
| `-p -t -x` | CONFORMS | `xargs.rs:92-108,162-164` |
| `-E eofstr` / `-0` | CONFORMS (insert-mode `#X4`) | `xargs.rs:77-82,98-99,503-508` |
| `-I replstr` (implies `-x`, per-line) | CONFORMS | `xargs.rs:84-90,238,512-541` |
| `-L number` (cont. on trailing blank) | CONFORMS | `xargs.rs:47-55,396-418` |
| `-n number` | CONFORMS | `xargs.rs:57-65,273-276` |
| `-s size` (ARG_MAX-derived default) | CONFORMS | `xargs.rs:67-74,253` |
| `-r` | MISSING | `#X1` |
| empty input → invoke once | DIVERGES (0×) | `#X1` |
| default utility = echo | CONFORMS | `xargs.rs:110` |
| word-split: blanks/quotes/backslash | CONFORMS (UTF-8 `#X2`, nl-in-quote `#X3`) | `xargs.rs:330-494` |
| `-p` → `/dev/tty` | CONFORMS (fmt `#X5`) | `xargs.rs:137` |
| EXIT 0 / 1-125 / 126 / 127 | CONFORMS | `xargs.rs:549-558,688` |
| i18n diagnostics | PARTIAL | `#C1` |

### Test coverage signal
Broad (n/s/L/I/E/0/x, quoting, escapes, 126/127/255). Gaps:
- [ ] `-r` + empty input invoke-once (proves `#X1`; fix `xargs_empty_input`)
- [ ] multibyte UTF-8 argument round-trip (proves `#X2`); newline-in-quotes error
- [ ] `-E` + `-I` interaction (proves `#X4`)

---

## Summary — priority counts

| Util | Critical | Major | Minor | Refuted |
|---|---|---|---|---|
| `env` | 1 (`#E1`) | 2 (`#E2`,`#E3`) | 2 | 1 (`-u`) |
| `fuser` | 0 | 3 (`#F1`–`#F3`) | 4 + `#C1` | 1 (exit-on-no-match) |
| `kill` | 0 | 0 | 2 + `#C1` | 3 |
| `nice` | 2 (`#NC1`,`#NC2`) | 2 (`#NC3`,`#NC4`) | 1 + `#C1` | 0 |
| `nohup` | 2 (`#NH1`,`#NH2`) | 2 (`#NH3`,`#NH4`) | 2 + `#C1` | 0 |
| `renice` | 2 (`#RN1`,`#RN2`) | 2 (`#RN3`,`#RN4`) | `#C1` | 2 (N/A) |
| `timeout` | 2 (`#T1`,`#T2`) | 2 (`#T3`,`#T4`) | 2 + `#C1` | 0 |
| `xargs` | 1 (`#X1`) | 3 (`#X2`–`#X4`) | 1 + `#C1` | 2 |

Crate-wide: **10 Critical, 16 Major**, plus the `#C1` i18n theme touching all 8.
Zero test coverage for `env`, `nice`, `nohup`, `renice`.

## Suggested PR groupings

- **PR A — "exec-launcher exit-status 126/127" (`#E1`, `#NC2`)**: shared pattern;
  `env` + `nice` should map exec errors like `nohup`/`timeout`/`xargs` already
  do (`#C3`). Add `nonexistent`→127 / non-exec→126 tests.
- **PR B — "nice honors the POSIX invoke-anyway guarantee" (`#NC1`, `#NC3`,
  `#NC4`)**: drop the `?`-abort, fix the `errno` check, remove the clap range.
- **PR C — "renice accepts `ID...` and continues per-ID" (`#RN1`–`#RN4`)**:
  `Vec<String>` operand, loop with error accumulation, accept ID 0, `-20..=20`.
- **PR D — "nohup file semantics" (`#NH1`–`#NH4`)**: 0600 mode, `$HOME`,
  stderr-follows-stdout, no-panic on open failure.
- **PR E — "timeout sub-second + signal fidelity" (`#T1`–`#T4`)**: `setitimer`,
  inherit SIGTTIN/SIGTTOU, full forwarding set, WCOREDUMP re-raise.
- **PR F — "xargs `-r` + empty-input + UTF-8" (`#X1`–`#X4`)**: add `-r`, invoke
  once on empty input (fix `xargs_empty_input`), decode UTF-8, reject quoted nl.
- **PR G — "fuser output + /proc robustness" (`#F1`–`#F5`)**: `" %1d"` format,
  `makedev`, propagate scan errors, bounds-check `/proc/mounts`.
- **PR H — "env/kill polish" (`#E2`–`#E5`, `#K1`, `#K2`)**: clap surface,
  `-l` format/edge.
- **PR I — "crate-wide i18n" (`#C1`)**: route every runtime diagnostic through
  `gettext()`; consider a shared `plib`-style diag helper (mirrors the `dev/`
  crate's `plib::diag` consolidation).
