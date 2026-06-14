# POSIX.1-2024 Conformance Audit — `uucp/` utilities

Per-utility POSIX conformance audit for the UUCP utilities crate, following the
playbook in `../audits.md`.

**Scope:** the three POSIX.2024 UUCP utilities — `uucp`, `uux`, `uustat` — plus
the shared `uucp/common.rs`. (`uuencode`/`uudecode` are a different facility and
live in the `xform/` crate; they are **out of scope** here.)

**Implementations:** `uucp/uucp.rs` (275), `uucp/uux.rs` (544), `uucp/uustat.rs`
(132), shared `uucp/common.rs` (529).
**Tests:** `uucp/tests/{uucp,uux,uustat}/mod.rs` (376 / 231 / 267).
**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 §3 — `uucp` pp. 3552–3555,
`uux` pp. 3567–3570, `uustat` pp. 3564–3566.
**Reference slices:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/{uucp,uux,uustat}.md`
**Date:** 2026-06-14
**Method:** static spec-vs-code audit with light behavioral confirmation of our
own binaries (option parsing, error paths, golden-path copy/exec). There is **no
reference UUCP installed** (no Taylor/HDB UUCP), so this is not a byte-for-byte
behavioral comparison — and a behavioral oracle would be of limited value
anyway, since this implementation uses **SSH as the transport** rather than the
traditional `g`/`f`/`t` protocols + `uucico` daemon.

## Framing: deliberately minimal UUCP

The project treats UUCP as a dead/dying protocol: the implementation is meant to
be **functional but absolutely minimal to what POSIX requires, and no more**.
This audit is scored against that stance, so findings are partitioned:

- **POSIX-mandated surface gaps** — places where the user-visible contract
  (options, operands, exit status, STDIN/STDOUT/STDERR routing, error semantics)
  does *less* than a POSIX "shall." These are the actionable punch list
  (unchecked `- [ ]`).
- **Accepted architecture divergences** — behaviors that diverge from POSIX
  because the SSH/no-spool-daemon design legitimately omits the traditional
  spool + deferred-delivery + multi-hop-routing model. These are **DIVERGES (by
  design)**; they need no action and are pre-checked `- [x]` with a rationale, so
  the outstanding-work view stays clean.
- **Minimalism / trim** — non-POSIX extensions or gold-plating that could be
  removed. (Spoiler: the CLI surface is already option-minimal — there are no
  non-POSIX options to trim.)

## TL;DR

The three utilities are **option-complete** against POSIX (every mandated option
letter and operand form is parsed) and the common golden paths work: local↔local
and local↔remote `uucp` copy, local/remote `uux` command execution, the
mandated `uux` redirection-operator rejection (`>>`/`<<`/`>|`/`>&`), and
`uustat` queue listing/kill/rejuvenate. There are **no Critical defects** (no
crash/hang/data-loss on the golden path) and the command-line surface carries
**zero non-POSIX options** — it is already minimal. The one real mandated gap is
in `uux`: **cross-system output-file routing is an unimplemented TODO**, and a
null-system output redirect (`>!file`) is routed to the execution host instead
of the local system, so POSIX `uux` EXAMPLE 3 does not fully work. Everything
else outstanding is Minor (hardcoded-English diagnostics, `.unwrap()` on locale
setup, throwaway `-j` job IDs, `$USER`-based identity). The large remainder —
`-r` queuing without a daemon to drain it, inert `-c`/`-C`, rejected multi-hop
routes, unsupported remote globbing, discarded command stdout — are accepted
consequences of the minimal SSH design.

## Priority issues

### Critical
None.

### Major
- [x] **#UX1 — `uux` cross-system output-file routing is unimplemented, and a
  null-system (`!`) output target is routed to the exec host, not local.**
  ✓ Phase 3 (minimal-correct): a `>`-target with an explicit `!` now names a
  system — a null system-name (`>!file`) resolves to the **local** system, while
  a target with no `!` still defaults to the execution host. The new
  `deliver_output()` actually moves the produced file: exec-remote→output-local
  via `ssh_fetch_file`, exec-local→output-remote via `ssh_send_file`; same-host
  is a no-op; a third remote system (neither local nor the exec host) is a **hard
  error** (non-zero exit) instead of the old silent TODO warning. POSIX EXAMPLE 3
  now targets the local PUBDIR. Verified: local-exec local-output writes in
  place; a remote output attempts SSH delivery and fails loudly when unreachable;
  unit tests cover the `>!file`→local resolution, same-host no-op, and
  third-system error. (Cross-host SSH delivery itself isn't exercised without a
  multi-host setup, but reuses the already-tested transfer helpers.)

### Minor
- [x] **#G1 — Diagnostics are hardcoded English; `LC_MESSAGES`/`NLSPATH` inert.**
  ✓ Phase 1: every user-facing `eprintln!`/`println!` diagnostic (and the
  `uustat -q` informative line) across `uucp.rs`/`uux.rs`/`uustat.rs` now routes
  its static clause through `gettext()`, with runtime values interpolated
  outside. English wording is byte-identical in the C/POSIX locale (all tests +
  diagnostics verified unchanged).
- [x] **#G2 — `textdomain()` / `bind_textdomain_codeset()` results are
  `.unwrap()`-ed in `main`.** ✓ Phase 1: replaced with `.ok()` in all three
  `main()` so a hostile locale environment degrades gracefully instead of
  panicking.
- [ ] **#UC1 — `-j` emits a throwaway job ID for immediate transfers that
  `uustat` cannot act on.** `uucp.rs:223-225` calls `generate_job_id()` for a
  non-`-r` transfer and prints it; no `J.*` spool record exists, so the ID is
  meaningless. `uux.rs:110-114` does the same — and `uux` *never* persists a job,
  so its `-j` ID is *always* unusable by `uustat`. POSIX: the job ID "can be used
  by `uustat` to obtain the status or terminate a job." Fix: for `uucp`, print
  the queued job's ID only when a job is actually created (i.e. effectively only
  meaningful under `-r`); for `uux`, document that the ID is informational only.
- [x] **#G3 — Current-user identity and `uustat` ownership checks use `$USER`,
  not `getpwuid(getuid())`.** ✓ Phase 2: added `common::current_login()`
  (`getpwuid(getuid())` via `plib::user::get_by_uid`, env only as fallback) and
  `common::is_root()` (`getuid()==0`). The `Job` owner, the `uustat -k`/`-r`
  ownership guard, and the mail recipients now use them. Verified: the recorded
  owner ignores a spoofed `USER=evil`, and a foreign-owned job cannot be killed
  even when `$USER` is spoofed to match (new `test_uustat_kill_foreign_job_denied`,
  root-guarded).
- [ ] **#UC2 — Remote `~user` is not expanded; `shell_escape` makes a remote
  `~` literal.** `common.rs:84-90` (`expand_remote_path`) only rewrites `~/` →
  PUBDIR for remote targets; a remote `~user/path` is passed through unchanged
  and then single-quoted by `shell_escape` (`common.rs:383,416`), so the remote
  shell cannot expand it either. POSIX OPERANDS describes `~user` on the remote
  system. Fix: resolve `~user` against the remote (or document the limitation);
  note that single-quoting deliberately defeats remote tilde/glob expansion.
- [ ] **#G4 — Newline-in-filename is not rejected (FUTURE DIRECTIONS).** Neither
  `uucp` nor `uux` rejects a destination pathname containing an encoded
  `<newline>`. POSIX FUTURE DIRECTIONS only *encourages* this today (Austin Group
  Defect 251), so it is optional — track only.

## Cross-cutting findings (`uucp/common.rs` + shared patterns)

- [x] **#G1 — hardcoded-English diagnostics** (above). ✓ Phase 1.
- [x] **#G2 — `.unwrap()` on locale setup** (above). ✓ Phase 1.
- [x] **#G3 — `$USER`-based identity / ownership** (above). ✓ Phase 2.
- [x] **`shell_escape` is correct and is used on every value interpolated into an
  `ssh`/remote shell command** — `common.rs:26-39`, exercised at
  `common.rs:368,383,416,482-487` and `uux.rs:265,307,374`. Unit tests cover
  quotes, `$HOME`, backticks, `$(id)`, `a;rm -rf /`. This is the security-load-
  bearing helper and it CONFORMS (injection-safe). No action.
- [x] **SSH transport via `ssh -T -o BatchMode=yes`** — `common.rs:357-461`. The
  POSIX "protocol for transfer of files is unspecified," so substituting SSH for
  the `g`/`t` protocols is conformant. No action. (Accepted design.)
- [ ] **Helper shell-outs to `hostname` / `getent` / `mail`.** `common.rs:129,146,
  465`. `is_local_system` runs `hostname` on every call and, if `hostname` is
  absent from `PATH`, returns `false` — treating the local host as remote and
  attempting SSH. Minor robustness; could use libc `gethostname`/`getpwnam`.

## `uucp`

**Spec:** `uucp [-cCdfjmr] [-n user] source-file... destination-file`

### Options
| Opt | Status | Notes (file:line) |
|---|---|---|
| `-c` | DIVERGES (by design) | `uucp.rs:32`. Parsed but **inert** — there is no spool-staging step in immediate SSH mode, so "do not copy to spool" has no observable effect. POSIX-mandated to *accept*; behavior is moot without a deferred-transfer spool. Accepted. |
| `-C` | DIVERGES (by design) | `uucp.rs:35`. Inert for the same reason — "force copy to spool" cannot matter without deferred transfer. Accepted. |
| `-d` | CONFORMS | `uucp.rs:38`, default; `copy_local`/`ssh_*` create intermediate dirs (`uucp.rs:242-245`, `common.rs:365-375`). |
| `-f` | CONFORMS | `uucp.rs:41`; suppresses dir creation (`create_dirs=false`). Verified. |
| `-j` | PARTIAL | (#UC1) `uucp.rs:44,212-225`; prints the queued job ID under `-r`, but a throwaway ID for immediate transfers. |
| `-m` | CONFORMS | `uucp.rs:47,228-235`; mails the requester via `mail` on success. |
| `-n user` | CONFORMS (remote only) | `uucp.rs:50,196-202`; notifies the remote user via remote `mail` when the destination is remote. (Local-dest `-n` is a no-op, which matches the "remote system" wording.) |
| `-r` | DIVERGES (by design) | `uucp.rs:53,153-166,207-216`; creates a `J.*` spool record but **no daemon ever drains the queue**, so the file is never transferred. Accepted as a consequence of the no-`uucico` design (see Architecture divergences). |

### Operands / path handling
- [x] `source-file... destination-file`, ≥2 operands required — `uucp.rs:56`. Verified (missing operand → exit 2).
- [x] `system!path` parsed; empty system = local — `common.rs:73-81`.
- [x] Destination-is-directory rule (multiple sources or trailing `/` ⇒ append source basename) — `uucp.rs:96,138-151`. Matches "if the destination-file is a directory, the last part of the source-file name shall be used."
- [x] `~/dest` → PUBDIR (local and remote) — `common.rs:84-121`.
- [ ] **`~user` remote expansion** (#UC2) — only local `~user` is resolved (via `getent`).
- [x] Multi-hop route `a!b!path` → diagnostic + exit 1 — `uucp.rs:80-83,113-116`. **DIVERGES (by design):** POSIX says "an attempt is made to send the file via the specified route"; multi-hop store-and-forward routing is obsolete and deliberately unsupported. Accepted.
- [x] Remote wildcard `?`/`*`/`[...]` → warning, passed literally — `uucp.rs:86-93,120-128`. **DIVERGES (by design):** remote pattern expansion is unsupported (local globbing still happens in the invoking shell before `uucp` sees argv). Accepted.

### STDIN / STDOUT / STDERR / exit
- [x] STDIN not used — no `stdin` read in `uucp.rs`.
- [x] STDOUT carries only the `-j` job ID — `uucp.rs:213,224`. (The spec's "STDOUT: Not used" is overridden by the `-j` option text.)
- [x] Diagnostics to stderr — `uucp.rs:81,89,114,190,209`.
- [x] 0 success / >0 error; per-source failure aborts with exit 1 — `uucp.rs:189-193,218-220`.
- [ ] Diagnostics hardcoded English (#G1).

## `uux`

**Spec:** `uux [-jnp] command-string` (and a `-` operand per STDIN)

### Options / operands
| Opt | Status | Notes (file:line) |
|---|---|---|
| `-j` | PARTIAL | (#UC1) `uux.rs:31,110-114`; writes a job ID in the mandated `"%s\n"` form, but the ID is never tied to a persistent job. |
| `-n` | CONFORMS | `uux.rs:34,124-130`; suppresses the failure mail. |
| `-p` | CONFORMS | `uux.rs:37,52-61,98-107`; makes stdin the command's stdin. |
| `-` operand | CONFORMS | `uux.rs:56-57`; equivalent to `-p` per the STDIN section. Verified. |
| `command-string` | CONFORMS | `uux.rs:40`; exactly one non-option operand required (a quoted string), else "too many arguments". |

### EXTENDED DESCRIPTION (the core semantics)
- [x] First token's `system!` prefix selects the execution host; the rest run there — `uux.rs:160-161,308,343`. The whole string is run via `sh -c` on the exec host, so a pipeline's later stages run on that host as required.
- [x] **Redirection `>>`, `<<`, `>|`, `>&` rejected with a diagnostic + non-zero exit** — `uux.rs:79-86`. CONFORMS (spec "shall"). Verified all four.
- [x] No pathname expansion by `uux` (a literal `*.c` is passed through) — `uux.rs` never globs. CONFORMS (spec "shall not").
- [x] No alias substitution — runs via `sh -c` of an explicit command. CONFORMS.
- [x] Input files from other systems are fetched into a per-job work dir and referenced by basename — `uux.rs:217-304`. Matches "all files required... put into this directory" + the unique-basename caveat (EXAMPLE 2's intentional collision is the application's responsibility).
- [x] **Cross-system output-file routing** (#UX1, Major) — ✓ Phase 3: null-system⇒local + `ssh_fetch_file`/`ssh_send_file` delivery; third system is a hard error.
- [x] Command stdout is discarded when not redirected to a file — `uux.rs:391-392`. **DIVERGES (by design):** traditional `uux` returns command output by mail; POSIX leaves un-redirected output unspecified, so discarding is acceptable minimal. Accepted.
- [x] `-n` suppresses the failure-notification mail; otherwise mail is sent on failure — `uux.rs:124-130`. CONFORMS (the notification is best-effort local `mail`).

### STDIN / STDOUT / STDERR / exit
- [x] STDIN used only with `-`/`-p` — `uux.rs:98-107`.
- [x] STDOUT used only for the `-j` ID, `"%s\n"` — `uux.rs:113`.
- [x] Command stderr is forwarded to our stderr; our own diagnostics to stderr — `uux.rs:383-384,92`.
- [x] 0 success / >0 error (non-zero command exit ⇒ exit 1) — `uux.rs:119-133,387-389`.
- [ ] Diagnostics hardcoded English (#G1).

## `uustat`

**Spec:** `uustat [-q|-k jobid|-r jobid]` / `uustat [-s system] [-u user]`

### Options
| Opt | Status | Notes (file:line) |
|---|---|---|
| (none) | CONFORMS | `uustat.rs:111-129`; lists the current user's jobs. |
| `-q` | CONFORMS | `uustat.rs:57-66`; per-system queued-job counts. |
| `-k jobid` | CONFORMS | `uustat.rs:67-88`; ownership-checked delete. |
| `-r jobid` | CONFORMS | `uustat.rs:89-110`; touches the job files (mtime bump). |
| `-s system` | CONFORMS | `uustat.rs:37,123`. |
| `-u user` | CONFORMS | `uustat.rs:40,114-123`. |
- [x] Synopsis groups enforced via clap `conflicts_with_all` — `uustat.rs:28-41` (e.g. `-q` excludes `-k/-r/-s/-u`). Verified (`test_uustat_conflicting_options`).

### STDOUT / exit / ownership
- [x] Per-job output includes ≥ job ID, user, remote system (`jobid\tuser\tsystem`) — `uustat.rs:127`. CONFORMS (format unspecified; required fields present).
- [x] 0 success / >0 error; missing job under `-k`/`-r` → exit 1 — `uustat.rs:84-87,106-109`.
- [x] Ownership check via `$USER` (#G3). ✓ Phase 2: real-login + root.
- [ ] Diagnostics hardcoded English (#G1).
- Note: because immediate `uucp`/`uux` transfers create no persistent job, the default listing and `-q` are usually empty in practice — only `uucp -r` populates the spool, and those jobs never run (Architecture divergences). This is consistent and not a defect, but means `uustat` is mostly vestigial in the minimal design.

## Accepted architecture divergences (DIVERGES by design — no action)

These follow directly from "minimal SSH UUCP, no `uucico`/`uuxqt` daemon, no
store-and-forward." They are documented here so a future reader does not re-file
them as bugs.

- [x] **No spool daemon ⇒ `uucp -r` queues but never transfers.** `uucp.rs:153-166`,
  `common.rs:185-197`. A `J.*` record is written and is visible to `uustat`, but
  nothing drains the queue. POSIX assumes a background agent (`uucico`); the
  minimal design has none. (If "queue but never deliver" is considered too
  surprising, the alternative minimal choice would be to make `-r` a documented
  no-op or to drop the spool entirely — see Minimalism below.)
- [x] **`-c` / `-C` are inert** (no spool-staging step in immediate mode).
- [x] **Multi-hop routing (`a!b!path`) rejected** rather than forwarded.
- [x] **Remote wildcard/pattern expansion unsupported** (local shell globbing still applies).
- [x] **`uux` command stdout discarded** when not redirected (vs traditional mail-back).
- [x] **SSH replaces the `g`/`f`/`t`/`e`/`i` protocols and the `Systems`/`L.sys`
  config + handshake** — the entire legacy transport layer is intentionally absent.

## Minimalism / trim assessment

Per the "no more than POSIX requires" directive:

- [x] **CLI surface is already minimal.** `uucp` exposes exactly `-cCdfjmr -n`,
  `uux` exactly `-jnp` + `-`, `uustat` exactly `-q -k -r -s -u` — every one is
  POSIX-mandated. **There are no non-POSIX options to remove.**
- [ ] **`UUCP_SPOOL` environment variable is a non-POSIX extension.** `common.rs:43`.
  Harmless and useful (it is what the integration tests use to redirect the
  spool); keep it, but note it is an extension, not POSIX.
- [x] **The spool/`Job` machinery (`common.rs:155-355`) is the minimal substrate
  `uustat` needs** to report/kill/rejuvenate jobs, so it is not gold-plating per
  se — but it exists only to serve `uucp -r`, which never delivers. If the
  project decides `-r` should not pretend to queue, the spool, `Job`, and the
  `uustat` job-store could all shrink dramatically; that is a product decision,
  not a conformance fix. No non-essential helpers were found beyond this.

## Test coverage signal

Existing tests cover the golden paths well (local copy, dir handling, `-f`,
multi-source, `-c` accepted, redirection rejection, `-p`/`-` stdin, `uustat`
list/kill/rejuvenate/filter/conflicts). Gaps that map to findings:

- [x] No test asserts `uux` cross-system output routing or the `>!file`
  (null-system ⇒ local) semantics (#UX1). ✓ Phase 3: unit tests added.
- [ ] No test asserts `LC_MESSAGES` affects diagnostics (#G1) — expected to fail
  until diagnostics are `gettext()`-wrapped.
- [ ] No test pins the `-j` job-ID semantics for immediate vs `-r` transfers (#UC1).
- [x] No test exercises `-k`/`-r` ownership enforcement across users (#G3). ✓ Phase 2.
- [ ] No test for remote `~user` expansion (#UC2).

## Suggested PR groupings

- **PR A — "uux cross-system output routing"** (#UX1): fix the null-system⇒local
  mapping and implement fetch-and-deliver of the produced output file. The one
  Major item; makes POSIX `uux` EXAMPLE 3 work.
- **PR B — "i18n + robustness"** (#G1, #G2): route diagnostics through
  `gettext()`; replace locale-setup `.unwrap()` with `.ok()`.
- **PR C — "identity correctness"** (#G3): real-login resolution + `getuid()==0`
  for the `uustat` ownership guard.
- **PR D — "job-ID & tilde semantics"** (#UC1, #UC2): only emit a usable `-j` ID
  when a job is persisted; resolve (or document) remote `~user`.
- **PR E — "product decision: spool/`-r`"**: decide whether `uucp -r` should keep
  pretending to queue (current), become a documented no-op, or gain a real
  drain step. Drives whether the spool/`Job`/`uustat` machinery stays or shrinks.

## Notes

- No Critical defects; no crashes/hangs/data-loss observed on the golden path.
- The CLI is already POSIX-minimal — the audit's actionable list is short by
  design, which is the intended outcome for a deliberately minimal, dead-protocol
  implementation.
- `shell_escape` is the security-critical helper and is sound; SSH command
  construction consistently quotes interpolated values.
</content>
