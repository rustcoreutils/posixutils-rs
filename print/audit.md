# POSIX.1-2024 Conformance Audit — `print/` utilities

This file collects per-utility POSIX conformance audits for the printing
utilities crate. Each audit follows the playbook in `audits.md`.

The `print/` crate ships a single POSIX utility, **`lp`**. (POSIX `pr` and
`printf` live in `text/`, not here.)

---

## `lp`

**Implementation:** `print/lp.rs` (243 lines)
**Tests:** `print/tests/lp/mod.rs` (567 lines, 16 `#[test]`s)
**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 §3, pp. 3121–3125
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/lp.md`
**Date:** 2026-06-17

### Architectural note (read first)

This `lp` is a thin **IPP (Internet Printing Protocol) client**: the
destination is an `ipp://` URI and the print data is uploaded synchronously to
an IPP server via the `ipp` crate (`Cargo.toml`: blocking client, no TLS, for
minimal deps). There is **no CUPS/spool integration, no `/dev/lp` path, and no
system-default-printer concept.** POSIX deliberately leaves the destination
format, queuing mechanism, and output device *unspecified* ("Destination names
vary between systems", 103084–103085; "the term *unspecified* is used … in
lieu of *implementation-defined*", 103212–103215), so an IPP-URI destination
model is **permitted by the letter of the spec**. This audit therefore treats
the IPP-transport decision the way the `uucp` audit treats SSH: as an accepted
architecture divergence, with the spec *intent* gaps it creates (#3, #4)
recorded as Major rather than Critical. Adjust severities if the project later
decides bare-name destinations / a default printer are required.

### TL;DR

The POSIX option/operand *surface* is complete and the common mechanics are
correct: `-` / no-operand → stdin, multiple files printed in order as integral
wholes, `-d` > `LPDEST` > `PRINTER` precedence, request-ID-to-stdout (gated by
`-s`), diagnostics-to-stderr, and — notably better than several other crates —
a fully wired `setlocale` + `textdomain` + `gettext` diagnostic surface.
The gaps are behavioral, not structural: **`-m` and `-w` are parsed-but-ignored
no-ops** (`-c` is also ignored but is a *conforming* no-op — see #C-note);
the **destination must be an `ipp://` URI**, so historical
`LPDEST=myprinter` / `PRINTER=myprinter` values fail; and there is **no
system-default destination**, so bare `lp file` always exits 1. No Critical
defects; no crashes, hangs, or data loss.

### Priority issues

#### Critical

- None. (No crash/hang/data-loss path; the destination-model gaps are recorded
  as Major per the Architectural note above.)

#### Major

- [ ] **#1 — `-m` (mail after printing) is a parsed-but-ignored no-op.** `print/lp.rs:34-35`. The `_mail` field is declared and never consulted anywhere (`grep -n '_mail' lp.rs` → only line 35). POSIX OPTIONS 103090–103091: "−m  Send mail (see mailx) after the files have been printed." Fix: after a job is accepted, invoke `mailx` to notify the user — or, if intentionally unsupported, document it (and stop advertising the option as functional).
- [ ] **#2 — `-w` (write to terminal after printing) is a parsed-but-ignored no-op.** `print/lp.rs:40-41`. The `_write` field is never consulted (`grep -n '_write' lp.rs` → only line 41). POSIX 103099–103101: "−w  Write a message on the user's terminal after the files have been printed." Fix: write the completion message to the user's terminal (`/dev/tty` / `write`-style), or document as unsupported.
- [x] ~~**#3 — No system-default destination; bare `lp` / `lp file` with no `-d`/`LPDEST`/`PRINTER` always exits 1.**~~ **WON'T-FIX (Phase 1).** `print/lp.rs` `get_destination` still returns `Err("no destination specified")` when `-d`/`LPDEST`/`PRINTER` are all absent. This is **conforming** per DESCRIPTION 103059–103061 ("If such a device is not available to the application, or if the system provides no such device, the lp utility shall exit with a non-zero exit status") — with no configured destination there is no available device. Resolving a *system default* would require querying a localhost CUPS server (`CUPS-Get-Default`), an assumption the project declined in favor of the explicit destination model. The RATIONALE's "should always be able to execute" is a non-normative *should*; exiting non-zero with a clear diagnostic satisfies the normative text.
- [x] **#4 — Destination must be an `ipp://` URI; historical printer-*name* values are rejected.** ✓ **fixed (Phase 1).** `resolve_uri` (`print/lp.rs`) now uses an `ipp://…` value verbatim and resolves any other value as a bare printer name → `ipp://localhost/printers/<name>`, so the ordinary `PRINTER=myprinter` / `LPDEST=myprinter` / `-d myprinter` forms work against the local IPP server. Names containing control/whitespace/`/` characters are rejected with `invalid destination name`. The resolved URI is now echoed in the connection-error diagnostic. Tests: `lp_bare_name_resolves_to_localhost`, `lp_invalid_name_rejected`, and the reworked precedence tests assert the resolved-name appears in the diagnostic.

#### Minor

- [x] **#5 — Multi-file run aborts on the first error; later operands are not attempted.** ✓ **fixed (Phase 3).** `do_lp` now returns `Result<bool, String>`: destination/resolve failures stay fatal, but a per-file `read_input`/`send_print_job` error is reported to stderr, sets a `had_error` flag, and `continue`s to the next operand. `main` maps `Ok(true)` → non-zero exit. So `lp a b c` attempts every operand and still exits non-zero. Test: `lp_multifile_continues_on_error` (first operand unreadable, second `-` still attempted).
- [x] **#6 — `-n copies` (`u32`) is cast to `i32` and wraps negative for values > 2147483647.** ✓ **fixed (Phase 2).** The clap validator is now `range(1..=i64::from(i32::MAX))`, so values above `i32::MAX` are rejected with exit 2 before any `as i32` cast. Test: `lp_n_copies_overflow_rejected`.
- [ ] **#7 — `ipps://` (TLS) destinations are unsupported.** `print/lp.rs:78` accepts only the `ipp://` prefix; `Cargo.toml` drops TLS features for minimal deps. Encrypted print queues cannot be targeted. Fix: accept `ipps://` (needs the `ipp` TLS feature) or document the limitation.
- [x] **#8 — `-o` options without `=` are silently discarded.** ✓ **fixed (Phase 2).** The `split_once('=')` `else` branch now emits a gettext'd `lp: ignoring malformed -o option (expected name=value): <opt>` diagnostic to stderr before skipping. Test: `lp_o_malformed_warned`.
- [x] **#9 — Request ID degrades to `<dest>-0` when the IPP response omits `job-id`.** ✓ **fixed (Phase 2).** `send_print_job` now returns `Err("printer response missing job-id")` (non-zero exit) instead of `.unwrap_or(0)`, honoring the unique-request-ID mandate (103065). Not unit-tested (requires a non-conformant IPP server, which the harness lacks).
- [ ] **#10 — `NLSPATH` (XSI) is not explicitly honored.** `print/lp.rs` never reads `NLSPATH`; message-catalog lookup relies on `gettextrs`/`setlocale` defaults. Consistent with the rest of the tree; track for completeness.

### Detailed conformance matrix

#### SYNOPSIS / argv parsing
`lp [-c] [-d dest] [-n copies] [-msw] [-o option]... [-t title] [file...]`

- [x] All eight option letters present (`-c -d -n -m -s -w -o -t`) — `lp.rs:24-50`.
- [x] Bundled short flags (`-msw`) and `--` end-of-options handled — clap default (derive `Parser`, `lp.rs:22`).
- [x] XBD §12.2 utility-conventions conformance — delegated to clap.
- [x] `-` operand routes to stdin at its position in the file list — `lp.rs:90-92, 207-211`.

#### OPTIONS

| Opt | Status | Notes (file:line) |
|---|---|---|
| `-c` | CONFORMS¹ | `_copy` parsed, never consulted (`lp.rs:25-26`). ¹**#C-note:** `read_input` slurps the whole file into a `Vec<u8>` and `client.send` uploads it synchronously *before* lp exits (`lp.rs:88-100, 162-168`), so "further access to the input files is no longer required" the moment lp returns — the `-c` guarantee (103072–103079) holds on **every** invocation. A conforming no-op, distinct from #1/#2. |
| `-d dest` | PARTIAL | Precedence correct (`lp.rs:54-73`) but value must be `ipp://…` (#4). |
| `-n copies` | PARTIAL | Sets IPP `copies` when >1 (`lp.rs:136-141`); `range(1..)` rejects 0/negative; `u32→i32` overflow (#6). |
| `-m` | MISSING | (#1) `_mail` never consulted (`lp.rs:34-35`). |
| `-s` | CONFORMS | Suppresses only the request-ID line (`lp.rs:221-223`), not stderr diagnostics — matches 103098 + STDERR 103150. |
| `-w` | MISSING | (#2) `_write` never consulted (`lp.rs:40-41`). |
| `-o option` | CONFORMS | Repeatable (`ArgAction::Append`, `lp.rs:43`); `name=value` → typed IPP attribute (`lp.rs:145-160`). `-o` is spec-unspecified; malformed-option silence is #8. |
| `-t title` | CONFORMS | Maps to IPP `job-name`/`job_title` (`lp.rs:128-133`); falls back to the file's basename when `-t` absent — a reasonable default, not a divergence. |

#### OPERANDS / STDIN / INPUT FILES

- [x] `file` pathname operand; no operands → stdin — `lp.rs:199-203`.
- [x] `-` operand → stdin — `lp.rs:90-92, 207`.
- [x] Multiple operands processed in order, each as a **separate** IPP job → "each file … output as an integral whole, not interleaved" (103094–103095) — `lp.rs:206-224`.
- [x] STDIN used only when no operands or `-` (103109–103111) — `lp.rs:90-92, 199`.
- [x] Input files read as opaque bytes; spec's "shall be text files" (103113) is an application obligation, not enforced by lp — CONFORMS.

#### ENVIRONMENT VARIABLES

| Var | Status | Notes |
|---|---|---|
| `LANG` / `LC_ALL` / `LC_CTYPE` | CONFORMS | `setlocale(LC_ALL, "")` at `lp.rs:230`, before `Args::parse()` (`:234`). |
| `LC_MESSAGES` | CONFORMS | All diagnostics routed through `gettext()` (17 sites; e.g. `lp.rs:72, 79, 83, 168, 215`); `textdomain`/`bind_textdomain_codeset` at `:231-232`. |
| `LPDEST` | CONFORMS | Read when `-d` absent; empty treated as unset (`lp.rs:60-64`). |
| `PRINTER` | CONFORMS | Read when `-d` and `LPDEST` absent/empty (`lp.rs:66-70`). |
| `-d` > `LPDEST` > `PRINTER` precedence | CONFORMS | (103087–103089, 103137–103138) — `lp.rs:56-70`. |
| `LC_TIME` | N/A | (103128–103129) Governs banner date/time strings; no banner is rendered locally (the IPP server owns it) → nothing to localize. |
| `TZ` | N/A | (103140–103142) Same as `LC_TIME` — no local banner date/time. |
| `NLSPATH` (XSI) | MISSING | (#10) Not explicitly read. |

#### ASYNCHRONOUS EVENTS

- [x] Default (103143–103144) — non-interactive; no handlers required. `grep -nE 'SIGCONT|SIGWINCH|signal|libc::signal' lp.rs` → 0 matches (expected).

#### STDOUT / STDERR

- [x] Request ID written to **stdout** unless `-s` (103145–103148) — `lp.rs:221-223`, format `request id is <dest>-<job_id>\n` (`:110-113`); format is spec-unspecified, matches historical System V wording.
- [x] **stderr** used only for diagnostics (103149–103150) — every error path uses `eprintln!`/returns `Err` rendered at `lp.rs:239`; no informational stdout chatter beyond the request ID.

#### OUTPUT FILES / EXTENDED DESCRIPTION

- [x] OUTPUT FILES: None (103151–103152) — lp writes no local files.
- [x] EXTENDED DESCRIPTION: None (103153–103154) — N/A.

#### DESCRIPTION mandates

- [x] "copy the input files to an output destination" (103057) — full upload via IPP `print-job` (`lp.rs:122-168`).
- [x] "If such a device is not available … exit with a non-zero exit status" (103059–103061) — IPP send/connect failure → `Err` → exit 1 (`lp.rs:166-179`).
- [x] "exclusive access to the device" during writing (103062–103064) — delegated to the IPP server. N/A locally.
- [x] "associate a unique request ID with each request" (103065) — `<dest>-<job-id>` from the IPP response (`lp.rs:182-190`); degrades to `-0` on a non-conformant server (#9).
- [x] Banner page (103066–103068) — the IPP server's responsibility; `-o` can suppress per implementation. N/A locally.

#### EXIT STATUS / CONSEQUENCES OF ERRORS

- [x] 0 when all input processed (103157) — `do_lp` returns `Ok(())` → `ExitCode::SUCCESS` (`lp.rs:236-237`).
- [x] >0 on no-device / error (103158) — any `Err` → `ExitCode::FAILURE` (`lp.rs:238-241`).
- [ ] **First-error aborts remaining operands** (#5) — `lp.rs:215, 218`. Permitted (CONSEQUENCES = Default, 103159–103160) but worth a continue-and-accumulate policy.

#### Cross-cutting

- [x] **i18n** — `setlocale` + `textdomain` + `bind_textdomain_codeset` + `gettext`-wrapped diagnostics and clap help (`lp.rs:17, 23-50, 230-232`). The cleanest i18n wiring of the crates audited so far.
- [x] **Regex** — N/A (lp does no pattern matching).
- [x] **Signals** — N/A (Default).
- [ ] **Robustness** — `args.copies as i32` overflow (#6); `job-id` `unwrap_or(0)` (#9). No panics/`unwrap()` on external input otherwise (`read_input`/`send_print_job` return `Result`).

### Test coverage signal

16 tests cover the option surface, env-var precedence, `-`/stdin routing, and
error paths well (they assert "printer error" reached, i.e. options were
accepted, since no IPP server is present in CI). Gaps that map to findings:

- [ ] No test asserts `-m` actually sends mail (#1) — current test only checks `-m` is *accepted*.
- [ ] No test asserts `-w` actually writes to the terminal (#2) — same.
- [ ] No test exercises the no-destination *success* path / system default (#3).
- [ ] No test feeds a bare printer **name** (non-URI) and asserts the intended behavior (#4).
- [ ] No test covers multi-file partial-failure continuation (#5).
- [ ] No test covers `-n` > `i32::MAX` overflow (#6).
- [ ] No test covers a successful job (request-ID-to-stdout format, `-s` suppression on success) — all current tests stop at the connection failure.

### Suggested PR groupings

- **PR A — "Implement `-m` and `-w` completion notifications"**: #1, #2. Both fire after a successful job; share a "notify on completion" helper (mailx for `-m`, terminal write for `-w`).
- **PR B — "Default & name-based destinations"**: #3, #4. Resolve a bare destination name → configured `ipp://` URI; fall back to a system-default IPP printer when no destination is given. The two halves of making `lp file` and `lp` (bare) work as the RATIONALE intends.
- **PR C — "Per-file error continuation + exit-status accuracy"**: #5. Loop records failures, continues to siblings, exits non-zero at the end.
- **PR D — "Robustness & validation"**: #6 (`copies` ≤ `i32::MAX`), #9 (missing `job-id` is an error), #8 (diagnose malformed `-o`).
- **PR E — "Transport completeness"**: #7 (`ipps://`), #10 (`NLSPATH`). Lower priority; #7 reintroduces a TLS dep.
