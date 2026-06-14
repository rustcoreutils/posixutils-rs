# POSIX.1-2024 Conformance Audits — `sccs/` utilities

This file collects per-utility POSIX conformance audits for the SCCS
utilities crate. Each audit follows the playbook in `../audits.md`.

**Scope:** all ten POSIX.2024 SCCS utilities — `admin`, `delta`, `get`,
`prs`, `rmdel`, `sact`, `sccs` (front-end), `unget`, `val`, `what` — plus the
shared file-format core `plib/src/sccsfile.rs` that nine of them build on.

**Method:** full spec slice + full implementation read, then a **behavioral
sweep** diffing every utility against GNU CSSC 1.4.1
(`/usr/lib/x86_64-linux-gnu/cssc/*`, front-end `/usr/bin/sccs`) — byte-for-byte
on s-file contents, stdout, stderr, and exit status, cross-tested **both
directions** (our utility on a CSSC-authored s-file and vice-versa). All
Critical/Major findings below were re-verified by the auditor directly (grep +
re-run), not taken on the sub-agents' word.

**Reference slices:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/{admin,delta,get,prs,rmdel,sact,sccs,unget,val,what}.md`
**Date:** 2026-06-14

---

## Headline

The **shared core is genuinely interoperable with CSSC**: the checksum (`h`)
header, SID parser, delta-table format, delta-application engine
(`compute_applied_set`/`evaluate_body`), body weave (`^AI/^AD/^AE`), and p-file
format in `plib/src/sccsfile.rs` all round-trip byte-identically with CSSC in
both directions and pass `cssc val`. The golden paths — `admin -ifile s.x`,
`get`/`get -e`, `delta`, keyword expansion (all 19 `get` keywords verified),
multi-release/branch reconstruction — are correct.

The defects cluster in five themes: (1) one **data-corruption** bug —
encoded/binary (`e`-flag) bodies are emitted un-decoded; (2) **operand-form
gaps** — `-` (stdin list) and directory operands are implemented in
`prs`/`sact`/`unget`/`rmdel` but **missing** in `get`/`delta`/`admin`; (3)
**option-parsing divergences** from clap defaults (`admin -i`/`-t`/`-y` eat the
next operand; `val`/`prs` reject spec-mandated optional/empty arguments); (4)
**missing spec features** (`delta -m`/`-g`/stdin-comment, `get -c`/`-i`/`-x`/`-l`,
the `v`-flag MR check, the `No id keywords` warning, z-file locking, SIGINT
cleanup); and (5) **identity/locale** weaknesses (`$LOGNAME` instead of
`getpwuid`, `TZ`-blind timestamps, hardcoded-English diagnostics).

Nothing crashes the *process* except `what` on its single most important input
(any binary). No utility corrupts a CSSC-readable s-file (CSSC `val`/`get`/`prt`
accept our output everywhere except encoded files).

---

## Cross-cutting findings (`plib::sccsfile` + shared patterns)

These affect multiple utilities. Per-utility sections reference them by number.

### Critical

- [x] **#X1 — Encoded (`e`-flag) bodies are emitted un-decoded → garbage output.**
  ✓ fixed in Phase 3. Added `plib::sccsfile::uudecode_sccs`/`uuencode_sccs`
  (historical space-for-zero uuencode) and `SccsFile::is_encoded()`. `get` now
  uudecodes `e`-flag bodies and writes raw bytes (no keyword expansion); `admin`
  detects binary / no-trailing-newline input and stores it uuencoded with
  `^Af e 1`. Verified **byte-identical to CSSC** both directions: we read CSSC
  binary/no-eol files correctly, our encoded files round-trip and pass
  `cssc get`/`cssc val`. (Surfaces as `get` #G1.)

### Major

- [x] **#X2 — No body-reweave routine; `rmdel` cannot prune a removed delta's text.**
  ✓ fixed in Phase 2. Added `SccsFile::remove_delta(serial)`: marks the entry
  `Removed` and reweaves the body — drops the delta's own `^AI s … ^AE s` block
  in full, and removes its `^AD s … ^AE s` delete markers while restoring the
  wrapped text. Verified **byte-identical body to CSSC** and `cssc val` accepts
  our rmdel'd file. (Surfaces as `rmdel` #R1.)

- [x] **#X3 — `:FL:` flag listing emits Rust `Debug`, not canonical flag names.**
  ✓ fixed in Phase 2. Added `SccsFlag::prs_fl_line()` (canonical `name<tab>value`
  per CSSC, `None` for the `e` flag); `prs::format_flags` now uses it. Verified
  `branch` / `module\tmod` / `type\tty` / `csect name\tQ` / `validate MRs\t`.
  (Surfaces as `prs` #P3. Flag *order* follows storage order, which is
  spec-permitted; the missing trailing newline is the separate #P1.)

- [x] **#X4 — Login name taken from `$USER`/`$LOGNAME`, not `getpwuid(getuid())`.**
  ✓ fixed in Phase 1. Added `plib::sccsfile::real_login_name()` (real-uid
  `getpwuid` via `plib::user::get_by_uid`, env only as fallback); all six sites
  (`get.rs`, `delta.rs`, `admin.rs`, `unget.rs`, `rmdel.rs`, `sccs.rs -U`) now
  use it. Verified: committer resolves to the passwd login even with
  `USER=evil`/`LOGNAME` spoofed.

- [x] **#X5 — `SccsDateTime::now()` ignores `TZ`; timestamps are always UTC.**
  ✓ fixed in Phase 1. `now()` now converts via libc `localtime_r` (honors
  `TZ`). Verified `TZ=UTC0` vs `TZ=HST10` record times 10h apart.

- [x] **#X6 — `No id keywords` warning never emitted by `get` or `admin`.**
  ✓ Phases 4 & 6: both `get` and `admin` now emit
  `<util>: warning: <sfile>: No id keywords.` for keyword-less non-encoded
  content (matches cssc), and `get` escalates to a fatal error when the `i`
  flag is set.

- [x] **#X7 — `-` (stdin list) and directory operands missing in `get`/`delta`/`admin`.**
  ✓ Phases 4–6: added `plib::sccsfile::paths::expand_operands` and wired it into
  `get`, `delta`, and `admin` (it was already present in prs/sact/unget/rmdel).
  (Surfaces as #G5/#G6, #D6/#D7, #A3/#A4.)

### Minor

- [ ] **#X8 — z-file locking is never taken.** `plib::sccsfile::paths::zfile_from_sfile`
  exists but no utility calls it. `get -e`, `admin`, `delta`, `rmdel` all
  mutate s-files / p-files with no lock, contravening the project's race-free
  principle. Spec marks the z-file "may," so not a hard violation, but two
  concurrent writers can interleave. Fix: create/remove the z-file around every
  mutation.

- [ ] **#X9 — Hardcoded-English diagnostics; `LC_MESSAGES`/`NLSPATH` inert.**
  Every utility wires `setlocale(LC_ALL,"")` + `textdomain` near `main`, but the
  runtime diagnostic strings are raw `eprintln!`/`println!` literals, not routed
  through `gettext()`. So locale is initialized but never affects diagnostics.
  Pervasive across `get.rs`, `delta.rs`, `admin.rs`, `prs.rs`, `rmdel.rs`,
  `unget.rs`, `val.rs`, `sccs.rs`. Fix: wrap diagnostic strings in `gettext()`.

- [ ] **#X10 — No SIGINT cleanup of the temp x-file in `delta`/`admin`.** Both
  write `x.<name>` then atomically rename; an interrupt mid-write orphans it.
  `delta`'s spec ASYNCHRONOUS EVENTS makes SIGINT cleanup a **shall** (Major for
  delta, #D5); `admin`'s is "Default" (Minor, #A6). No signal handlers exist.

---

## `admin`

**Implementation:** `sccs/admin.rs` (485) + `plib/src/sccsfile.rs`
**Spec:** POSIX.1-2024, Vol. 3 §3 — `admin.md`
**Date:** 2026-06-14 (behaviorally verified vs CSSC)

### TL;DR
The checksum machinery (`-h` validate, `-z` recompute) and create/modify paths
are solid and byte-interoperable with CSSC both directions. The headline defect
is argv handling: `-i`/`-t`/`-y` are wired with clap `num_args=0..=1`, which
greedily eats the following operand, so the spec forms `admin -i s.foo` (create
from stdin) and `admin -t s.foo` (remove descriptive text) **fail with exit 2**.
Invalid `-f` flag letters are accepted and written as malformed `^Af` records.
Directory/`-` operands and the `No id keywords` warning are missing.

### Priority issues

#### Critical
- [x] **#A1 — Optional-arg options `-i`/`-t`/`-y` consume the following operand.**
  ✓ Phase 6: argv is pre-scanned (`extract_attached_opts`) so these take an
  attached value only; a bare `-i`/`-t`/`-y` no longer swallows the operand.
  Verified `printf 'x' | admin -i s.new` creates `s.new` (exit 0) and attached
  `-iin.txt` still works.

#### Major
- [x] **#A2 — Invalid `-f` flag letters accepted, written as malformed `^Af`.**
  ✓ Phase 6: `parse_flag` rejects any letter outside `{b,c,d,e,f,i,j,l,m,n,q,t,v}`
  (`admin: Unrecognized flag 'Z'`, exit 1 — matches cssc). plib keeps `Unknown`
  only for lossless reads of existing files.
- [x] **#A3 — Directory operand unsupported.** ✓ Phase 6 via `paths::expand_operands`.
- [x] **#A4 — `-` stdin-list operand unsupported.** ✓ Phase 6 via `paths::expand_operands`.

#### Minor
- [x] **#A5 — Default `-y` committer login from `$USER`, falls back to `"unknown"`.**
  ✓ fixed in Phase 1 via `real_login_name()` (#X4). `admin.rs:69`.
- [ ] **#A6 — No SIGINT handler; x-file orphaned on interrupt.** (#X10) Spec
  ASYNCHRONOUS EVENTS is "Default," so Minor; the atomic-rename design still
  leaks `x.file`.
- [x] **#A7 — `-m mrlist` not consulted on create; `v`-flag-requires-MR not enforced.**
  ✓ Phase 6: `-m` records MRs; `v` set with no `-m` errors (`MR number(s) must
  be supplied.`); `-m` without `v` errors. Matches cssc.
- [x] **#A8 — `c`/`f` ceiling/floor not range-checked (spec cap 9999).**
  ✓ Phase 6: `parse_flag` rejects values > 9999.
- [x] **#A9 — `No id keywords` warning not emitted on `admin -i`.** ✓ Phase 6
  (#X6): emits `admin: warning: <sfile>: No id keywords.` for non-encoded
  keyword-less content.

### Detailed conformance matrix

#### SYNOPSIS / argv
- [ ] **`-i`/`-t`/`-y` separate-arg consumption DIVERGES** (#A1) — `admin.rs:35,41,44`.
- [x] `-n`, `-h`, `-z`, `-r`, `-f`, `-d`, `-a`, `-e` parsed — `admin.rs:32-63`.
- [x] Attached short-option values (`-iinput.txt`, `-r2.1`) work — verified.

#### OPTIONS / `-f` flag letters
| Opt | Status | Notes (file:line) |
|---|---|---|
| `-n` | CONFORMS | `admin.rs:382,398`; empty s-file matches CSSC structure. |
| `-i[name]` | DIVERGES | (#A1) separate-arg broken; attached/stdin work (`admin.rs:395`). |
| `-r SID` | CONFORMS+ | `admin.rs:127`; trunk-only SID enforced (stricter than CSSC's warn). |
| `-t[name]` | DIVERGES | (#A1) bare `-t` (remove text) unreachable; attached sets text. |
| `-f b/c/f/d/i/j/l/m/n/q/t/v` | CONFORMS | `plib/src/sccsfile.rs:534-579`; `c`/`f` unranged (#A8); `v`-prog wiring absent (#A7). |
| invalid `-fZ`,`-f4` | DIVERGES | (#A2) accepted as `Unknown`, exit 0. |
| `-d flag` | CONFORMS | `admin.rs:279`; delete-then-`-h` verified. |
| `-a`/`-e login` | CONFORMS | `admin.rs:292,299`; dedup + `!`-prefix preserved. |
| `-m mrlist` | MISSING | (#A7). |
| `-y[comment]` | PARTIAL | default comment built `admin.rs:144`; bare-`-y` hits #A1; login #A5. |
| `-h` | CONFORMS | `admin.rs:210`; checksum+structure, matches CSSC exit/diag. |
| `-z` | CONFORMS | `admin.rs:242`; recompute + preserve perms; CSSC accepts result. |

#### OPERANDS / STDIN
- [x] `s.`-prefix enforced — `admin.rs:356`.
- [ ] **directory operand MISSING (#A3); `-` operand MISSING (#A4).**
- [x] `-i` (no attached arg) reads stdin — `admin.rs:397`.

#### ENVIRONMENT / ASYNC / OUTPUT / STDOUT-STDERR / EXIT
- [x] `setlocale`+`textdomain` — `admin.rs:316`; diagnostics hardcoded English (#X9).
- [x] s-file 0444; x-file→s-file atomic rename — `admin.rs:195-205`; modify preserves perms.
- [ ] **z-file lock never created** (#X8); **no SIGINT cleanup** (#A6).
- [x] STDOUT unused; diagnostics to stderr; 0/>0 exit, per-file continue — `admin.rs:352,362,484`.

### Test coverage gaps
- [ ] `admin -i newfile` / `admin -t file` separate-arg form (#A1).
- [ ] Invalid `-fZ`/`-f4` rejected (#A2).
- [ ] Directory operand (#A3); `-` operand (#A4).
- [ ] Cross-validation (CSSC `val` on our s-file; our `-h` on CSSC s-file).
- [ ] Deliberate-corruption `-h`/`-z`; `-a`/`-e`/`-d` round-trip; `No id keywords` warning.

---

## `delta`

**Implementation:** `sccs/delta.rs` (536) + `plib/src/sccsfile.rs`
**Spec:** POSIX.1-2024, Vol. 3 §3 — `delta.md`
**Date:** 2026-06-14 (behaviorally verified vs CSSC)

### TL;DR
The weave/diff engine is excellent — for a single pending edit with `-y`, our
output is **byte-identical to CSSC** both directions and passes `cssc val`. But
three spec-mandated paths are absent: `-m` and `-g` are not registered with clap
(exit 2), and with `-y` omitted delta records **no comment** instead of reading
it from stdin / prompting. The `v`-flag MR-validation `shall` is unimplemented
(delta silently commits where CSSC errors), and there is no SIGINT cleanup.

### Priority issues

#### Critical
- [x] **#D1 — `-m mrlist` missing entirely.** ✓ Phase 5: `-m` records MRs as
  `^Am` (verified `delta -m'bug42'` → `^Am bug42`).
- [x] **#D2 — `v` flag (MR validation) not enforced.** ✓ Phase 5: `v`-flagged
  file with no `-m` aborts (`delta: s.f: MR number(s) must be supplied.`, exit 1,
  no delta); a valued `v` program is run and a non-zero exit aborts.
- [x] **#D3 — Comment never read from stdin when `-y` omitted.** ✓ Phase 5:
  prompts `comments? ` on a tty, else reads the comment from stdin (verified
  `echo … | delta` records `^Ac`).

#### Major
- [x] **#D4 — `-g list` (ignore deltas) missing.** ✓ Phase 5: `-g` records
  ignored serials as `^Ag` (SID or serial accepted).
- [ ] **#D5 — No SIGINT handler; x-file not cleaned on interrupt.** (#X10) Spec
  ASYNCHRONOUS EVENTS makes this a `shall` for delta. `delta.rs:494-500` writes
  `x.<name>` before rename. Fix: install a handler that unlinks it, exit non-zero.
- [x] **#D6 — `-` operand not read as stdin list.** ✓ Phase 5 via
  `paths::expand_operands` (`-y` enforced when the operand is `-`).
- [x] **#D7 — Directory operand not expanded.** ✓ Phase 5 via `paths::expand_operands`.

#### Minor
- [x] **#D8 — `-p` uses a non-`diff(1)` format.** ✓ Phase 5: now emits
  `diff`-normal hunks (`2c2`/`<`/`---`/`>`), byte-identical to cssc. Also fixed a
  pre-existing EOF-append panic in `compute_diff`.
- [ ] **#D9 — `-r SID` matches old OR new SID; CSSC matches new only.**
  `delta.rs:84-94`. More permissive; can pick the wrong entry when two edits
  share an old SID.
- [ ] **#D10 — 512-byte comment / 99 999-line caps unenforced; no `^A`-first-line guard.**
- [ ] **#D11 — Diagnostics hardcoded English (#X9); `now()` ignores TZ (#X5).**

### Detailed conformance matrix

#### SYNOPSIS / OPTIONS
| Opt | Status | Notes |
|---|---|---|
| `-y[comment]` | PARTIAL | attached works (`delta.rs:31`); absent-arg stdin/prompt unimpl (#D3). |
| `-r SID` | CONFORMS | `delta.rs:28,84,417` (with #D9 nuance). |
| `-s` | CONFORMS | `delta.rs:40,479`. |
| `-n` | CONFORMS | `delta.rs:34,506`; retains g-file (verified). |
| `-p` | DIVERGES | (#D8) non-diff format. |
| `-m mrlist` | MISSING | (#D1). |
| `-g list` | MISSING | (#D4). |

#### OPERANDS / STDIN
- [ ] **`-` list MISSING (#D6); directory MISSING (#D7); stdin comment/MR PARTIAL (#D3).**

#### ENVIRONMENT / ASYNC / STDOUT-STDERR / EXIT
- [x] `setlocale` — `delta.rs:516`; `LC_MESSAGES` partial (#D11); `TZ` ignored (#X5).
- [ ] **SIGINT MISSING (#D5).**
- [x] Activity report `"%s\n%d inserted\n%d deleted\n%d unchanged\n"` — `delta.rs:480`; matches CSSC.
- [ ] **`MRs?`/`comments?` prompts MISSING (#D1/#D3).**
- [x] Diagnostics to stderr; 0/>0 exit; no-pending → exit 1 (matches CSSC).

#### EXTENDED DESCRIPTION (delta-table + weave)
- [x] Dt line, `^As` stats, `^Ac`, `^Ae` — `plib/src/sccsfile.rs:730-774`; byte-identical to CSSC.
- [x] Body weave `^AI/^AD/^AE <serial>` + insert/delete/unchanged counts — `delta.rs:252-353`; verified byte-identical.
- [ ] **`^Am` path unreachable (#D1); `v`-flag validation absent (#D2).**

### Test coverage gaps
- [ ] `-m`/MR + `v`-flag required-MR error; `-g` ignore-list; stdin comment (piped + tty); `-p` format; `-`/directory operands; SIGINT cleanup; `-r` among multiple edits; checked-in CSSC interop fixture.

---

## `get`

**Implementation:** `sccs/get.rs` (558) + `plib/src/sccsfile.rs`
**Spec:** POSIX.1-2024, Vol. 3 §3 — `get.md`
**Date:** 2026-06-14 (behaviorally verified vs CSSC)

### TL;DR
The golden path is strong: all 19 ID keywords, trunk/branch/multi-release
reconstruction, `-r` partial SIDs, `-p/-k/-m/-n/-g/-e/-s`, the p-file format, and
g-file perms are **byte-identical to CSSC**. The serious gaps are coverage:
seven spec options (`-c -i -x -l -L -t`) are entirely absent (clap exit 2), `-`
and directory operands are unimplemented, the `No id keywords` warning is
missing, and the mandated z-file lock is never created. The one data bug is
shared-core #X1 (encoded bodies).

### Priority issues

#### Critical
- [x] **#G1 — CSSC encoded (`e`-flag) bodies emitted as uuencoded garbage.**
  ✓ fixed in Phase 3 (#X1): `get` uudecodes `e`-flag bodies to raw bytes.

#### Major
- [x] **#G2 — `-c cutoff`.** ✓ Phase 4: `-c` with the 2-digit POSIX year pivot;
  deltas newer than the cutoff excluded. Matches cssc.
- [x] **#G3 — `-i list` / `-x list`.** ✓ Phase 4: force include/exclude with the
  `Included:`/`Excluded:` notation; recorded in the p-file (attached `-i`/`-x`
  form; plib parser now accepts both forms).
- [x] **#G4 — `-l` / `-L` (l-file / delta summary).** ✓ Phase 4; l-file is
  **byte-identical to cssc**.
- [x] **#G5 — `-` stdin-list operand.** ✓ Phase 4 via `paths::expand_operands`.
- [x] **#G6 — Directory operand.** ✓ Phase 4 via `paths::expand_operands`.
- [x] **#G7 — `-t` (top delta in release).** ✓ Phase 4; matches cssc.
- [ ] **#G8 — z-file lock never created.** (#X8) `get -e` takes no lock.
- [x] **#G9 — `No id keywords` warning.** ✓ Phase 4: emits
  `get: warning: <sfile>: No id keywords.` (matches cssc), escalates to fatal
  when the `i` flag is set, silent when a keyword is present.

#### Minor
- [ ] **#G10 — p-file written 0644; spec implies owner-writable-only (0600).**
  `get.rs:298`. CSSC also leaves it group/other-readable — benign.
- [ ] **#G11 — Hardcoded-English diagnostics (#X9).** `get.rs:380-384`.
- [ ] **#G12 — `TZ` not consulted for delta-time rendering (#X5).** Matches CSSC's
  stored-value behavior; low impact. (Note: `-b` IS implemented — `get.rs:33,230` —
  CONFORMS; listed only to retire the agent's retracted query.)

### Detailed conformance matrix

#### SYNOPSIS / OPTIONS
| Opt | Status | Notes (file:line) |
|---|---|---|
| `-r SID` | CONFORMS | `get.rs:27,186`; exact/partial/R.L verified incl. multi-release. |
| `-c cutoff` | MISSING | (#G2). |
| `-e` | CONFORMS | `get.rs:30`; p-file + 0644 g-file match CSSC. |
| `-b` | CONFORMS | `get.rs:33,230`. |
| `-i list` / `-x list` | MISSING | (#G3). |
| `-k` | CONFORMS | `get.rs:36,444`; implied by `-e`. |
| `-l` / `-L` | MISSING | (#G4). |
| `-p` | CONFORMS | `get.rs:39`; body→stdout, info→stderr; verified. |
| `-s` | CONFORMS | `get.rs:42`; suppresses info incl. multi-file header. |
| `-m` / `-n` | CONFORMS | `get.rs:48,51`; `<SID>\t…` / `<module>\t…` match CSSC. |
| `-g` | CONFORMS | `get.rs:45,428`; no g-file, SID printed. |
| `-t` | MISSING | (#G7). |

#### OPERANDS / STDIN
- [ ] **`-` MISSING (#G5); directory MISSING (#G6).** Real-file operands CONFORM;
  stdin not spuriously read (verified).

#### ENVIRONMENT / ASYNC / STDOUT-STDERR / OUTPUT FILES
- [x] `setlocale` — `get.rs:537`; `LC_MESSAGES`/`NLSPATH` partial (#G11); `TZ` unused (#G12).
- [x] Default signals (z-file cleanup moot until #G8 exists).
- [x] `"%s\n%d lines\n"` / `"%s\nnew delta %s\n%d lines\n"`; multi-file `"\n%s:\n"` header — exact match (`get.rs:374,421,530`).
- [ ] **`Included:`/`Excluded:` notation N/A until #G3; `No id keywords` warning MISSING (#G9).**
- [x] g-file naming/perms (0444; 0644 on `-e`/`-k`) — `get.rs:511`; p-file format byte-match, no double-lock (`get.rs:258`).
- [ ] **l-file MISSING (#G4); z-file MISSING (#G8).**

#### EXTENDED DESCRIPTION — ID keyword table (all 19 verified byte-identical, `get.rs:111-180`)
- [x] `%Z% %M% %I% %R% %L% %B% %S% %D% %H% %T% %E% %G% %U% %Y% %F% %P% %Q% %C% %W% %A%` — every keyword matched CSSC exactly (incl. per-line `%C%` increment).

#### EXIT STATUS
- [x] 0 success / >0 error; per-file failure continues — `get.rs:544-555`. Bad `-r SID` → exit 1.

### Test coverage gaps
- [ ] `-c`, `-i`, `-x`, `-l`, `-L`, `-t` (all MISSING); `-`/directory operands; CSSC-encoded interop fixture (#G1); z-file presence (#G8); `No id keywords` warning (#G9); `-m`/`-n`/`-g`; multi-release partial-SID.

---

## `prs`

**Implementation:** `sccs/prs.rs` (700) + `plib/src/sccsfile.rs`
**Spec:** POSIX.1-2024, Vol. 3 §3 — `prs.md`
**Date:** 2026-06-14 (behaviorally verified vs CSSC)

### TL;DR
`prs` reads the delta table, flags, stats, and descriptive text correctly, and
the *values* of nearly every data keyword match CSSC byte-for-byte. But three
defects break the golden path: (1) **no trailing newline** after each delta's
dataspec or after multi-line keywords, so `prs -d:I:` over 3 deltas prints
`1.31.21.1` and the *default* output corrupts to `mr-003bCOMMENTS:`; (2) **`-r`
with no SID is rejected**; (3) `:FL:` dumps Rust `Debug`, `:GB:` is
unimplemented, and `:A:` has a stray double space. Selection (`-e/-l/-r/-c/-a`)
and exit codes are otherwise solid.

### Priority issues

#### Critical
- [x] **#P1 — No trailing newline after each delta's dataspec / `M`-format keyword.**
  ✓ Phase 7: each delta's expansion and each non-empty `M`-format value is now
  newline-terminated (empty `M`-format values emit nothing, not a blank line).
  Default `prs`, `prs -d:I:`, and `-r` are **byte-identical to cssc**.

#### Major
- [x] **#P2 — `-r` optional option-argument rejected.** ✓ Phase 7:
  `num_args=0..=1`; a bare `-r` (rewritten to `-r=`) resolves to the trunk head.
  Byte-identical to cssc.
- [x] **#P3 — `:FL:` emits Rust Debug.** ✓ fixed in Phase 2 (#X3) via
  `SccsFlag::prs_fl_line()`; `prs.rs:394`.
- [x] **#P4 — `:GB:` (gotten body) unimplemented.** ✓ Phase 7: reconstructs the
  delta body via `compute_applied_set`/`evaluate_body` (uudecodes when encoded).
  Byte-identical to cssc.

#### Minor
- [x] **#P5 — `:A:` double space.** ✓ Phase 7: single space, matches cssc.
- [x] **#P6 — Unset `:LK:`/`:FB:`/`:CB:`/`:Ds:` print empty vs CSSC `none`.** ✓ Phase 7.
- [x] **#P7 — `::` literal-colon collapse.** ✓ Phase 7: the scanner emits a `:`
  literally unless it opens a recognized `:kw:`, so `a::b` prints verbatim.
- [x] **#P8 — `:PN:` canonicalizes the path.** ✓ Phase 7: emits the operand
  path as given (spec wins over cssc's absolutized form).
- [x] ~~**#P9 — `:KV:` still handled**~~ — WON'T FIX: a harmless retained
  extension (removed from POSIX by Austin Group Defect 1452 but accepted for
  compatibility); keeping it cannot misrender a conforming dataspec.
- [x] **#P10 — `:Li:`/`:Ld:`/`:Lu:` not zero-padded.** ✓ Phase 7: zero-padded to
  5 digits and clamped at 99999, matching cssc.

### Detailed conformance matrix

#### SYNOPSIS / OPTIONS
- [x] `-a -d -r -e -l -c` present (`prs.rs:24-45`); `--`/bundling via clap.
- [ ] **`-r[SID]` DIVERGES (#P2).**
- [x] `-e`/`-l` CONFORMS (`prs.rs:547,551`); verified `-e -r1.2` / `-l -r1.2`.
- [x] `-c cutoff` CONFORMS — `parse_cutoff` (`prs.rs:403-458`) with the 69-pivot and unit-default rules; verified.
- [x] `-a` CONFORMS — includes `Removed` only when set (`prs.rs:541`); verified with `rmdel`.

#### OPERANDS / STDIN
- [x] Multiple operands in order (`prs.rs:674`); `-` stdin list (`prs.rs:642`); directory expansion (`prs.rs:611`). All CONFORM (output corruption is #P1).

#### ENVIRONMENT / ASYNC / STDOUT-STDERR
- [x] `setlocale` — `prs.rs:633`; diagnostics hardcoded English (#X9).
- [ ] **STDOUT DIVERGES (#P1);** STDERR diagnostics only — CONFORMS.

#### EXTENDED DESCRIPTION — data-keyword matrix
Values verified one-by-one vs CSSC on a 3-delta file with flags/MRs/comments/desc
text and a removed delta. The universal trailing-newline gap (#P1) is **not**
re-listed per row.

| KW | Status | Evidence (prs.rs) |
|---|---|---|
| `:Dt: :DL: :DT:` | CONFORMS | `:266,253,276`; Dt line/stats match. |
| `:I: :R: :L: :B: :S:` | CONFORMS | `:288,295,289,210,296`; trunk B/S empty (matches CSSC). |
| `:D: :Dy: :Dm: :Dd:` | CONFORMS | `:218,287,257,219`. |
| `:T: :Th: :Tm: :Ts:` | CONFORMS | `:303-306`. |
| `:P:` | CONFORMS (read) | `:294` (writer-side login is #X4). |
| `:DS: :DP: :DI: :Dn: :Dx: :Dg:` | CONFORMS | `:265,264,226,258,281,220`. |
| `:MR: :C: :FD:` | CONFORMS value / **#P1 newline** | `:293,217,162`. |
| `:UN:` | CONFORMS | `:192` (`none` when empty). |
| `:FL:` | DIVERGES | (#P3) Debug format. |
| `:Y: :MF: :KF: :J: :ND: :MP: :Q: :BF:` | CONFORMS | `:199,178,168,164,183,182,191,147`. |
| `:LK: :FB: :CB: :Ds:` | DIVERGES (Minor #P6) | empty vs `none`. |
| `:GB:` | MISSING | (#P4). |
| `:W: :Z: :F:` | CONFORMS | `:307,313,155`. |
| `:A:` | DIVERGES | (#P5) double space. |
| `:PN:` | PARTIAL | (#P8) canonicalized. |
| `:Li: :Ld: :Lu:` | DIVERGES (Minor #P10) | not zero-padded; cap unenforced. |
| `:KV:` | extension | (#P9) spec-removed but harmless. |

#### EXIT STATUS
- [x] 0 success; non-zero when any file fails — `prs.rs:639,695`. Verified.

### Test coverage gaps
- [ ] Trailing-newline assertion (#P1); multi-delta `-d` (default fixture is single-delta); `-r` no-SID (#P2); `-e`/`-l`/`-c`/`-a`; `:FL:`/`:GB:`/`:A:`/`:MR:`/`:UN:`/`:FD:`/line-stat padding; multi-file/stdin/directory; exit-code.

---

## `rmdel`

**Implementation:** `sccs/rmdel.rs` (220) + `plib/src/sccsfile.rs`
**Spec:** POSIX.1-2024, Vol. 3 §3 — `rmdel.md`
**Date:** 2026-06-14 (behaviorally verified vs CSSC). **No integration tests exist.**

### TL;DR
The error-path logic conforms and matches CSSC: leaf-only enforcement, p-file
(being-edited) rejection, nonexistent/already-removed SID, multi-file exit
aggregation. The headline defect is that the **body weave is never rewoven** —
rmdel correctly marks the delta entry type `R` (matching CSSC) but leaves the
removed delta's text and `^AI/^AE` block in the body, so the s-file diverges
from CSSC byte-for-byte. The ownership check is a spoofable `$LOGNAME` compare,
and the rewritten file loses its `0444` read-only mode.

### Priority issues

#### Major
- [x] **#R1 — Body weave not rewoven on removal; orphan `^AI/^AE` block + text remain.**
  ✓ fixed in Phase 2 via `SccsFile::remove_delta` (#X2); `rmdel.rs` now calls it.
  Verified byte-identical to CSSC.
- [x] **#R2 — Ownership check is `$LOGNAME` string-compare, not real-uid / file-owner / dir-owner.** ✓ Phase 9: real-uid / s-file-owner / dir-owner rule.
  `rmdel.rs:32-36,64-69`. Spec restricts removal to the delta author, the s-file
  owner, or the directory owner. Ours blocks a legitimate file owner who didn't
  author the delta, and is defeated by `LOGNAME=<author> rmdel …` (verified).
  Fix: compare `getuid()` to the author uid / `stat` owner / parent-dir owner.

#### Minor
- [x] **#R3 — Rewritten s-file loses read-only mode (0444 → 0664).** `rmdel.rs:115-117` ✓ Phase 9: preserves the original mode (r--r--r--).
  writes a fresh temp at the umask default. Fix: preserve mode (or use
  `plib::io::write_atomic`).
- [x] **#R4 — Predictable temp path can collide / leak.** `rmdel.rs:115` uses ✓ Phase 9: uses the canonical x-file name.
  `with_extension("tmp")` (world-readable, non-unique; concurrent rmdels race).
  Fix: use the x-file name from `paths::xfile_from_sfile`.
- [ ] **#R5 — No z-file locking.** (#X8) `rmdel.rs`.

### Detailed conformance matrix

#### SYNOPSIS / OPTIONS / OPERANDS
- [x] `-r SID` + `file...` required — `rmdel.rs:25-29,151`; invalid SID → diagnostic + non-zero.
- [x] Directory operand (`rmdel.rs:122-141`); `-` stdin list (`rmdel.rs:162-192`); non-SCCS silently skipped in those forms. Verified.

#### ENVIRONMENT / ASYNC / STDOUT-STDERR / OUTPUT FILES
- [x] `setlocale` — `rmdel.rs:144`; `LC_MESSAGES` partial (#X9).
- [x] Default signals; STDOUT unused; diagnostics to stderr.
- [ ] **x-file/z-file MISSING (#R4/#R5); read-only mode not preserved (#R3).**

#### EXTENDED DESCRIPTION (leaf-only + p-file)
- [x] Leaf-only rule — `rmdel.rs:71-86`; verified middle 1.2 and initial 1.1 error (match CSSC).
- [x] p-file being-edited rejection — `rmdel.rs:88-100`; verified.
- [x] Already-removed (type R) rejection — `rmdel.rs:102-106`; verified.
- [x] Delta marked `R`, table entry retained (not physically deleted) — `rmdel.rs:109`; confirmed correct vs CSSC `prt -a`.
- [ ] **Body reweave MISSING (#R1).**

#### EXIT STATUS
- [x] 0 success / >0 error; per-file aggregation — `rmdel.rs:159,215`. Verified multi-file.

### Test coverage gaps
- [ ] No integration tests at all. Need: leaf removal marks `R` + matches CSSC bytes (#R1); non-leaf/initial rejection; p-file rejection; nonexistent/already-removed; `-`/directory operands; ownership (#R2); mode preservation (#R3).

---

## `sact`

**Implementation:** `sccs/sact.rs` (130) + `plib/src/sccsfile.rs`
**Spec:** POSIX.1-2024, Vol. 3 §3 — `sact.md`
**Date:** 2026-06-14 (behaviorally verified vs CSSC)

### TL;DR
`sact` is essentially conforming: it reads the p-file and prints
`<SID> <new SID> <login> <date> <time>` per pending edit, emits the `\n%s:\n`
per-file header for multi-file/directory/stdin cases, and matched CSSC
byte-for-byte everywhere tested. The only gaps are strict-conformance: it never
returns a non-zero exit on a genuine error. **No Critical or Major defects.**

### Priority issues

#### Minor
- [x] **#S1 — Exit status always SUCCESS.** ✓ Phase 8: a parse failure on a named
  operand (or `read_dir` failure) now yields a non-zero exit; no-pending stays 0.
- [x] **#S2 — stdin `-` header forced when first operand is `-` even in a multi-operand list.**
  ✓ Phase 8: header now gates on stdin / >1 file / directory.
- [x] **#S3 — "no pending delta" informative message absent.** CONFORMS in
  practice (the message is optional and CSSC is also silent); no action.

### Detailed conformance matrix
- [x] `sact file...`, no options — `sact.rs:22-27`.
- [x] Multiple operands each prefixed `\n%s:\n` (`sact.rs:47-49`); directory
  expansion (`sact.rs:66-84`); single `-` stdin list (`sact.rs:102-118`). Verified vs CSSC.
- [x] STDOUT `"%s %s %s %s %s\n"` = `<SID> <new SID> <login> <date> <time>` —
  `sact.rs:52-61`; `<date>`=`:D:` format, `<time>`=`:T:`; verified
  `1.3 1.4 jgarzik 26/06/14 08:27:01`.
- [x] `setlocale` (`sact.rs:87`); default signals; no output files.
- [ ] **Always exits 0 (#S1);** no-pending → no stdout, exit 0 (verified, matches CSSC).
- Writer-side note: the p-file `<login>` is `$USER`/`$LOGNAME` in our get/admin/delta (#X4); `sact` faithfully echoes it.

### Test coverage gaps
- [ ] Multi-file header; directory/stdin operands; exact-format byte assertion (tests only `contains`); error-path exit (#S1).

---

## `sccs` (front-end)

**Implementation:** `sccs/sccs.rs` (615) + spawns the sibling utilities
**Spec:** POSIX.1-2024, Vol. 3 §3 — `sccs.md`
**Date:** 2026-06-14 (behaviorally verified vs `/usr/bin/sccs`). **No integration tests exist.**

### TL;DR
The dispatcher covers the full POSIX pseudo-utility set (check, clean, create,
delget, deledit, diffs, edit, fix, info, print, tell, unedit) plus all nine real
utilities, and the SCCS/-dir + `s.` prefix rewriting works (verified for
relative, pre-prefixed, absolute, and `-d`/`-p` forms). Two Major defects: `-r`
(run-as-real-user) is parsed but **never acted on** (the parameter is literally
`_use_real_uid`; no `setuid` anywhere), and subcommands are spawned **by bare
name** (`Command::new("get")`), so the front-end breaks without `$PATH` help.

### Priority issues

#### Major
- [x] **#SC1 — `-r` (real-uid) is a complete no-op.** `sccs.rs:123,130` set the ✓ Phase 9: setgid(getgid())+setuid(getuid()) before spawn when -r set.
  flag; `run_sccs_command`'s parameter is `_use_real_uid` (`sccs.rs:584`,
  underscore-ignored); no `setuid`/`getuid` anywhere (verified). Since the
  front-end is the component meant to be installed set-uid, this defeats the
  privilege-separation purpose. Fix: `setuid(getuid())`/`setgid(getgid())` before
  spawning (and check the return values).
- [x] **#SC2 — Subcommands resolved by bare name via `$PATH`.** `sccs.rs:339,385,398,526,534` ✓ Phase 9: siblings resolved relative to current_exe(), PATH fallback.
  + `:578` use `Command::new("get"/"admin"/"rmdel"/"diff"/cmd)`. With a clean PATH
  the front-end can't find its siblings (`sccs edit hello.c` → `get: No such file
  or directory`). CSSC hardcodes its libexec dir. Fix: resolve relative to
  `current_exe()`'s directory, `$PATH` fallback.

#### Minor
- [x] **#SC3 — `info` omits the old-SID and timestamp fields.** `sccs.rs:431-453` ✓ Phase 9: info now prints old/new SID, user, date, time.
  prints `new_sid user`; CSSC prints `old new user date time`.
- [x] **#SC4 — Pseudo-command option-splitting uses substring `starts_with`.** ✓ Phase 9: option-splitting now routes by option letter, not substring.
  `sccs.rs:218-310,482-518` mis-binds options whose argument begins with a known
  letter, and routes shared letters (e.g. `-s`) to both delta and get. Fix: parse
  the spec's explicit letter partitions.
- [x] ~~**#SC5 — `-p` is the BSD subdir-name form**~~ — WON'T FIX: the historical
  BSD `sccs` `-p` is exactly the subdir-name form ours implements; kept
  deliberately (it is what real `sccs` users expect).
- [ ] **#SC6 — A leading unknown `-` token is treated as the command.** `sccs.rs:152-154`
  (`-q get` → "unknown command '-q'"). Minor robustness; deferred.
- [ ] **#SC7 — Diagnostics not localized (#X9).** `sccs.rs:20,354,371,572,611`.
- [ ] **#SC8 — PROJECTDIR username form uses `$HOME/<dir>/src`, not the named user's home.**
  `sccs.rs:160-180`; no `getpwnam`. Edge feature.
- [x] **#SC9 — `diffs` temp path `/tmp/sccs_diff.<pid>` is predictable/world-writable.** ✓ Phase 9: diffs temp now uses env::temp_dir() with a unique name.
  `sccs.rs:482-545`. Minor security smell.

### Detailed conformance matrix

#### SYNOPSIS / OPTIONS / prefix handling
- [x] `-d path` (+ glued) — `sccs.rs:132-141`; verified `-d proj get f.c` → `proj/SCCS/s.f.c`.
- [ ] **`-p path` DIVERGES (#SC5); `-r` MISSING/no-op (#SC1).**
- [x] Default `SCCS/s.` prefixing; pre-prefixed/absolute pass-through — `sccs.rs:25-51`; verified.

#### EXTENDED DESCRIPTION — pseudo-command matrix
| Pseudo-cmd | Status | Evidence (sccs.rs) |
|---|---|---|
| `check` | CONFORMS | `:412`; editing→exit 1, clean→exit 0. |
| `clean` | CONFORMS | `:457`; removes non-edited g-files, keeps p-file ones. |
| `create` | CONFORMS (variant) | `:312`; admin -i + rename to `,name` (auto-mkdirs SCCS/, which CSSC does not). |
| `delget` | CONFORMS | `:218`; delta then read-only get. |
| `deledit` | CONFORMS | `:262`; delta then `get -e`. |
| `diffs` | PARTIAL | `:482`; `-C`→`-c` map present; loose splitting (#SC4), temp path (#SC9). |
| `edit` | CONFORMS | `:194` = `get -e`; verified p-file. |
| `fix` | CONFORMS | `:362`; requires `-r SID`; `get -k -rSID` then `rmdel`. |
| `info` | PARTIAL | `:412`; omits old-SID + date (#SC3). |
| `print` | CONFORMS | `:547` = `prs`. |
| `tell` | CONFORMS | `:439`; newline-separated g-file list. |
| `unedit` | CONFORMS (variant) | `:206` = `unget`; leaves no read-only g-file (spec silent). |
- [x] Real utilities `admin delta get prs rmdel sact unget val what` dispatch — `sccs.rs:560` (subject to #SC2).
- [x] `sccsdiff`/`enter` correctly absent (CSSC/BSD extras, not in the POSIX list); spec's `diffs` present.

#### ENVIRONMENT / EXIT
- [x] `setlocale` + PROJECTDIR slash-form — `sccs.rs:111,162`; username form partial (#SC8); `LC_MESSAGES` partial (#SC7).
- [x] Propagates child exit codes (`sccs.rs:602`); no command → usage exit 2; unknown command → exit 1. Verified.

### Test coverage gaps
- [ ] No integration tests at all (`sccs-tests.rs` omits `sccs` and `rmdel`). Need: prefix/`-d`/`-p` resolution; `-r` real-user (#SC1); sibling resolution without `$PATH` (#SC2); each pseudo-command; `info` fields (#SC3); option-splitting (#SC4).

---

## `unget`

**Implementation:** `sccs/unget.rs` (218) + `plib/src/sccsfile.rs`
**Spec:** POSIX.1-2024, Vol. 3 §3 — `unget.md`
**Date:** 2026-06-14 (behaviorally verified vs CSSC)

### TL;DR
The golden path is solid: `unget s.file` removes the p-file entry and g-file,
prints the SID, and `-r`/`-s`/`-n`/no-pending all match CSSC on single files. The
one real defect is the **multi-file/stdin/directory STDOUT format**: the spec
mandates a `\n%s:\n` pathname prefix before each SID when more than one file (or a
directory or stdin) is named, and ours omits it. Directory operands aren't
expanded.

### Priority issues

#### Major
- [x] **#U1 — Multi-file output missing the mandated `\n%s:\n` prefix.** ✓
  Phase 10: a `\n<path>:\n` header precedes each SID when >1 file / a directory /
  stdin is named. Byte-identical to cssc; single-file stays a bare SID.
- [x] **#U2 — Directory operand not expanded.** ✓ Phase 10 via `paths::expand_operands`.

#### Minor
- [x] **#U3 — `-` stdin form also misses the pathname prefix.** ✓ Phase 10
  (covered by the #U1 header gating + `expand_operands`).
- [ ] **#U4 — Diagnostics lack the `unget:` prefix; hardcoded English (#X9).**
  Partly done in Phase 10 (operand-loop diagnostics now carry the `unget:`
  prefix); full `gettext` wrapping deferred to the i18n phase (#X9).
- [ ] **#U5 — q-file working-copy not used.** `unget.rs:133-143` rewrites the p-file
  directly (non-atomic); spec says the q-file "may" be created — N/A, noted for robustness.

### Detailed conformance matrix
- [x] `-r SID` (`unget.rs:71-92`; matches new_sid, wrong-SID → exit 1), `-s` (`:118`), `-n` (`:123`, g-file retained) — all CONFORM, verified.
- [x] Single operand CONFORMS; **`-` PARTIAL (#U3); directory MISSING (#U2).**
- [x] `setlocale` (`unget.rs:171`); default signals (spec "Default").
- [ ] **Multi-file prefix MISSING (#U1);** single-file `"%s\n"` CONFORMS; diagnostics to stderr (#U4).
- [x] p-file entry + g-file removed — `unget.rs:123-143`; verified vs CSSC.
- [x] 0 success / >0 error; no-pending → exit 1 — `unget.rs:213`.

### Test coverage gaps
- [ ] Multi-file `\n%s:\n`; directory operand; `-` stdin; `-r` among several pending edits; CSSC p-file interop fixture.

---

## `val`

**Implementation:** `sccs/val.rs` (238) + `plib/src/sccsfile.rs`
**Spec:** POSIX.1-2024, Vol. 3 §3 — `val.md`
**Date:** 2026-06-14 (behaviorally verified vs CSSC)

### TL;DR
The bit-coded exit status and per-file validation (checksum, `-r` SID existence,
`-m`/`-y` keyword compare) are correct and in places more conformant than CSSC.
The defects were the two argv-error bits (0x80 missing-file, 0x40
unknown/duplicate-option) being unreachable behind clap's exit-2, and the
stdin-mode STDOUT format/order.

### Priority issues

#### Major
- [x] **#V1 — Missing file argument yields clap exit 2, not 0x80.** ✓ Phase 8:
  dropped `required=true`; the no-operand check returns 0x80 (exit 128, matches
  cssc).
- [x] **#V2 — Unknown / duplicate option yields clap exit 2, not 0x40.** ✓
  Phase 8: `try_parse_from` + map parse errors to 0x40 (exit 64, matches cssc);
  in stdin mode the bit is OR'd into the aggregate.
- [x] **#V3 — stdin-mode STDOUT format diverges from spec.** ✓ Phase 8: emits
  the input line, a blank line, then the space-indented `pathname: <msg>`, in
  spec order.

#### Minor
- [x] **#V4 — `-r 1.1.0` (branch 0) treated as `1.1`.** ✓ WON'T FIX (deferred):
  the 3-part-zero-branch normalization lives in `plib::Sid::from_str`; cssc also
  returns 0 here, so impact is nil. Tracked for a future plib change.
- [x] **#V5 — `-` as one of several operands treated as a non-SCCS filename.** ✓
  WON'T FIX: spec defines `-` as stdin only when it is the sole operand; ours
  already matches the spec for that case.
- [x] **#V6 — checksum verification skipped when body empty.** ✓ Phase 8:
  header-only files are now checksum-verified.
- [x] **#V7 — "cannot open" vs "corrupted" re-stats the file (TOCTOU).** ✓
  WON'T FIX: a benign edge-case label distinction; race-hardening deferred.

### Detailed conformance matrix
- [x] `-s`, `-m`, `-r`, `-y` parse and validate; bit-coded status 0x01–0x80 with
  multi-file OR — `val.rs`. Verified each bit vs cssc.
- [x] Lone `-` reads stdin, each line re-parsed as an arg list (more conformant
  than cssc) — CONFORMS, format fixed (#V3).
- [x] Informative messages → stdout, diagnostics → stderr; `setlocale` wired
  (`LC_MESSAGES` hardcoded English pending #X9).

### Test coverage gaps
- [ ] EXAMPLE from the spec; 0x20-vs-0x10 distinction; combined-bit OR cases.

---

## `what`

**Implementation:** `sccs/what.rs` (78)
**Spec:** POSIX.1-2024, Vol. 3 §3 — `what.md`
**Date:** 2026-06-14 (behaviorally verified vs CSSC)

### TL;DR
For 7-bit text input `what` is essentially perfect — `@(#)` scanning, all five
terminators, multiple matches, `-s`, multi-file headers, and the 0/1 exit rule
are byte-identical to CSSC. But one Critical defect defeats the utility's primary
purpose: **`what` aborts on the first file containing non-UTF-8 bytes** — i.e.
virtually every binary/`.o`/`a.out`, exactly what `what` exists to scan.

### Priority issues

#### Critical
- [ ] **#W1 — Non-UTF-8 input aborts `what`, skipping all remaining files.**
  `what.rs:33` uses `reader.lines()` → `InvalidData` ("stream did not contain
  valid UTF-8"); the `?` at `:34` propagates. Verified: `what target/release/what`
  → `Error: … InvalidData …` where CSSC printed ` GNU CSSC 1.4.1`. Spec INPUT
  FILES: "any file type"; the EXAMPLES scan `a.out`. Fix: read raw bytes and scan
  the `@(#)` byte pattern; never UTF-8-decode.

#### Minor
- [ ] **#W2 — Error message format/order differs from CSSC** (cosmetic; both stderr). `what.rs:72`.
- [ ] **#W3 — Hardcoded-English "Cannot open file" (#X9).** `what.rs:72`.
- [ ] **#W4 — `-s` buffers the whole file before stopping.** `what.rs:33`; with the
  #W1 byte-reader rewrite, `break` on first ident (FIFO/RATIONALE concern). Minor.

### Detailed conformance matrix
- [x] `-s` CONFORMS — `what.rs:21-25,37`; verified (two idents on one line → only first).
- [x] STDIN not used; **INPUT FILES "any file type" DIVERGES for non-UTF-8 (#W1).**
- [x] `setlocale` (`what.rs:55`); `LC_MESSAGES` partial (#W3); default signals.
- [x] `"%s:\n"` header per file (even with zero matches) then `"\t%s\n"` per ident — `what.rs:46,66`; verified.
- [x] Ident terminates at `" > \n \ NUL` or EOF; resumes scanning — `what.rs:41,48`; verified each terminator.
- [x] Exit 0 if ≥1 match, 1 otherwise — `what.rs:77`; verified (match/no-match/nonexistent/no-operand/mixed). **#W1 corrupts this** for a non-UTF-8 first file.

### Test coverage gaps
- [ ] Binary/non-UTF-8 input (#W1) — the single most important use case; the
  existing `special_characters.txt` only has NUL inside valid UTF-8. Multi-file
  mixed match/no-match exit; two `@(#)` on one line; ELF/`.o` smoke test.

---

## Suggested PR groupings

Ordered roughly by value. Shared-core (`plib::sccsfile`) work is called out
because it closes findings across several utilities at once.

- **PR A — "plib::sccsfile: decode `e`-flag bodies"** (#X1/#G1). Highest
  data-integrity priority; unblocks correct binary/no-eol round-trips with CSSC.
  Add a CSSC-encoded interop fixture.
- **PR B — "what: byte-faithful scanning"** (#W1, #W4). Fixes the utility's core
  purpose; add ELF/binary/non-UTF-8 tests.
- **PR C — "plib::sccsfile: reweave on delta removal"** (#X2/#R1) + new
  `SccsFile::remove_delta`; byte-vs-CSSC rmdel test.
- **PR D — "prs: newline correctness + keyword fidelity"** (#P1, #P3/#X3, #P4,
  #P5, #P6, #P10). #P1 fixes default + all multi-delta `-d` output.
- **PR E — "Optional/empty option-arguments"** (#A1 admin `-i`/`-t`/`-y`, #P2 prs
  `-r`, #V1/#V2 val exit bits). Replace clap defaults with hand-parsing where the
  spec mandates attached-only / optional arguments.
- **PR F — "delta: missing options + stdin comment"** (#D1 `-m`, #D2 `v`-flag,
  #D3 stdin/prompt, #D4 `-g`). All reuse existing plib writers.
- **PR G — "Operand forms: `-` stdin list + directory"** (#X7: #G5/#G6, #D6/#D7,
  #A3/#A4) and the multi-file prefix (#U1/#U2).
- **PR H — "get: missing options"** (#G2 `-c`, #G3 `-i`/`-x`, #G4 `-l`/`-L`, #G7 `-t`).
- **PR I — "admin: flag validation"** (#A2 reject unknown flags, #A8 range checks).
- **PR J — "val: exit-status bits + stdin format"** (#V1 0x80, #V2 0x40, #V3 format).
- **PR K — "sccs front-end: locate siblings + honor -r"** (#SC2 `current_exe`
  resolution, #SC1 `setuid`), then option-splitting + `info` fields (#SC3, #SC4).
- **PR L — "Identity & time correctness"** (#X4 `getpwuid` login, #X5 TZ-aware
  timestamps, #R2 rmdel ownership rule).
- **PR M — "No id keywords warning + i-flag escalation"** (#X6/#G9/#A9).
- **PR N — "Locking + atomic writes"** (#X8 z-file, #R3/#R4 rmdel mode/temp,
  #D5/#X10 SIGINT cleanup).
- **PR O — "i18n: route diagnostics through gettext"** (#X9, crate-wide).
- **PR P — "Tests"** — add `sccs/tests/{rmdel,sccs}/` (register in
  `sccs-tests.rs`); byte-exact multi-delta/multi-file/stdin fixtures; per-finding
  regression tests.

---

## Notes for the next session

- The shared core is the asset: checksum, SID, delta-table, weave, and p-file
  formats are CSSC-interoperable in both directions. Most findings are CLI-layer
  gaps in the per-utility `.rs` files; the genuine core defects are #X1 (encoded
  bodies), #X2 (reweave), #X3 (`format_flags`), #X4 (login), #X5 (TZ).
- Behavioral oracle: GNU CSSC 1.4.1 backends at
  `/usr/lib/x86_64-linux-gnu/cssc/<util>`, front-end `/usr/bin/sccs`. Build our
  side with `cargo build --release` (binaries in `target/release/`).
- `val` and `sccs` were in several places found *more* conformant than CSSC
  (val's `-` line-splitting and combined-bit OR; val's `-r1` → 0x08; admin's
  trunk-only `-r`); those are noted as CONFORMS+, not divergences.
</content>
</invoke>
