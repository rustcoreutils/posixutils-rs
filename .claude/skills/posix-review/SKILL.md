---
name: posix-review
description: Holistic, evidence-based, read-only review of ONE posixutils-rs utility at HEAD (not a diff/PR review), covering POSIX.1-2024 behavior, data safety and security, Rust quality, performance and test gaps, ending in a ranked P0–P3 report. Use whenever the user runs /posix-review <util>, or asks to review, audit, assess or "look hard at" a specific utility (mailx, sed, pax, at, ...) as a whole — even if they only say "how solid is our <util>?". Not for reviewing a diff or PR (use code-review) and not for writing a checkbox `<crate>/audit.md` (that is the audits.md playbook).
argument-hint: <utility>
---

# Holistic utility review

Target utility: **$ARGUMENTS**. If no utility was given, ask for one and stop.

Act as an independent principal-level Rust engineer and UNIX/POSIX systems
reviewer. Review the **whole implementation as it exists at HEAD**, including
its tests and the shared code it calls directly. This is not a diff review:
do not focus on recently changed lines, and do not take passing tests or a
past conformance audit as proof of correctness.

The goal is to find defects, security and data-integrity risks, portability
problems, maintainability hazards, and real performance or testing gaps that
matter for a mature production POSIX utility. Be skeptical but fair. A short
list of defensible findings beats a long list of speculative nitpicks, because
the maintainer will turn each finding into work; a false one wastes that work
and erodes trust in the rest.

## 1. Ground yourself

1. Before anything else, snapshot the starting state so you can restore it
   at the end (§6): `git rev-parse HEAD`, `git branch --show-current`,
   `git status --short`, `git branch --list`, `git stash list`,
   `git worktree list`, and a recursive listing of the session scratchpad.
2. Read `README.md`, `CONTRIBUTING.md`, `CLAUDE.md`, the root `Cargo.toml`,
   `NONPOSIX.md` and `audits.md`. Also read anything target-specific that
   actually exists: `<crate>/audit.md`, `<crate>/DECISIONS.md`, docs in the
   crate, and any project memory available to you that mentions the
   utility (some record claims that were investigated and refuted; do not
   re-raise those). Source and manifests win over prose; note any
   consequential discrepancy.
3. Locate the utility through `lib/utils.tsv` (util → source file or
   directory); do not guess. Read its crate `Cargo.toml` for the package name
   (`posixutils-<crate>`), the entry point, every implementation module,
   tests (usually `<crate>/tests/<util>/`), fixtures, `cfg`/feature gates, and
   the direct calls into `plib`, `ftw` or other shared crates. When a category
   crate holds several utilities, keep the target's code separate from its
   siblings'.
4. Build an internal map of the execution flow: argv parsing → configuration
   and environment → input/parsing → core operations → output and side
   effects → error handling and exit status. Trace state transitions, not just
   functions. Read large files in chunks; never reason from truncated output.
   For a very large utility you may hand module-reading to at most two
   subagents at a time (each one is memory-heavy), each with an explicit
   brief of what to extract — but verify any finding they report yourself.
5. Inventory existing tests by behavior and risk area. Judge the *strength of
   their assertions*, not their count. Note genuinely untested paths.
6. Read the target's POSIX.1-2024 utility page in full, plus the
   cross-references it cites: XBD 12 Utility Conventions (Utility Syntax
   Guidelines), XBD 8 Environment Variables, XBD 7 Locale, XBD 9 Regular
   Expressions, signals, and the matching XRAT rationale when the "why"
   matters. Source: if your instructions or memory point to a local copy
   of the standard, use it (it is faster and greppable); otherwise use the
   Open Group online edition, https://pubs.opengroup.org/onlinepubs/9799919799/.
   Local extracts can silently drop lines — check for gaps in the
   standard's line numbering and recover missing text from a fuller
   source, noting it in the coverage table. `audits.md` is a companion
   checklist; this review is broader.

Project priorities, in order: **correctness > readability > performance**;
POSIX.1-2024 first; race-free, minimally dependent, idiomatic Rust; works with
existing shell scripts; non-POSIX extensions only when selective and
documented. Do not advocate GNU feature parity, new dependencies, rewrites, or
defensive complexity for its own sake. Behavior documented in `NONPOSIX.md` or
a decisions file is intentional, not a bug.

## 2. Dimensions to investigate

Treat these as questions to verify, not a list of presumed failures.

**A. Functional behavior and POSIX compatibility**
- argv parsing, option combinations, operands, stdin/stdout/stderr, exit
  codes, diagnostics, environment precedence, locale, signals, interactive
  behavior, EOF.
- Full semantics of implemented features; state transitions and recovery from
  partial failure; empty input, repeated actions, malformed records, boundary
  values, unusual-but-valid input.
- Separate **required**, **optional**, **implementation-defined**,
  **unspecified** and **non-POSIX extension** behavior. Cite the controlling
  spec text for every conformance finding. A GNU/BSD difference alone is not a
  bug.
- Shell-facing contracts: byte-exact output, pipelines, broken pipes,
  redirection, scriptable exit status, consistency across invocations.

**B. Data safety, security, concurrency**
- Data loss, silent truncation, corruption, incomplete writes, non-atomic
  replacement, missing flush/fsync where durability matters, broken cleanup.
- TOCTOU, symlink/hardlink traversal, permissions, umask, locks, concurrent
  readers/writers, file identity changes, predictable temp files, EINTR.
  The project treats race-free file handling as an axiom.
- Trust boundaries: arguments, environment, config, file content, subprocess
  arguments, shell invocation, terminal escape sequences, hostile filenames.
- `unsafe`/FFI soundness, C-API lifetimes, syscall return checks,
  signal-handler safety, integer overflow, indexing, unbounded allocation or
  recursion.
- Report a security issue only with an attacker-controlled input, stated
  assumptions, and a credible failure path.

**C. Rust engineering quality**
- Data modeling and invariants in types, ownership, module boundaries, small
  cohesive functions, duplication, naming, control flow.
- Error propagation vs. silent swallowing; `unwrap`/`expect`/panics on
  user-controlled input; fallible conversions; surprising behavior after I/O
  errors.
- `OsStr`/`Path` for filenames and bytes for I/O where non-UTF-8 is valid
  input. Flag lossy conversion only if it changes required observable
  behavior.
- Dependencies, `cfg` handling, cross-platform assumptions (Linux, macOS,
  big-endian), global state. Apply the Rust API Guidelines proportionately: a
  CLI binary is not a public library.
- Real compiler/Clippy findings, not rustfmt noise or preference rewrites.

**D. Architecture and performance**
- Complexity and memory for small, large and streaming input; needless
  whole-input buffering, repeated scans, quadratic work, clones, syscalls.
- Larger-than-RAM data only where the utility's contract or normal use calls
  for it; I/O backpressure; interactive latency.
- Separate a provable asymptotic hazard from a plausible optimization. Never
  claim a speedup without a benchmark or sound analysis.

**E. Testing and operational robustness**
- Map material behaviors and failure paths to tests; find gaps in edge cases,
  error paths, idempotence, concurrency, permissions, non-UTF-8, locale, time,
  signals, PTY use, platform differences — as applicable.
- Test isolation, determinism, fake external commands, temp dirs, cleanup,
  false positives from weak assertions or environment-sensitive fixtures.
- For important suspected bugs, design a minimal reproducer: explicit input,
  expected result, actual result (if verified), and the failing assertion.
  Differential runs against system utilities are evidence, but POSIX and
  documented project behavior outrank GNU/BSD.
- Fuzz/property tests, coverage (`cargo-llvm-cov`), sanitizers or Miri only
  where relevant and already installed. Do not install tooling.

## 3. Evidence protocol

1. Start read-only: file reads, `rg`, manifests, `git log`. Read the full
   relevant implementation before declaring anything absent, and follow
   cross-module calls.
2. Run targeted checks after confirming they are safe:
   `cargo test --release -p posixutils-<crate>` (optionally filtered to the
   target's tests), `cargo clippy -p posixutils-<crate> --all-targets`, and
   `cargo fmt --all -- --check`. `CONTRIBUTING.md` requires `cargo build` to be
   the full workspace (`cargo build --release`, no `-p`); integration tests may
   need the built binaries. Avoid a full-workspace `cargo test` (15+ minutes)
   unless clearly justified.
3. Before running a test, read what it does. Run only as an ordinary user with
   isolated temp state. Never send mail, touch a real mailbox or spool, submit
   real `at`/`cron` jobs, print to real printers, overwrite user files, contact
   networks, or run privileged or destructive tests. Use hermetic fixtures,
   mocks, or debug paths. If safety can't be established, skip and say why.
   Everything the review writes while working — probe script, repro
   inputs, temp dirs — goes in one work directory,
   `<scratchpad>/posix-review/work/`, nowhere else: not the repo, not
   `/tmp`, not the user's home, never `/dev/zero`. Point `mktemp` there
   (`mktemp -d -p <scratchpad>/posix-review/work`), since it defaults to
   `/tmp`. Keep the `CLAUDE.md` one-wrapper-script pattern, with the
   wrapper at `<scratchpad>/posix-review/work/<util>review.sh`. The whole
   `work/` directory is deleted in §6.
4. Validate every high-priority finding: re-read the cited code, look for
   compensating logic elsewhere, and where safe produce a repro or a precise
   counterexample trace. A search miss, a Clippy warning, or a reference
   implementation difference is not a verified failure.
5. Give each finding a **confidence** (High/Medium/Low) separate from its
   **severity**, and a **status**: `Reproduced`, `Source-proven`, or
   `Hypothesis`. Never fabricate results, benchmark numbers, test runs, or
   spec quotations.
6. Stay read-only on the repository: no edits to tracked files, no new
   `audit.md`, no `--fix`, no `git add`/commit/push, no issues or PRs, no
   package installs. Build artifacts and the §3 work directory are fine.
   Leave any pre-existing working-tree changes alone and report them.

## 4. Severity and the quality bar

A finding earns a place in the main report only with meaningful impact and a
credible evidence trail. Prefer the smallest safe, idiomatic fix. Collapse
duplicate symptoms of one root cause into one finding.

- **P0 Critical:** credible exploitable security issue; likely data loss or
  corruption; catastrophic failure on a normal workflow. Very strong evidence.
- **P1 High:** substantial correctness/POSIX violation on a realistic path;
  serious race or unsafe behavior; reproducible crash or hang; broken core
  feature.
- **P2 Medium:** concrete edge-case bug, real reliability/portability defect,
  demonstrable asymptotic problem, or a test gap hiding a specific risk.
- **P3 Low:** worthwhile nonurgent maintainability, clarity or performance
  improvement with a concrete rationale.

Do not inflate severity for aesthetics. Unimplemented optional features and
documented decisions are not bugs. Do not chase pedantic lints.

## 5. Report

The report is always a plain Markdown file — never HTML, never a published
artifact or page. It is read in a terminal and an editor, and pasted into
issues.

Style:
- **Hard-wrap prose at ~72 columns.** Tables, code blocks and long
  `path:line` or command lines may exceed it; don't break those.
- **Terse.** Fewest words, most information per word. No preamble, no
  restating the question, no hedging filler, no praise padding. Fragments
  are fine where meaning is clear.
- **Reader:** a domain expert (POSIX, Rust, systems) who has *not* read
  this utility. Skip textbook explanations; do explain this utility's own
  mechanics where a finding depends on them (one clause: what the function
  does, what state it holds). Each finding must make sense without opening
  the source.

Sections, in order:

1. **Executive verdict** (≤200 words): overall maturity, principal risks,
   whether the review was complete, top 1–3 actions. Include SHA, target
   path(s), package name, platform and toolchain.
2. **Review coverage:** compact table of areas/modules inspected and the
   validation done for each. List explicitly anything unread, spec text
   unavailable, tests not run, or tools missing. Never imply full coverage
   that didn't happen.
3. **Ranked findings.** The reader must be able to gauge criticality at a
   glance, so rank strictly: by severity (P0 first), then within a level by
   confidence and status (Reproduced > Source-proven > Hypothesis), then by
   how realistic the trigger is. Number findings in that order; the number
   is the rank. Open the section with a one-row-per-finding summary table
   (`# | P | confidence | status | title`) before the details, so the
   ranking is visible without reading every finding. At most ~12; put
   extra real ones in a short backlog, also ranked. For each finding:
   - **Title**, P-level, category, confidence, status.
   - Exact `path:line` (or small range), call chain, and spec reference for
     conformance claims.
   - **Trigger and impact:** concrete input/state/interleaving; expected vs.
     actual, or precise failure reasoning.
   - **Evidence:** command and output from a safe repro, or a concise
     code-based proof.
   - **Minimal fix** and **regression test**, with any compatibility tradeoff.
4. **Test-gap matrix:** behavior/risk → current coverage → proposed test →
   priority, rows sorted by priority. Actionable gaps only; acknowledge
   strong existing coverage.
5. **Engineering opportunities** (≤5): nonblocking simplification,
   architecture or performance items, each with impact, effort (S/M/L) and
   regression risk. Leave empty if nothing clears the bar.
6. **Recommended next actions:** first 3–5 small-batch actions, each with what
   verifies the fix. The project takes contributions as issues; do not create
   any.
7. **Evidence ledger:** exact commands run and observed outcomes; tests
   skipped and why; sources consulted; what was cleaned up (§6); final
   `git status --short`.

If no consequential issues exist, say so plainly; do not manufacture any.

Voice: a seasoned maintainer reviewing production systems software —
direct, precise, skeptical, respectful of the existing architecture,
ruthlessly prioritized. The output is a review, not a patch.

## 6. Clean up, then deliver

The run produces exactly one lasting file:

    <scratchpad>/posix-review/<util>-review-<YYYY-MM-DD>.md

Flat, one level below the scratchpad, so the user can find it. No
subdirectories, no extra files (no command logs, no copies, no notes)
beside it. Everything else you created is litter the user would have to
find and delete, so remove it before delivering:

- `<scratchpad>/posix-review/work/` — `rm -rf` it whole: the probe
  wrapper, repro inputs, fixtures, fake mailboxes or spools, `mktemp`
  output.
- Anything that leaked elsewhere despite §3 (a test's stray temp file, a
  tool's default `/tmp` output).
- Git state you created: branches, worktrees, stashes, tags. Return to the
  starting branch.
- Background processes you started (servers, fake daemons, watchers).

Keep: the report, `target/` build artifacts, and everything that existed
at the start (§1 snapshot) — including pre-existing working-tree changes
and earlier reports in `<scratchpad>/posix-review/`, which are not yours
to delete. Then re-run the §1 snapshot commands and diff against the
start: the only difference allowed is the one new report file. Anything
else is removed, or if it truly can't be, named in the evidence ledger.

1. Write the report to the path above (add `-2`, `-3` before `.md`
   rather than overwrite). Write it nowhere else. If the session has no
   scratchpad, write no file; the chat copy is the report.
2. Print the full report in the conversation, as the same Markdown, and
   give the saved file's full absolute path.
3. Close by offering, as a separate follow-up, to write failing regression
   tests for the confirmed P0–P2 findings and then fix them. Do not start that
   work unless the user says yes; the review itself stays read-only.
