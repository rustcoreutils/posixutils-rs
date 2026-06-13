# Per-Utility POSIX Conformance Audit Playbook

A template + collected wisdom for auditing any single posixutils-rs utility against POSIX.1-2024 (IEEE Std 1003.1-2024). Distilled from the first full audit (`more`, see `display/audit.md`).

Goal of an audit: produce `<crate>/audit.md` — a checkbox-driven punch list a maintainer can work through PR by PR.

---

## 0. Inputs you always need

- **Sliced spec:** `~/tmp/posix.2024/sliced/` — see its top-level `README.md`, `INDEX.md`, `ALIASES.md`. Use the slice, never the 4107-page PDF.
- **Primary spec file:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/<util>.md`. If the utility name is weird (shell builtin `.`/`:`, posixutils-rs `pcc`→`c17`, etc.), consult `ALIASES.md` first.
- **Implementation:** typically `<category>/<util>.rs` (e.g. `display/more.rs`, `file/cat.rs`). Multi-file utilities live in their own crate (`sh/`, `awk/`, `pax/`, `cc/`, `make/`, `m4/`, `editors/`).
- **Tests:** `<category>/tests/<util>/mod.rs`. Skim for coverage signal, not bug-hunting.
- **Cross-referenced spec sections** to consult when the main spec links them:
  - `xbd-base-definitions/12-utility-conventions/` — argv parsing rules (POSIX `getopt` semantics, `--`, `-` operand, `+` prefix exceptions)
  - `xbd-base-definitions/9-regular-expressions/9.3-Basic-Regular-Expressions.md` and `9.4-Extended-Regular-Expressions.md` — every search-capable utility
  - `xbd-base-definitions/8-environment-variables/` — `LANG`/`LC_*` precedence, `COLUMNS`/`LINES` semantics
  - `xrat-rationale/` — the matching appendix when "why" matters (e.g., Austin Group Defects mentioned in CHANGE HISTORY)

---

## 1. Workflow

1. **Locate spec + implementation + tests** (paths above). Verify alignment via `ALIASES.md` if name is non-obvious.
2. **Read the full per-utility spec.md.** Do not skim. The spec sections set the audit outline.
3. **Read the implementation in full.** Files >2000 lines: chunk-read, or delegate to a `feature-dev:code-explorer` subagent with an explicit template (see §5).
4. **Skim tests** for what's covered. Track gaps as "write a test" items.
5. **Verify Critical/Major claims** by reading cited line ranges directly, and by grepping for "absent" things (signals, file descriptors, env vars). Do this BEFORE writing the audit doc — never publish unverified claims.
6. **Write `<crate>/audit.md`** using the template in §6. Every actionable finding is a checkbox.
7. **Propose PR groupings** at the bottom — 3 to 6 small, themed PRs are easier to land than one mega-PR.

---

## 2. Categories every audit must cover

Walk the spec **section by section in order**. For each section, decide: CONFORMS / PARTIAL / MISSING / DIVERGES / N/A.

| Spec section | What to verify | Common pitfalls |
|---|---|---|
| **SYNOPSIS** | Option letters match; option grouping (`-abc`) works; `--` end-of-options handled; `+` prefix if spec mentions XBD 12.2 exception | Clap quirks around `+`, position-of-`-` operand, unknown-option behavior |
| **OPTIONS** | Each declared option is present AND its full semantics match | Flag parsed but never consulted in the render/exec path; only first sentence of the spec implemented |
| **OPERANDS** | `-` operand routes to stdin; missing-operand → stdin; multiple operands handled in order | `-` ambiguity when mixed with file paths; single-line stdin reads |
| **STDIN** | Only consumed when spec says so | Spurious early stdin slurp; truncation to first line |
| **INPUT FILES** | File types accepted; for interactive utilities, **where commands are read from** | Reading commands from stdin instead of stderr / `/dev/tty` |
| **ENVIRONMENT VARIABLES** | Every variable listed in the spec is read; precedence rules honored | Reading once at init and never re-checking on resize/refresh; missing `LC_*` chain |
| **ASYNCHRONOUS EVENTS** | Every signal mentioned has a handler; default-others rule respected | Missing SIGCONT/SIGWINCH; signal handling via polling instead of `signal-hook` |
| **STDOUT / STDERR** | Content target, diagnostic target, **prompt target** | Prompt written to stdout instead of stderr; missing required prompt fields (filename, EOF, next-file) |
| **OUTPUT FILES** | Usually "None" — quick check | — |
| **EXTENDED DESCRIPTION** | Utility-specific rendering / behavior rules | Multi-column character handling, control-char display, fold/wrap rules |
| **Interactive commands** | One row per command; count-prefix; semantics | `''`-style "last position" trackers not updated; commands that move >1 screenful not flagged as "large movement" |
| **EXIT STATUS** | 0 success / >0 error mapping | Always exits 0; doesn't propagate per-file errors |
| **CONSEQUENCES OF ERRORS** | Per-command error policy (continue vs abort) | `:n`/`:p`-class errors abort instead of advancing |
| **Cross-cutting** | i18n (gettext), regex flavor (BRE vs ERE), locale-driven behavior, security-sensitive syscalls (setuid/setgid) | Hardcoded English diagnostics; ERE leaks where BRE required; ignored `setgid()` return values |

---

## 3. Severity & status conventions

**Per-finding status** (use exactly these labels):

- **CONFORMS** — matches spec; pre-check the box `- [x]`.
- **PARTIAL** — present but incomplete; semantics not fully implemented. Unchecked.
- **MISSING** — not implemented at all. Unchecked.
- **DIVERGES** — implemented but contradicts the spec. Unchecked.
- **N/A** — optional/XSI/CT feature legitimately not provided; track but unchecked.

**Per-finding priority** (assign to every unchecked item):

- **Critical** — user-visible data loss, hang, crash, or "doesn't do what `man` says" on the golden path (`util | more`, `util file`). Block on these for any release pitch.
- **Major** — spec-mandated behavior absent on a common path; or wrong exit code; or wrong I/O channel; or missing signal handling for interactive utilities.
- **Minor** — strict-conformance gaps that don't bite real users today (string-vs-path comparison, locale env var coverage, undocumented flags, hardcoded English).

When in doubt, prefer the higher severity — easier to demote than to discover the bug in the field later.

---

## 4. Collected wisdom — bug patterns to grep for

These are recurring antipatterns surfaced by the `more` audit. Worth a 30-second grep on every utility.

### I/O channels

- [ ] `grep -n 'stdin\(\)' <util>.rs` — interactive utilities should NOT read commands from stdin. Spec says stderr (with `/dev/tty` fallback).
- [ ] `grep -n 'self.tty\|stdout()' <util>.rs` near prompt-writing code — prompts go to stderr per spec.
- [ ] Look for `.lines().next()` or `read_line()` where the code expects to ingest a whole file/stream — truncation bug.

### Signals

- [ ] `grep -nE 'SIGCONT|SIGWINCH|signal_hook|libc::signal' <util>.rs` — interactive utilities (more, vi, ed, ex, mailx) MUST handle SIGCONT and SIGWINCH per spec. Zero matches = MISSING.
- [ ] Window-size changes detected via polling `terminal_size()` in an input loop are DIVERGES from "asynchronous event" semantics.

### Environment variables

- [ ] `grep -nE 'env::var\("(LANG|LC_ALL|LC_CTYPE|LC_COLLATE|LC_MESSAGES|NLSPATH|COLUMNS|LINES)"' <util>.rs` — verify every var the spec lists is read.
- [ ] After a resize or refresh, the env vars `COLUMNS`/`LINES` should be re-honored. Common bug: read once at init, then ignored.
- [ ] MORE/`LESS`/`EDITOR`-style pre-option env vars: must be processed BEFORE the command line, with CLI args overriding.

### Argv / clap

- [ ] Spec says "may treat `+` as an option delimiter": clap doesn't do this natively — track as MISSING unless explicitly wired.
- [ ] Hidden test flags (`--test`, `--debug`) in the public clap surface: should be `#[arg(hide = true)]` or behind a feature gate.
- [ ] `-` operand: confirm it routes to stdin AT THAT POSITION in the file list, not just "stdin if `-` present".
- [ ] Bundled short options (`-ceisu` vs `-c -e -i -s -u`): clap handles by default; double-check for utilities written without clap.

### Path / environment-variable string comparisons

- [ ] `EDITOR == "vi"` / `EDITOR == "ex"` antipattern — won't match `/usr/bin/vi`, `vim`, `view`. Use `Path::new(&editor).file_name()`.
- [ ] Same pattern for `SHELL`, `PAGER`, `TERM`-prefix checks.

### Editor / external-command invocation

- [ ] For `vi`/`ex` invocations, POSIX mandates `-c linenumber`. Many implementations historically use the older `+N` form — **DIVERGES** from spec even though most editors accept both.
- [ ] `setuid(getuid())` / `setgid(getgid())` return values being ignored (`let _ = ...`) is a security smell worth flagging.

### Regex

- [ ] Search/match utilities default flavor: `grep -nE 'RegexFlags::|Regex::new' <util>.rs`. POSIX `grep` (no `-E`), `sed` (without `-E`), `more`/`ed`/`ex`/`vi`/`expr` use **BRE**. `awk` and `grep -E` / `egrep` use **ERE**. Easy to wire the wrong flavor.
- [ ] Tag-style lookups: if the spec says "tag" (literal name), don't use `grep` (regex). Use literal match or `grep -F`.

### i18n

- [ ] `grep -n 'gettext\|gettextrs' <util>.rs` — clap help strings often are gettext'd, but runtime diagnostic strings (errors, prompts) often aren't. POSIX requires `LC_MESSAGES` to affect diagnostic text.
- [ ] `setlocale(LC_ALL, "")` should be present near `main()`. Missing → `LC_*` env vars do nothing.

### Output / rendering

- [ ] Lines longer than the terminal: fold vs truncate vs wrap — check spec wording.
- [ ] Multi-column (wide) Unicode characters at the wrap boundary: POSIX leaves this unspecified, but truncating mid-codepoint is a crash risk.
- [ ] Backspace/underscore/overstrike → underline/bold translation: check the FULL POSIX sequence (`char + n*BS + n*'_'`), not just the n=1 case.
- [ ] `\r` at end of line: should be silently discarded before `\n`, not displayed as a control character.
- [ ] Non-printable characters: spec usually says "use the same format as `ex print`" — `^X` for control, `\NNN` octal for high bytes.

### Exit status

- [ ] `exit(0)` everywhere → never propagates errors. Should set a `had_error` flag and exit non-zero at the end.
- [ ] Per-command error policy (CONSEQUENCES OF ERRORS) is often missed: `:n`/`:p`-style errors should advance/retreat AND set non-zero exit, not abort.

---

## 5. Delegating to a subagent

For utilities >1500 lines, delegate to `feature-dev:code-explorer`. It's read-only and good at cross-referencing two large files.

Prompt template (paste, fill in `<util>`, `<path>`):

> Perform a systematic POSIX.1-2024 conformance audit of `<util>`.
>
> **Spec** (read in full): `/home/jgarzik/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/<util>.md`
> **Implementation** (read in full): `/home/jgarzik/repo/posixutils-rs/<path>/<util>.rs`
> **Tests** (skim for coverage signal): `/home/jgarzik/repo/posixutils-rs/<path>/tests/<util>/mod.rs`
>
> **Supporting spec sections to consult as referenced:**
> - `xbd-base-definitions/12-utility-conventions/` — argv rules
> - `xbd-base-definitions/9-regular-expressions/` — BRE/ERE if applicable
> - `xbd-base-definitions/8-environment-variables/` — LC_*/COLUMNS/LINES
>
> **Output sections (in order):** SYNOPSIS, OPTIONS (table), OPERANDS, STDIN, INPUT FILES, ENVIRONMENT VARIABLES (table), ASYNCHRONOUS EVENTS, STDOUT/STDERR, OUTPUT FILES, EXTENDED DESCRIPTION, Interactive commands (table — only if utility has them), EXIT STATUS, CONSEQUENCES OF ERRORS, Cross-cutting (i18n, regex flavor, signal handling).
>
> **Per finding:** cite implementation line numbers; mark CONFORMS / PARTIAL / MISSING / DIVERGES / N/A; do NOT flag things the spec leaves "implementation-defined" or "unspecified" unless they cause a crash/hang.
>
> **End with:** Summary table — top 5–10 issues ranked Critical / Major / Minor with one-line fix sketches.
>
> ~2000–3000 words. Tables liberally. No code blocks >5 lines. Read-only — do NOT modify files.

After it returns: **always** spot-check at least the Critical findings. Read the cited lines. Grep for "absent" claims. Agent summaries describe what the agent intended to find, not necessarily what's true. (The `more` audit had two such checks that both confirmed real bugs — but treat verification as mandatory, not optional.)

---

## 6. Output template — `<crate>/audit.md`

```markdown
# POSIX.1-2024 Conformance Audit — `<util>`

**Implementation:** `<path>/<util>.rs` (N lines) + `<path>/tests/<util>/mod.rs` (N lines)
**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 §3, pp. X–Y
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/<util>.md`
**Date:** YYYY-MM-DD

## TL;DR
[2-4 sentences: what works, what doesn't, headline risks.]

## Priority issues

### Critical
- [ ] **#1 — <one-line summary>.** `<file>:<line>`. Fix: <one-line sketch>.

### Major
- [ ] **#N — ...**

### Minor
- [ ] **#N — ...**

## Detailed conformance matrix

### Options
- [x] `-a` CONFORMS — ...
- [ ] **`-b` PARTIAL** — ...

### Operands / STDIN
- [x] ...
- [ ] ...

### Environment variables
- [x] `VAR` CONFORMS — ...
- [ ] **`VAR` MISSING** — ...

### Asynchronous events
- [ ] **SIGFOO MISSING** — ...

### STDOUT / STDERR
- [ ] **...**

### Interactive commands (if applicable)
- [ ] **`cmd` PARTIAL** — ...

### Extended description / rendering
- [ ] **...**

### Exit status / consequences of errors
- [x] ...
- [ ] **...**

## Test coverage signal

Not covered:
- [ ] <behavior>
- [ ] <behavior>

## Suggested PR groupings

- **PR A — "<theme>"**: #X, #Y, #Z
- **PR B — "<theme>"**: ...
```

**Rules for the doc:**

1. Every actionable item is `- [ ]` (unchecked). Future sessions will tick boxes as PRs land.
2. Things already CONFORMS are `- [x]` (pre-checked) so the doc shows what's done vs outstanding at a glance.
3. Every Critical / Major / Minor priority item gets a number (`#1`, `#2`, …) that the PR groupings can reference.
4. Every finding cites a `<file>:<line>` range. "CONFORMS" without a line is useless.
5. The doc is the contract. Don't rewrite it during fix PRs — only tick boxes and append "✓ fixed in PR #NNN" inline.

---

## 7. Anti-patterns to avoid in the audit itself

- **Don't paraphrase the spec.** Cite the section name and quote the operative `shall` verbatim when something is on the line. (The slice gives you line numbers and section headers for free.)
- **Don't audit by running a subagent on the spec alone.** It hallucinates code structure. Always pair spec + implementation.
- **Don't trust "the agent said the file doesn't contain X."** Verify with `grep`.
- **Don't propose fixes for things the spec calls "unspecified" or "implementation-defined"** — unless the current behavior crashes, hangs, or is actively wrong (negative count, division-by-zero on empty file, etc.).
- **Don't audit tests.** Tests are signal, not source-of-truth. The implementation is what conforms (or doesn't).
- **Don't bundle the fixes into the audit.** Audit = punch list. Fixes = separate, focused PRs. The whole point of the checkbox format is to defer execution.

---

## 8. After the audit

- Commit `<crate>/audit.md` (alongside source). Reviewers can compare against the spec at any time.
- When picking up the next session: read the file, choose 1–3 unchecked items that share a theme, write the fix + a test that proves the box now ticks, and tick the box in the same PR.
- If a finding turns out to be wrong on closer inspection: do NOT silently delete the box. Tick it with a one-line note (`- [x] ~~#7~~ — re-examined; actually conforms because of <reason>`). This preserves history for the next auditor.

---

## 9. Reference audits

- [`display/audit.md`](display/audit.md) — `more` (the first full pass; covers 15 priority items, 28 interactive commands, signal handling, i18n).
- [`dev/audit.md`](dev/audit.md) — `yacc`, `lex`, `ar`, `nm`, `strings`, `strip` (development utilities; shared `plib::diag`/`io`/`archive`/`locale` infrastructure).
- [`awk/audit.md`](awk/audit.md) — `awk` (full language audit, behaviorally verified; 1 Critical `close()` crash, 6 Major: `-F` escapes, byte-vs-char, printf `*`, `-f -`, field uninitialized comparison, 1024-field cap).
- [`calc/audit.md`](calc/audit.md) — `expr`, `bc` (behaviorally verified). `expr`: 4 Critical (precedence ignored, regex-not-BRE `:`, div-by-zero panic, exit status). `bc`: 2 Critical (`quit`-in-`for`-in-function panic, diagnostics on stdout), Major (no `BC_SCALE_MAX`/`BC_BASE_MAX`/`BC_DIM_MAX`, no 70-col wrap). Note: behavioral verification *refuted* several agent-proposed findings (three Critical "division/sqrt rounds" claims and the `-1^2` precedence Major) — `bc` truncates correctly and matches GNU on `-1^2`.
- [`cron/audit.md`](cron/audit.md) — `crontab`, `at`, `batch`, `crond` (the cron family; `crond` audited vs. Vixie cron + implicit `crontab`/`at`/`batch` requirements + secure-daemon practice). Headline risks: `crond` executes spool files with no ownership/permission/symlink checks; `crontab -e` truncates the existing entry; `at`/`batch` jobs are filed but never run; multi-operand `at` timespecs and the `crontab` stdin form are unsupported.
- [`m4/audit.md`](m4/audit.md) — `m4` (macro processor; behaviorally verified against the mega-PDF spec, no sliced tree available). 2 Critical panics (`index` empty-needle `windows(0)`; `eval` `/0`,`%0`), 8 Major (`eval` ignores radix/min-digit args + no octal/hex + shift-vs-relational precedence; `$#`==0 for `macro()`; `m4wrap` not rescanned; `defn` can't reproduce built-ins; recoverable errors abort the run; byte-not-character semantics). Core path (spec EXAMPLES, `$@`/`$*`/`shift`, divert/undivert, pushdef/popdef, m4exit-after-error) conforms.
- [`make/audit.md`](make/audit.md) — `make` (behaviorally verified against the mega-PDF spec, no sliced tree). 5 Critical (recipe lines containing `=` abort the parse with `EmptyIdent`; `.POSIX` rejected as unsupported; missing `include` panics; `make -k` reports failure+exit 2 on success; command-line `macro=value` operands unsupported), plus Major gaps: no `$(VAR:a=b)` substitution, no backslash-newline continuation, single-suffix inference unused, no `-j`/token-pool/`.WAIT`/`.NOTPARALLEL`, no multiple `-f`, no shell `-e`, `SHELL` env var misused, `.SUFFIXES` BTreeSet loses order, `MAKEFLAGS` ignored. Behavioral verification *refuted* four agent-proposed findings (`$(VAR)`-verbatim-to-shell, `$$` passthrough, inverted `?=`, `-include` panic).
- [`misc/audit.md`](misc/audit.md) — `true`, `false`, `test`/`[` (the `misc` crate; behaviorally verified against the mega-PDF, no sliced tree). The cleanest crate audited so far: **no Critical or Major defects**. `true`/`false` are textbook-conforming; `test` implements the full primary set, the 0/1/2/3/4/>4 argument-count precedence algorithm, the `[ … ]` form, and correct 0/1/>1 exit codes. Only real divergence: `<`/`>` compare by byte/scalar order instead of `LC_COLLATE` collation. Informational: the POSIX-2024-removed `-a`/`-o`/`(`/`)` operators survive as a >4-arg extension (legal — that path is "unspecified") but the 3-arg `-a`/`-o` form errors; integer operands are `i64`-bounded and reject leading blanks.
- [`editors/audit.md`](editors/audit.md) — `ed`, `ex`, `vi` (the editor family; Critical/Major findings behaviorally verified). Four cross-cutting themes: the `regex` crate is ERE where the spec requires BRE (`ed` does no translation at all; `ex` address searches bypass the converter); `vi`/`ex` install **zero** signal handlers (no SIGWINCH/SIGCONT/SIGHUP — resize corrupts the screen, hangup loses the buffer); `ed` never sets a non-zero exit status on command errors; and `vi`/`ex` never call `setlocale`. Other headline risks: `ed` reports an identical-result substitution as "no match", `vi -r`/`-t` hard-error and exit, and `ex -s` still sources EXINIT/`.exrc`.

When you finish a new audit, add the link here.
