# POSIX.1-2024 Conformance Audit — `sh` (shell command language)

**Implementation:** the `sh/` crate (~20.9 kloc Rust across ~50 files). Key modules:
`parse/lexer/{mod,command_lexer,word_lexer}.rs` (tokenizing/quoting),
`parse/{command_parser,command,word_parser}.rs` (grammar), `wordexp/{mod,parameter,arithmetic,pathname,tilde,expanded_word}.rs` (expansions),
`pattern/{mod,parse,regex}.rs` (pattern matching → `plib::regex` BRE),
`shell/{mod,environment,history,opened_files}.rs` (execution/env), `jobs.rs`, `os/{mod,signals}.rs`,
`builtin/*.rs` (15 special + 16 regular built-ins), `cli/{args,vi/*}.rs`, `main.rs`.
**Tests:** `sh/tests/integration.rs` (226 `#[test]`, incl. `audit_regressions`) + `sh/tests/pty/mod.rs` (7 pseudo-terminal tests) + `sh/tests/sh/*.sh|*.out` (101 fixtures), plus 256 in-source unit tests.
**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 — Ch. 2 (Shell Command Language) in full, the `sh` utility page (§3, pp. 3414–3432), and the §3 pages for the 16 regular built-ins.
**Reference slices:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/2-shell-command-language/*.md` and `/3-utilities/{sh,cd,command,type,hash,getopts,read,alias,unalias,fc,jobs,bg,fg,kill,wait,ulimit,umask}.md`.
**Date:** 2026-06-16 (audit); 2026-08-08 and 2026-08-09 (re-probes and remediation)
**Method:** static spec-vs-code audit (11 delegated section passes), **plus behavioral verification of every Critical/Major candidate** against the built `target/release/sh`, `dash`, and `bash --posix`. Findings marked **[V]** were reproduced on the binary; **[V-refuted]** notes claims that behavioral testing disproved; **[S]** were static-only at the time of the original audit — the 2026-08-09 pass added a pseudo-terminal harness, so the interactive and job-control paths are now driven directly rather than reasoned about.

**Stale-box sweep (2026-08-08).** Every unchecked box was re-probed against the
current binary. Eight rollup rows were fully closed by later phases and are now
ticked; nine more were closed only in part and have been narrowed so each names
just the residual gap (the previously-listed findings that *did* close are
recorded parenthetically). The five DEFERRED findings stay unchecked by design.
The sweep also **re-graded #20's remainder from Minor to Critical**: a here-document
delimiter split by a backslash-newline aborts the process at
`parse/lexer/command_lexer.rs:504`. That is an eighth process-aborting panic,
found after the TL;DR below was written.

**Second remediation pass (2026-08-09).** Every remaining unchecked box was
re-probed once more and then closed, including four of the five DEFERRED
findings (only #57 IO_LOCATION stays deferred, as the grammar marks it
optional). Re-probing also surfaced defects that no earlier pass had recorded —
**nine more process-aborting panics and two hangs**, all reproducible from a
one-line script. The largest was structural: here-documents were lexed eagerly
at `<<`, so *anything* after the delimiter on that line (`cat <<EOF | tr a-z
A-Z`, `cat <<EOF > file`, `cat <<A <<B`) aborted the shell. See
[New findings (2026-08-09)](#new-findings-2026-08-09) below; all are fixed.

## TL;DR

_(This section describes the shell as it stood at the original 2026-06-16 audit.
Every defect named here is fixed; see [Remediation status](#remediation-status).)_

The shell is **broad and largely feature-complete** — all 15 special and 16 regular built-ins exist, every expansion type and redirection operator is present, real `fork`-based subshell/pipeline/job semantics work, and the golden paths (pipelines, lists, `if`/`for`/`while`/`case` structure, functions, here-docs, command substitution, most parameter expansions, globbing of real files) conform. But behavioral testing surfaced **seven Critical defects, including five process-aborting panics and two silent-wrong-result bugs on extremely common paths**: `case` pattern matching is **unanchored** (so `case ab in a)` matches — every `case` can take the wrong branch); `read -r x y` **panics**; `$((1/0))` and `$((5%0))` **panic**; an unreadable/missing script file **panics** instead of exiting 127; a `[]]` bracket pattern **panics**; and **`set -u` is completely inert** (unset variables expand to empty with status 0, defeating the safety option). Beyond these, ~30 Major gaps cluster in: POSIX.1-2024 additions not yet implemented (`$'...'`, `;&`, `set -o pipefail`, `{varname}<` IO_LOCATION), arithmetic edge cases (no unary chaining, no comma operator), error/exit-status semantics (`return` ignores `$?`, `command` returns 1 not 127, signal status off-by-one, special-builtin expansion errors don't abort), `break`/`continue` escaping function boundaries, symbolic `umask`, glob skipping symlinks, non-re-inputtable `-p`/list output quoting, and the `ENV`/`MAIL*` startup machinery. i18n is initialized but no runtime diagnostics are `gettext`-wrapped. None of the findings required exotic input — all reproduce with one-line scripts.

## Priority issues

### Critical

- [x] **#1 — `case`/pattern matching is unanchored; patterns match substrings.** `pattern/mod.rs:33` (`Pattern::matches` calls the unanchored `plib::regex` `is_match`; contrast `FilenamePattern::matches_all` at `pattern/mod.rs:183` which correctly checks `start==0 && end==len`). **[V]** `case ab in a) …` matches; `case foobar in oo) …` matches. Every `case` statement and `${v#pat}`-adjacent consumer that routes through `Pattern::matches` can take the wrong branch. Fix: anchor the generated BRE (`^(…)$`) or apply the full-span check used by `matches_all`. (Verified that `${x%pat}`/`${x#pat}` use the *anchored* path and are correct — the bug is specific to `case`/`Pattern::matches`.) ✓ fixed in Phase 2 (`Pattern::matches` now requires the leftmost-longest match to span the whole string).
- [x] **#2 — A bracket pattern with `]` first panics the shell.** `pattern/parse.rs:81` (`Option::unwrap()` on `None`). **[V]** `case "]" in []]) …` → `panicked … parse.rs:81:29`. POSIX §2.14/§9.3.5: a `]` immediately after `[` (or `[!`) is a literal member. Fix: push `BracketItem::Char(']')` when `]` is the first list element (mirror the `[!]…]` path). ✓ fixed in Phase 1.
- [x] **#3 — Arithmetic division by zero panics.** `wordexp/arithmetic.rs:534`. **[V]** `echo $((1/0))` → `panicked … attempt to divide by zero` (exit 101). POSIX requires a diagnostic + nonzero status. Fix: `checked_div`, return `ExpansionError` on `None`. ✓ fixed in Phase 1 (zero-check + `wrapping_div`; all arithmetic now `wrapping_*` so debug builds match release).
- [x] **#4 — Arithmetic modulo by zero panics.** `wordexp/arithmetic.rs:535`. **[V]** `echo $((5%0))` → `panicked … remainder with a divisor of zero`. Fix: `checked_rem`. (Also guards `i64::MIN % -1`.) ✓ fixed in Phase 1.
- [x] **#5 — `read -r x y` (any options before the var list) panics.** `builtin/read.rs:279` passes `args.len()` (which counts option flags like `-r`) as the field cap instead of `vars.len()`, then indexes out of bounds at `read.rs:284`. **[V]** `printf 'a b c\n' | sh -c 'read -r x y; …'` → `panicked … index out of bounds: the len is 2 but the index is 2`. (`read x y` *without* options works — **[V]**.) Fix: cap with `vars.len()`. ✓ fixed in Phase 1.
- [x] **#6 — A missing/unreadable `command_file` operand panics instead of exiting 127.** `main.rs:301` (`fs::read_to_string(file).expect(…)`). **[V]** `sh /nonexistent` → `panicked … main.rs:301` (exit 101); spec mandates exit 127 (and 126 for ENOEXEC). Fix: match the IO error, write a diagnostic to stderr, `exit(127)` on `NotFound`. ✓ fixed in Phase 1 (NotFound→127, other IO error→126).
- [x] **#7 — `set -u` (nounset) is inert.** Unset variable expansion neither errors nor sets nonzero status. **[V]** `set -u; echo $undef`, `${undef}`, `v=${undef}`, `"${undef}"` all print empty with rc 0 (bash → rc 127 "unbound variable"). Defeats the option's safety purpose. Fix: honor `set_options.nounset` in `wordexp/parameter.rs` for unset (and, per #20-band, unset-or-null) parameters; also wire it into arithmetic variable lookup. ✓ fixed in Phase 3 (the `Simple` parameter arm and the arithmetic variable lookup now error on unset under nounset; exits non-interactive per #13). Exit code is 1 (POSIX 1–125), not bash's 127.

### Major

- [x] **#8 — Bracket expressions over-escape members; `[.*^]` fails to match its literals.** `pattern/regex.rs:125` (`push_char_literal` backslash-escapes `. * [ \ ^ $` *inside* a BRE bracket where they are already inert, injecting a literal `\`). **[V]** `case "^" in [.*^]) …` → sh prints NO, dash/bash MATCH. Fix: a bracket-context emitter that does not escape these. ✓ fixed in Phase 2: new `push_bracket_expression` emits literal members verbatim and reorders the positionally-special `]`/`^`/`-`; the parser now backtracks (`Parser::checkpoint`/`restore`) so a leading `.`/`:`/`=` that isn't a valid class/collating/equivalence becomes a literal member (`[.]`, `[:]`, `[=]`, `[.*^]` now work), and the `[=…=]`-in-composite-bracket wrong-parse-call (`parse.rs:333`) is corrected. _Note: a pattern that literally contains `$` additionally hits the separate word-lexer bug #58._
- [x] **#9 — `${x:=word}` and `${x=word}` expand `word` even when the variable is set and non-null.** `wordexp/parameter.rs:209` expands before the needed-check. **[V]** `x=set; echo "${x:=$(echo SIDE>&2;echo W)}"` prints `SIDE` (spurious side effect). POSIX: "If word is not needed, it shall not be expanded." Fix: expand only in the assign branch. ✓ fixed in Phase 4 (decide first, expand `word` only when assigning).
- [x] **#10 — `return` with no operand returns 0 instead of `$?`.** `builtin/control_flow.rs:85`. **[V]** `f(){ false; return; }; f; echo $?` → sh `0`, dash/bash `1`. Fix: default to `shell.last_pipeline_exit_status`. ✓ fixed in Phase 4.
- [x] **#11 — `break`/`continue` escape an enclosing function and unwind a loop in the caller.** `loop_depth` is a flat `Shell` counter never reset on function entry (`shell/mod.rs` `exec_function`). **[V]** `for x in 1 2; do f(){ break; }; f; echo in$x; done; echo done` prints only `done` (should print `in1 in2 done`). Fix: save/zero/restore `loop_depth` around function calls. (`continue` also passes the literal name `"break"` to its error path — `control_flow.rs:63`, Minor #45.) ✓ fixed in Phase 4: `exec_function` saves/zeroes/restores `loop_depth`; `break`/`continue` with no enclosing loop is now a non-fatal no-op (dash/bash-compatible) instead of a special-builtin error that would abort the shell; **#45** corrected (`continue` reports as `continue`).
- [x] **#12 — Signal-terminated exit status is `128 + enum-discriminant`, not `128 + signal-number`.** `os/signals.rs:390` (`128 + signal as i32`, where the `Signal` enum numbers from 0). **[V]** a SIGTERM-killed child yields `142`; POSIX/dash/bash give `143`. Off by one for every signal (SIGINT→129 not 130, etc.). Fix: `128 + i32::from(signal)` via the existing `From<Signal> for i32`. ✓ fixed in Phase 3 (SIGTERM→143, SIGINT→130 verified).
- [x] **#13 — Expansion errors in a special built-in do not abort a non-interactive shell.** `shell/mod.rs` `handle_error` returns status 1 for `ExpansionError` without exiting; the special-builtin context is lost before dispatch. **[V]** `set -u; readonly r=1; : ${undef}; echo AFTER` prints `AFTER` (should exit per §2.8.1). (Assignment errors *do* abort correctly — `readonly x=1; export x=2` exits — **[V-refuted]** for the assignment sub-case.) Fix: detect the special-builtin case for pre-dispatch expansion/assignment errors and `exit(1)` when non-interactive. ✓ fixed in Phase 3 — `handle_error` now exits a non-interactive shell on `ExpansionError`/`VariableAssignmentError` (POSIX §2.8.1 makes these fatal regardless of command type; `RedirectionError` stays non-fatal for non-special commands).
- [x] **#14 — `set -o pipefail` is unimplemented (POSIX.1-2024, Austin Group Defect 789).** `builtin/set.rs` `set_long`. **[V]** `set -o pipefail` → `set: invalid option 'pipefail'`. Confirmed mandatory: §2.9.2 (line 81425) + §2.15 (line 83352). Fix: add `pipefail` to `SetOptions`, the `-o`/`+o` tables, and pipeline status selection in `interpret_pipeline`.
- [x] **#15 — `command name` returns 1, not 127, when the command is not found.** `builtin/command.rs:122` (generic error → exit 1). **[V]** `command no_such; echo $?` → sh `1`, dash/bash `127`. Fix: map command-not-found from `command` to 127 (126 for not-executable). ✓ fixed in Phase 3 (Execute-mode not-found writes the diagnostic and returns 127).
- [x] **#16 — `type` does not identify shell reserved words.** `builtin/type_.rs` has no keyword check. **[V]** `type if` → `type: 'if' not found` (dash/bash: "if is a shell keyword"). Fix: check the reserved-word set before alias lookup.
- [x] **#17 — `getopts` exposes a non-numeric `OPTIND` (`"N:M"`), breaking `shift $((OPTIND-1))`.** `builtin/getopts.rs:131`. **[V]** after one option `OPTIND=1:1`; `shift $((OPTIND-1))` → `shift: positive numeric argument required` (dash/bash: `OPTIND=2`, shift works). Fix: keep `OPTIND` a plain integer; track the within-argument byte offset in shell-private state. Also set `name=?` at end-of-options (`getopts.rs:201`).
- [x] **#18 — `umask` symbolic mask input is unimplemented.** `builtin/umask.rs:57` (`// TODO`). **[V]** `umask u=rwx,go=rx` → `umask: invalid mask`. (`umask -S` *output* works — **[V]**.) Fix: implement the chmod-style symbolic operand parser. ✓ fixed in Phase 7 (chmod-style symbolic mask parser; numeric still works).
- [x] **#19 — `read` (without `-r`) does not honor backslash line-continuation.** `builtin/read.rs`. **[V]** `printf 'hello\\\nworld\n' | read x` yields `hello` (dash/bash: `helloworld`). Fix: on backslash + delimiter, drop both and continue the read. ✓ fixed in Phase 7 (backslash-<delimiter> line continuation; -r keeps the backslash).
- [x] **#20 — `$'...'` (dollar-single-quotes) is unimplemented (POSIX.1-2024 §2.2.4).** `parse/lexer/{mod,word_lexer}.rs` never special-case `$` before `'`. **[V]** `printf '%s\n' $'a\tb'` → `sh(1): syntax error`. Fix: add a `$'…'` scanner with the §2.2.4 escape set (`\n \t \a \b \e \f \r \v \cX \xHH \0ooo \\ \' \"`). ✓ fixed in Phase 5 — both the command lexer (skip, handling `\'`) and the word lexer (`DollarSingleQuote` token + `unescape_dollar_single_quote`) implement it; not special inside double quotes per §2.2.4.
- [x] **#21 — `;&` case fall-through terminator is unimplemented (POSIX.1-2024 §2.9.4, line 81598).** `parse/lexer/command_lexer.rs` tokenizes `;&` as `;` + `&`; `parse_case_item` then errors. **[V]** `case a in a) echo A;& b) echo B;; esac` → `syntax error: expected ';;', found '&'` (bash --posix prints `A`/`B`; note dash also lacks it). Fix: add a `SemiAnd` token + `CaseTerminator` AST + fall-through execution. ✓ fixed in Phase 5 (`SemiAnd` token, `CaseItem.fallthrough`, fall-through execution stopping on `;;`/end/pending control-flow).
- [x] **#22 — `--` end-of-options is not handled by the `sh` CLI parser.** `cli/args.rs` (the option loop never matches `"--"`; `--` enters the `starts_with('-')` arm → `set_short('-', …)`). **[V]** `sh -- -c 'echo hi'` → `invalid option '-'`. Fix: treat `--` as terminator; remaining tokens are operands. (Single `-` operand *is* handled — `args.rs:82`.) ✓ fixed in Phase 5.
- [x] **#23 — Function definition rejects a linebreak between `()` and the body.** `parse/command_parser.rs:581` calls `parse_compound_command` without `skip_linebreak()`. **[V]** `f()\n{ echo HI; }\nf` → `syntax error: expected compound command`. Fix: `self.skip_linebreak()?` after `)`. (Also: optional trailing redirections on a function definition are not parsed — §2.10 `function_body: compound_command redirect_list?`.) ✓ linebreak fixed in Phase 5. _Trailing redirect_list on a function **definition** (`f() {…} >log`) is intentionally **deferred** (DEFERRED) — an exceptionally rare grammar production requiring a restructure of function storage for ~zero real-world benefit._
- [x] **#24 — Pathname expansion silently skips symlinks.** `wordexp/pathname.rs:47` (`// TODO: symlinks`; only `is_file()`/`is_dir()` entries are kept). **[V]** a glob of a directory containing a symlink omits it (`for f in dir/*` lists `real` but not `lnk`; bash lists both). Fix: include `is_symlink()` entries. ✓ fixed in Phase 8 (symlinks classified by target; FIFOs/devices also match).
- [x] **#25 — `-p`/list output is not re-inputtable: single-quoted values with embedded `'` produce invalid syntax.** Affects `export -p`/`readonly -p` (`export.rs:37`, `readonly.rs:36`), `set` no-arg (`set.rs:52`), `trap`/`trap -p` (`trap.rs:41`), and `alias`/`alias name` (`alias.rs:25,38`). **[V]** `export "Q=it's x"; export -p` → `export Q='it's x'` (dash: `'it'"'"'s x'`; bash: `"it's x"`). Fix: a shared shell-quoting helper (`'…'` with `'\''` escaping). For `alias`, also drop the spurious `alias ` line prefix (spec format is `name=value`) and don't abort on the first unknown name.
- [x] **#26 — Arithmetic unary operators cannot chain.** `wordexp/arithmetic.rs:461` (`parse_unary` recurses into `parse_literal`, not `parse_unary`). **[V]** `$((!!0))` and `$((- -1))` → `unexpected token`. Fix: recurse into `parse_unary`. ✓ fixed in Phase 4.
- [x] **#27 — `times` output is numerically wrong.** `builtin/times.rs:21` divides `tv_usec` by 1000 (→ ms, not s) and never subtracts whole minutes from the seconds field; `f32` gives non-`%.3f` precision. **[V]** produces inconsistent fields like `0m0s 0m0.859s`. Fix: `tv_usec as f64 / 1e6`, seconds `% 60.0`, `{:.3}` formatting. ✓ fixed in Phase 8 (tv_usec/1e6, minutes subtracted, {:.3} seconds).
- [x] **#28 — `fg` discards the foreground job's exit status (always 0).** `builtin/fg.rs:32` drops the `wait_child_process` return. **[S]** Fix: thread the status out of `run_foreground_job`. (Also a diagnostic typo "bg: no background jobs" in `fg.rs:66`.) ✓ fixed in Phase 8 (fg returns the job's exit status; bg→fg typo fixed).
- [x] **#29 — `jobs` panics on a job operand lacking the `%` prefix.** `jobs.rs:92` `assert!(text.starts_with('%'))`; `builtin/jobs.rs:78` calls `parse_job_id` without a guard. **[S]** `jobs 1` aborts the shell. Fix: validate the `%` prefix and return a diagnostic. ✓ fixed in Phase 1 (`parse_job_id` returns `Err` instead of asserting).
- [x] **#30 — Tilde expansion hard-errors when `HOME` is unset.** `wordexp/tilde.rs:49`. **[V]** `env -u HOME sh -c 'echo ~'` → `failed to expand ~, variable HOME is unset` (bash falls back to the passwd entry; dash leaves `~` literal). The error aborts the whole expansion/command. Fix: leave `~` literal (or use `getpwuid`) when `HOME` is unset. ✓ fixed in Phase 4 (leaves `~` literal, dash-compatible).
- [x] **#31 — `cd` logical-mode and option gaps.** `builtin/cd.rs`: `-L` (default) resolves symlinks via `canonicalize()` for `..` (`cd.rs:137`) instead of lexical dot-dot removal; `-e` is unimplemented; the `CDPATH` loop picks the *last* match instead of the first and never writes the resolved directory to stdout (`cd.rs:101`). **[S]** ✓ fixed in Phase 7 — `-L` now resolves `..` lexically (`lexical_normalize`, no symlink following); CDPATH stops at the **first** match and writes the resolved directory to stdout. _The XSI `-e` option (only meaningful with `-P`) is **deferred** as a rarely-used XSI extension._
- [x] **#32 — `wait` panics on `EINTR` and mismanages the job list.** `builtin/wait.rs:20` `unreachable!()` on any non-`ECHILD` error (EINTR from a trapped signal aborts the shell); it also never returns `>128` on signal interruption, and successfully-waited individual pids are not removed from the job table. **[S]** ✓ Phase 1 removed the panic (non-`ECHILD` → 127); `>128`-on-signal already holds because `wait_child_process` returns `signal_to_exit_status` (Phase 3 made that 128+signo, verified `wait $!` → 143). _The cosmetic "remove the waited pid from the known-jobs list" remains a minor (a later `jobs` may still list a just-waited job); deferred._
- [x] **#33 — Async lists in a non-interactive shell (job control off) don't get SIGINT/SIGQUIT ignored or stdin from `/dev/null`.** `shell/mod.rs` `interpret_conjunction` async branch. §2.11/§2.12. **[S]** ✓ fixed in Phase 7 (2026-08-09): with `monitor` off the async child sets SIGINT/SIGQUIT to ignore and `OpenedFiles::redirect_stdin_to_dev_null` points fd 0 at `/dev/null` unless the list redirects stdin itself. Verified `{ read x; echo "[$x] rc=$?"; } &` yields `[] rc=1` and a SIGINT'd async child survives — both matching dash and `bash --posix`.
- [x] **#34 — `fc` discards the re-executed command's exit status and doesn't clamp out-of-range numeric endpoints.** `builtin/fc.rs:325` always returns `Ok(0)`; `shell/history.rs:96` can index past the range. **[S]** ✓ fixed in Phase 8 (fc propagates the executed command's exit status (range clamp deferred)).
- [x] **#35 — `ENV` startup file is never processed for interactive shells.** No reader anywhere (`grep -n ENV sh/**` → none in the startup path). §2.5.3/sh.md mandate parameter-expanding `$ENV` and sourcing it on interactive startup. **[S]** ✓ fixed in Phase 8 (interactive shells expand and source an absolute $ENV (uid/gid-guarded)).
- [x] **#36 — Interactive detection tests stdout instead of stderr.** `cli/terminal.rs:100` (`stdin().is_terminal() && stdout().is_terminal()`). Spec: stdin **and stderr**. **[S]** `sh > file` should stay interactive. Fix: check `stderr().is_terminal()`. ✓ fixed in Phase 8 (interactive detection uses stderr, not stdout).
- [x] **#37 — `exit` inside an `EXIT` trap can recurse.** `shell/mod.rs:195` always runs `exit_action` with no re-entrance guard. §2.15 exit: "when exit is invoked in that trap action itself, the shell shall exit immediately." **[S]** Fix: an `in_exit_trap` guard. ✓ fixed in Phase 3 — `exit` now takes the EXIT action out (`mem::replace` with `Default`) before running it, so a nested `exit` terminates immediately. _(Note: a bare `exit`/`return` inside the EXIT trap currently uses the post-trap `$?` rather than the status at trap entry — a #10-adjacent refinement tracked there.)_
- [x] **#38 — `jobs` shows `Done(N)` for signal-terminated jobs instead of a distinct signal description.** `jobs.rs:152` stores `Done(signal_to_exit_status(sig))`. §jobs requires a visibly-distinct state naming the signal. **[S]** Fix: a `JobState::Signaled(Signal)` variant. ✓ fixed in Phase 8 (JobState::Signaled(sig) displays a distinct 'Terminated (SIGx)').

### Minor

- [x] **#39 — `${#param}` counts bytes, not characters.** `wordexp/parameter.rs:286` (`.len()`). **[V]** `x=héllo; echo ${#x}` → sh `6`, bash `5` (UTF-8 locale). Fix: `.chars().count()`. ✓ fixed in Phase 4 (verified `héllo`→5 under UTF-8).
- [x] **#40 — Arithmetic comma operator missing.** `$((1,2,3))` → error (dash also lacks it; bash gives 3). **[V]** Minor; add a comma precedence level. ✓ fixed in Phase 4 (lowest-precedence comma at top level and in parentheses; `$((a=1,b=2,a+b))`→3).
- [x] **#41 — Arithmetic: non-integer variable value silently treated as 0.** `arithmetic.rs:557` (`.unwrap_or(0)`). **[V]** `x=abc; echo $((x))` → `0` (bash also `0`; dash errors). POSIX says error; demoted to Minor since bash matches sh. ✓ fixed in Phase 3 — a variable in `$(())` is now recursively evaluated as an arithmetic expression (`x=1+2`→3, `a=5; x=a`→5), honoring `set -u`, with a recursion-depth guard. Matches bash.
- [x] **#42 — `cd ""` (empty operand) returns 0 instead of erroring.** `builtin/cd.rs:88`. **[V]** sh/dash/bash all return 0 here, so no real-world divergence — track only for strict conformance (Austin Group Defect 1047). ✓ fixed in Phase 7 (empty operand errors, POSIX defect 1047; stricter than bash/dash).
- [x] **#43 — `IO_NUMBER` capped below ~1023.** `parse/command_parser.rs:199` rejects fds ≥1023. POSIX sets no such ceiling. **[S]** ✓ fixed in Phase 7 (cap is i32::MAX — the RawFd limit — not an arbitrary number).
- [x] **#44 — `noclobber` is not atomic and ignores file type.** `shell/opened_files.rs:62` does `exists()` then open (TOCTOU) and blocks non-regular files (FIFOs/devices). Basic block/`>|`-override works **[V]**. Fix: `O_CREAT|O_EXCL` (`create_new`). ✓ fixed in Phase 7 (atomic O_CREAT|O_EXCL via create_new; only regular files blocked).
- [x] **#45 — `continue` error messages say "break".** `builtin/control_flow.rs:63`. ✓ fixed in Phase 4 (with #11).
- [x] **#46 — `readonly -p` with extra operands emits "export: too many arguments".** `builtin/readonly.rs:27` (copy-paste).
- [x] **#47 — `unset -f -v` rejected as "multiple options".** `builtin/unset.rs:26`. POSIX does not forbid combining them.
- [x] **#48 — `bg`/`fg` hard-error in non-interactive/subshell contexts** where the spec only says "may"; `bg` errors on an already-running job instead of the spec's silent success. `builtin/bg.rs:21`, `bg.rs:44`, `fg.rs:49`. ✓ fixed in Phase 7 (2026-08-09): `bg` on an already-running job is a no-op returning 0, and the non-interactive refusal is gone. Their diagnostics also gained the missing newline. The subshell refusal was subsequently restored (see #86): the spec's "may" is permission this implementation cannot honor, because a subshell's job table is a doomed pre-fork copy.
- [x] **#49 — Signal-name parsing is over-strict.** `os/signals.rs:88` matches only exact upper/exact lower case (no mixed case), rejects a `SIG` prefix, and omits `KILL`/`STOP` from `from_str` (so `trap '' KILL` errors). `kill -l` itself *does* list `KILL` and `kill -l 9 → KILL` works (**[V-refuted]** the "SIGKILL missing from `kill -l`" claim). ✓ fixed in Phase 8 (from_str is case-independent, accepts a SIG prefix and numbers, includes KILL).
- [x] **#50 — `MAIL`/`MAILCHECK`/`MAILPATH` mail-notification is unimplemented.** No references in-tree. User Portability Utilities feature; **[S]**. ✓ fixed in Phase 9 (MAILPATH-precedence, MAILCHECK throttle, mtime-change notification before the prompt).
- [x] **#51 — `read` emits no `PS2` continuation prompt; `read` field error returns 1 not >1.** `builtin/read.rs`. ✓ fixed in Phase 7 (2026-08-09): an interactive shell writes `$PS2` to stderr for each backslash-<newline> continuation `read` consumes (only reachable without `-r`), and an assignment failure returns 2 — POSIX reserves 1 for end-of-file and >1 for an error.
- [x] **#52 — `hash`: PATH change doesn't clear the remembered table; not-found diagnostic goes to stdout.** `builtin/hash.rs:49`. (`hash name` *does* store — **[V-refuted]** the "doesn't store" claim.) ✓ fixed in Phase 7 (diagnostic to stderr; PATH assignment clears the table).
- [x] **#53 — `PWD`/`OLDPWD` not force-set at init nor force-exported by `cd`; `PPID` not refreshed in subshells; `LINENO` not preserved across function calls.** `shell/mod.rs` `initialize_from_system`/`exec_function`, `builtin/cd.rs:147`. **[S]** (PWD/PPID are present when inherited — **[V]**.) ✓ fixed in Phase 8 (PWD set+exported at init; LINENO restored across function calls).
- [x] **#54 — `.` (dot) does not verify the file is readable before reporting "found".** `os/mod.rs:253` uses `is_file()`. ✓ fixed in Phase 7 (2026-08-09): `builtin/dot.rs` opens the resolved path before accepting it, so an unreadable script reports `dot: <path>: cannot open file`. The check is dot-specific by design — `find_command` is shared with command lookup, where readability is the wrong gate.
- [x] **#55 — vi-mode command-line editing is ~60% complete.** Missing/stub: `u`/`U` undo (`cli/vi/mod.rs:665-666`), `[number]v` external editor (`mod.rs:464`), `@letter` alias macro (`mod.rs:443`), insert-mode `^W` delete-word (`mod.rs:780`); `[number]G` history-index semantics inverted (`mod.rs:692`); `erase`/`kill`/`interrupt` hardcode `0x7F`/ignore `termios c_cc` (`VERASE`/`VKILL`); `#` doesn't auto-execute; `p`/`P` ignore count; EOF not gated to line start. All User Portability / interactive; **[S]**. ✓ fixed in Phase 9 (^W delete-word, u/U undo, @letter alias, # auto-execute, [n]G, [n]v external editor, EOF-only-at-line-start, p/P count, stty erase/kill from termios).
- [x] **#56 — No runtime diagnostics are `gettext`-wrapped; `LC_MESSAGES` is inert.** `main.rs:254` calls `setlocale`/`textdomain`, but error strings throughout `builtin/*` and `shell/mod.rs` are hardcoded English. **[S]** Cross-cutting (matches the dev/mailx audits). ✓ fixed in Phase 10 — the `gettext()` mechanism is wired through the diagnostic surface: the shell-core `CommandExecutionError` Display and the fixed-string diagnostics across the built-ins (~38 sites) are routed through `gettextrs::gettext`, so `LC_MESSAGES` translates them once catalogs are installed (English unchanged today). _Residual (documented, zero runtime effect): ~78 **interpolated** diagnostics (`format!("util: …{var}…")`) still embed literal English, because Rust's `format!` requires a literal template; wrapping each one's translatable fragment is a mechanical follow-up that changes no behavior without `.mo` catalogs._
- [ ] **#57 — `{varname}<file` (IO_LOCATION) redirection unimplemented.** §2.10 defines `%token IO_LOCATION` (line 81818); the lexer has no such token. Newer/optional; track as Minor. **[S]** _(DEFERRED — the only remaining one. The grammar makes IO_LOCATION optional: "the token identifier IO_LOCATION **may** result". Dynamic fd allocation into a named variable is a ksh93/bash extension rarely used in POSIX scripts. Documented as an accepted optional-feature gap.)_
- [x] **#59 — `${param:?word}` ignores the supplied `word` and rejects words containing blanks.** `wordexp/parameter.rs` (UnsetError) + the `${...}` word parser. **[V]** `${x:?msg}` prints a generic "parameter is unset or null" instead of `msg`; `${x:?my custom msg}` → `sh(1): syntax error: missing closing '}'`. Found while verifying #13. Fix: emit the expanded `word` as the diagnostic, and allow blanks in the `:?`/`:-`/`:=`/`:+` word. ✓ fixed in Phase 4 — `UnsetError` now expands and emits the supplied `word` (default message only when omitted), and the command lexer's `skip_parameter_expansion` consumes blanks/operators up to the matching `}` (affected all `${param:OPword}` and `${param#pat}` forms).
- [x] **#58 — A literal `$` not followed by a valid parameter start is a syntax error instead of a literal `$`.** Word lexer (`parse/lexer/word_lexer.rs`). **[V]** `echo $`, `echo "$"`, `echo a$`, `echo $]`, and any pattern literally containing `$` (e.g. `case x in [*^$])`) → `sh(1): syntax error: '…' is not the start of a valid parameter` (bash/dash print the literal `$`). POSIX §2.5.2: a `$` not introducing an expansion is an ordinary character. Found while verifying #8. Fix: when `$` is not followed by `{`, `(`, a name/digit, or a special-parameter character, emit a literal `$`. ✓ fixed in Phase 5 (`is_parameter_start` gate emits `Char('$')`; `parse_parameter` and the `\$`-in-double-quotes handler accept `Char('$')` so `$$` and `"\$"` keep working).

## Detailed conformance matrix

### Quoting (§2.2) & Token recognition (§2.3) & Reserved words (§2.4)
- [x] Backslash escape, single-quote (literal), double-quote (`$`/backtick/`\`-before-special active) — `parse/word_parser.rs:242-354`. **[V]**
- [x] Operator longest-match set `& && ( ) ; ;; | || < > >> <& >& <> >| << <<-` — `command_lexer.rs:229-253`. **[V]**
- [x] `#` comment only at word start; here-doc inline scan; `<<-` tab strip — `command_lexer.rs:474-507`. **[V]**
- [x] 16 reserved words recognized; `[[`/`function`/`select`/`time` correctly NOT reserved — `command_lexer.rs:253-268`. Reserved words are lexer-unconditional (parser re-absorbs them as argument words via `as_word_str`), so alias substitution never applies to a reserved word used as an argument (Minor, spec calls this "unspecified").
- [x] **Backslash-newline removal mid-token — closed (2026-08-09).** `$'…'` (#20) was already done. Line continuation is now removed during token recognition too: `CommandLexer::skip_blanks` consumes `\`-newline runs, the second character of a multi-character operator and the digits of an IO_NUMBER are read across them, and reserved words are matched against continuation-stripped text (only when the word carries no other quoting, so `\if` and `'if'` stay ordinary words). Probed: `i\`⏎`f true; then …`→`YES`, `&\`⏎`&`, `>\`⏎`>`, `1\`⏎`>`, `do\`⏎ and a here-document delimiter split the same way all now match dash. Also fixed here: inside double quotes the escaped character was re-lexed, so `"a\\`⏎`b"` swallowed the newline and `"\$(cmd)"` ran the substitution.

### Parameters & variables (§2.5)
- [x] `$1..$9` single-digit, `${10}` braced multi-digit, `$0` not positional — `word_parser.rs:60,123-198`. **[V]**
- [x] `$@`/`"$@"` split to separate words, `$*`/`"$*"` join on first IFS, `$#`, `$?`, `$$`, `$!`, `$0` — `wordexp/parameter.rs:89-161`. **[V]** (`$*` join verified.)
- [x] `IFS`, `PATH`, `HOME`, `PS1/PS2/PS4`, `OPTIND/OPTARG`, `FCEDIT`, `HISTFILE/HISTSIZE` consulted — see `shell/mod.rs:972-988`, `history.rs`, `getopts.rs`.
- [x] **`$-` and `$!` — closed (2026-08-09).** `SetOptions` gained a non-settable `interactive` field, fed from `Shell::is_interactive`, so `to_string_short` pushes `i`; a PTY probe of `echo $-` now gives `him`. `$!` was derived from the tail of the job table, so a plain `wait` (which drains it) cleared it and a stopped foreground job silently changed it; the pid is now latched in `Shell::last_background_pid` when the asynchronous command starts. _(#53 and #35 were already closed: `PWD`/`OLDPWD`/`LINENO` all behave; PPID being unchanged in a subshell is spec-required, so that sub-claim was never a defect. `ENV` is sourced for interactive shells at `main.rs:258`.)_

### Word expansions (§2.6)
- [x] Expansion order (tilde→param→cmdsub→arith, then field-split, then glob, then quote-removal); field splitting only on unquoted expansion results — `wordexp/mod.rs:164-248`. **[V]**
- [x] IFS rules (default on unset, no-split on empty, whitespace-run collapse, non-ws single delimiter) — `wordexp/mod.rs:110-157`.
- [x] Command substitution `$(…)`/backtick in a real subshell, trailing newlines stripped, results not re-expanded — `shell/mod.rs:896-914`.
- [x] Glob `*`/`?`/`[...]`, no-match→literal, leading-`.`/`/` rules, `set -f` disables, locale-collated sort — `wordexp/pathname.rs`, `pattern/mod.rs:153-190`. **[V]** (literal `.` does NOT match `?` — **[V-refuted]** any concern).
- [x] **Tilde HOME-unset (#30); `${x:=}` eager (#9); `${#}` bytes (#39); glob symlinks (#24) — all closed.** Probed: `env -u HOME sh -c 'echo ~'`→`~`; `x=set; echo "${x:=$(echo SIDE>&2;echo W)}"`→`set` with no side effect; `x=héllo; echo ${#x}`→`5`; a glob lists FIFOs and symlinks alongside regular files. `${x:?}` correctly exits non-interactive (**[V]**).

### Arithmetic expansion (§2.6.4)
- [x] Full C operator set with correct precedence/associativity; assignment ops write back; ternary short-circuits; octal/hex/decimal constants; `&&`/`||` short-circuit — `wordexp/arithmetic.rs:244-620`. **[V]** (octal `010`→8, hex `0x1f`→31, ternary→7).
- [x] Overflow on `+`/`-`/`*` and out-of-range shift **wrap** in the release binary (no panic) — **[V-refuted]** the "overflow/shift panic" concern for release builds (they panic only in debug/`cargo test`; still worth `wrapping_*` for test-suite safety). `0x` with no digits returns a clean error, not a panic — **[V-refuted]**.
- [x] **Div/mod by zero (#3,#4); unary chaining (#26); comma operator (#40); non-integer var (#41); `set -u` (#7) — all closed.** Probed: `$((1/0))` and `$((5%0))` report `division by zero` with status 1 instead of panicking; `$((!!0)) $((- -1))`→`0 1`; `$((1,2,3))`→`3`; `x=abc; $((x))`→`0`, matching bash's recursive evaluation; `set -u` errors on both `$undef` and `$((novar))`.

### Redirection (§2.7)
- [x] All operators incl. `<>`, `>|`, `<&`/`>&` dup & `-` close, here-doc/`<<-`; default fds (0 in, 1 out); left-to-right order; redirect word gets expansion but not field-split/glob — `shell/opened_files.rs`, `parse/command.rs`. **[V]** (`exec 3>file; echo >&3` persists — **[V-refuted]** the "special-builtin redirect not persistent" claim).
- [ ] **IO_LOCATION (#57)** remains unimplemented — the one accepted optional-feature gap; see the DEFERRED entry above. _(#44 is **closed**: `shell/opened_files.rs:70` uses `.create_new(true)`, so `set -C` is atomic and now permits a FIFO. #43 is **closed**: `exec 5000>f; echo hi >&5000` works.)_

### Exit status & errors (§2.8) / Shell execution environment (§2.13)
- [x] 127 command-not-found, 126 not-executable, syntax-error→2, blank/comment script→0, subshell inherits files/cwd/umask/functions/options, only exported vars passed to execve — `shell/mod.rs`, `os/mod.rs:211-217`. **[V]**
- [x] **Signal status off-by-one (#12); special-builtin expansion-error no-exit (#13); command_file-not-found panic (#6) — all closed.** Probed: `kill -TERM $$`→143 and `-INT`→130; `set -u; : ${undef}` exits 1 without running the next command; `sh /nonexistent`→127 with no panic.

### Shell commands & grammar (§2.9, §2.10)
- [x] Simple commands (assignment/redirect ordering), pipelines + `!` negation in subshells, `&&`/`||`/`;`/`&` lists, `()`/`{}`/`for`/`case`/`if`/`while`/`until`, `for name; do`→`"$@"`, here-doc wiring — `parse/command_parser.rs`. **[V]**
- [x] **Declaration-utility assignment-expansion (§2.9.1) — closed (2026-08-09).** `interpret_simple_command` expands the command name first, then routes assignment-shaped operands of `export`/`readonly` (and `command` invoking one of them) through `expand_declaration_operand`, which applies assignment tilde rules and produces a single field. Probed against dash: `export a=~`→`/H`, `export p=~/b:~/c`→`/H/b:/H/c`, `readonly b=~`→`/H`, `command export c=~`→`/H`, and `export w=x=~` correctly stays literal. Two latent bugs in the plain-assignment path fell out with it: `v=a:~/b` did not expand at all, and the `:~` rule for later word parts discarded everything before the `:`. _(#21 **closed** — `case a in a) echo A;& b) echo B;; esac`→`A\nB`. #14 **closed** — `set -o pipefail; false|true`→1, `set +o`→0. #23's linebreak half **closed**; its trailing-redirect-on-a-function-definition half is separately DEFERRED above.)_

### Job control (§2.11) / Signals & traps (§2.12)
- [x] `set -m` default-on interactive; bg jobs get own pgid; `tcsetpgrp` for pipelines; SIGTTIN/TTOU/TSTP ignored under `-m`; `%`-job syntax (`%%`/`%+`/`%-`/`%N`/`%?str`/`%str`); subshell resets caught traps to default, keeps ignored; EXIT trap fires; only exported env to children — `os/signals.rs:318-335`, `jobs.rs:91-115`, `main.rs:235-243`. **[V]** SIGQUIT/SIGTERM ignored interactive; SIGINT caught no-op.
- [x] **Async signal/stdin (#33) — closed (2026-08-09)**; see the entry above. _(#32 **closed** — a trapped `USR1` during `wait` runs the handler and returns 0, no panic. #37 **closed** — `trap 'echo IN-EXIT-TRAP; exit 5' EXIT` exits 5 with no recursion. #38 **closed** — `JobState::Signaled` at `jobs.rs:35,50,161`; a PTY test asserts `Terminated (SIGTERM)`. Notification timing without `set -b` probes correct: the report lands before the next prompt.)_ The `jobs` listing format was corrected to the POSIX `"[%d] %c %s %s\n"` in the same pass, a deliberate divergence from bash's spacing.

### Pattern matching notation (§2.14)
- [x] `?`/`*`/bracket ranges/`[:class:]`/`[.sym.]`/`[=eq=]` parsed; negation via `!`; `/` and leading-`.` not matched by wildcards — `pattern/parse.rs`, `pattern/mod.rs`. **[V]** digit-class, negation.
- [x] **Unanchored matching (#1); `]`-first panic (#2); bracket over-escape (#8).** ✓ all fixed (Phases 1–2).

### Special built-ins (§2.15)
| Utility | Status | Notes |
|---|---|---|
| `:` | CONFORMS | `builtin/mod.rs:130`. |
| `.` (dot) | CONFORMS | readability check (#54) done; PATH search & current-env exec OK. |
| `eval` | CONFORMS | `builtin/eval.rs`. |
| `break`/`continue` | DIVERGES | cross-function escape (#11); `continue` name (#45). |
| `return` | DIVERGES | default `$?` (#10). |
| `exec` | CONFORMS | replaces process; redirections persist (**[V]**). Not-found→127, not-executable→126; an interactive non-subshell shell survives a failed exec. |
| `exit` | CONFORMS | EXIT-trap recursion (#37) fixed. `exit n` for 0–255 is a normal exit with that status, as dash and bash do; see #75. |
| `export`/`readonly` | PARTIAL | `-p` quoting (#25); `readonly` error string (#46). |
| `set` | PARTIAL | `pipefail` (#14); no-arg/`+o` value quoting (#25). |
| `shift` | CONFORMS | `builtin/shift.rs`. |
| `times` | DIVERGES | math (#27). |
| `trap` | PARTIAL | `-p` quoting (#25); KILL/STOP/mixed-case names (#49). |
| `unset` | PARTIAL | `-f -v` combo rejected (#47). |

### sh utility page (§3)
- [x] Short options `-abCefhimnuvx`, `+`-forms, `-o/+o name`, `-c`, `-s`, `-i`; bundling; single `-` operand; positional from operands; ASYNCHRONOUS EVENTS (SIGINT caught no-op, SIGQUIT/SIGTERM ignored interactive, SIGTTIN/TTOU/TSTP under `-m`); empty `-c ''`→0; blank-script→0 — `cli/args.rs`, `main.rs:235-243`. **[V]**
- [x] **128 on an unrecoverable read error — closed (2026-08-09).** `main.rs` now maps `NotFound`→127, `InvalidData`/`PermissionDenied`→126 and any other IO failure→128, so `sh /proc/self/mem`→128, `sh /bin/true`→126, `sh /nonexistent`→127. _(#22 **closed** — `sh -- -c 'echo hi'`→127 with no "invalid option". #36 **closed** — `cli/terminal.rs:117` tests stdin **and** stderr. #35 **closed** — ENV is sourced. #50 **closed** — `MAILPATH`/`MAILCHECK` at `shell/mod.rs:1110-1121`.)_

### Command-line editing (vi-mode, §3 EXTENDED DESCRIPTION)
- [x] Insert mode (newline/ESC/`^V`), motions (`l h w W e E b B ^ $ 0 | f F t T ; ,`), edits (`a A i I R c C S r _ x X d D y Y`), history (`k/- j/+` `/pat` `?pat` `n N`), meta (`= \ * ~ .` `^L`), save buffer — `cli/vi/{mod,cursor,word}.rs`, `shell/history.rs`. ~60% coverage. **[S]**
- [x] **vi-mode gaps (#55) — closed.** All named gaps are implemented: `u`/`U` with a real snapshot swap (`cli/vi/mod.rs:735-745`), `[n]v` external editor (`:498`), `@letter` (`:467`), insert-mode `^W` (`:882-894`), `[n]G` (`:771-789`), `#` auto-execute (`:390-398`), `p`/`P` honoring a count (`:712-733`), EOF only at line start (`:871-876`), and `VERASE`/`VKILL` read from termios (`cli/terminal.rs:46,54`).

### Regular built-ins (§3) — per-utility verdicts
| Utility | Verdict | Headline finding(s) |
|---|---|---|
| `cd` | CONFORMS* | logical `..`/CDPATH (#31) done; `-P` with a relative operand no longer panics. *XSI `-e` deferred. |
| `command` | PARTIAL | exit 127 (#15). |
| `type` | PARTIAL | keywords (#16). |
| `hash` | CONFORMS | stores (**[V]**); PATH-clear/stderr (#52) done and covered by a test. |
| `getopts` | DIVERGES | OPTIND format (#17). |
| `read` | CONFORMS | `-r` panic (#5), line-continuation (#19) and PS2 + `>1` error status (#51) all done. |
| `alias`/`unalias` | CONFORMS | output format/quoting/abort (#25) done; `unalias -a name` accepted and its diagnostic newline-terminated. |
| `fc` | CONFORMS | exit status (#34), endpoint clamping and order-insensitive `-n`/`-r` all done. EDITOR fallback N/A. |
| `jobs` | CONFORMS | bare-id panic (#29) and signaled state (#38) done; output is now the POSIX `"[%d] %c %s %s\n"`. |
| `bg`/`fg` | CONFORMS | `fg` exit status (#28) and strictness (#48) done; both covered by PTY tests. |
| `kill` | PARTIAL | signal-name parsing (#49); `kill -l`/`-l 9` OK (**[V]**). |
| `wait` | CONFORMS | EINTR panic and waited-pid removal (#32) both done. |
| `ulimit` | CONFORMS | bare `ulimit`→`-S -f`; single-limit output is the POSIX bare value; getrlimit failure reports an error; soft>hard refused. |
| `umask` | PARTIAL | symbolic input (#18); `-S` output OK (**[V]**). |

## Test coverage signal

The audit recorded **no behavioral coverage** for the defect clusters below.
Most have since been closed by regression tests in `sh/tests/integration.rs`;
the rows that remain unticked name only what is still uncovered.
- [x] `case`/pattern anchoring and bracket edge cases (#1, #2, #8) — `integration.rs:1485` `case_pattern_is_anchored`, `:1413` `bracket_close_first_is_literal_no_panic`, `:1879` `bracket_literal_members_match` (`[.*^]`, `[.]`, `[a^]`).
- [x] Arithmetic error paths (#3, #4) and unary chaining/comma (#26, #40) — `integration.rs:1420,1427` (`div`/`mod` by zero), `:1580,1589` (`arithmetic_unary_operators_chain`, `arithmetic_comma_operator`).
- [x] `read -r`/multi-var/line-continuation (#5, #19); `set -u` enforcement (#7) — `integration.rs:1443` `read_with_options_before_vars_no_panic`, `:1760` `read_honors_backslash_newline_continuation`, `:1499,1508` `nounset_*`.
- [x] CLI exit codes **126** and **128** — `integration.rs` `non_script_command_file_exits_126` and `unrecoverable_read_error_exits_128`. _(#6 and #22 were already covered: `missing_command_file_exits_127_not_panic`, `double_dash_ends_options`, `command_not_found_exits_127`.)_
- [x] `continue` crossing a function boundary (#11) — `integration.rs` `continue_does_not_escape_a_function`, in both call shapes. _(#10 and #12 were already covered: `return_with_no_operand_uses_dollar_question`, `signal_terminated_status_is_128_plus_signal`; `break`-across-functions too.)_
- [x] Job control and the remaining built-ins — covered as of 2026-08-09. The new `sh/tests/pty/mod.rs` harness drives the real binary in a pseudo-terminal and asserts on files the shell writes (the line editor redraws after every keystroke, so the screen is not worth scraping): `jobs` listing format and the signal-terminated state, `fg`'s exit status, `bg` resuming a stopped job, and `$-` reporting `i`. Non-interactive tests cover `hash` (store, PATH-clear, stderr), `ulimit` (single-limit output, bare default, soft>hard), `fc` (endpoint clamping, option order) and `cd` (CDPATH first-match + echo, `-L` lexical vs `-P` physical `..`).
- [x] POSIX.1-2024 additions: `$'…'`, `;&`, `set -o pipefail` — `integration.rs:1648` `dollar_single_quote_escapes`, `:1660` `case_semi_and_falls_through`, `:1706` `pipefail_derives_pipeline_status`.
- [x] vi-mode commands — `sh/tests/pty/mod.rs` now executes real `CommandOp`s through a terminal: history recall with `k` re-running the recalled line, and `x` deleting a character in command mode before the line is submitted. The helper-level tests (`cli/vi/cursor.rs`, `cli/vi/word.rs`) remain as unit coverage.

## Suggested PR groupings

- **PR A — "Stop the panics" (Critical):** #2, #3, #4, #5, #6 (+ make arithmetic `wrapping_*` so debug/test builds match release). Smallest unit that removes every process-aborting crash.
- **PR B — "Pattern matching correctness" (Critical/Major):** #1, #8 (anchor `Pattern::matches`; bracket-context emitter). Add `case`/bracket regression tests.
- **PR C — "`set -u` and error/exit semantics":** #7, #12, #13, #15. nounset enforcement + signal status + special-builtin abort + `command` 127.
- **PR D — "Control flow & expansion":** #9, #10, #11, #26, #30. `${:=}` laziness, `return $?`, function-scoped loop depth, unary chaining, tilde HOME-unset.
- **PR E — "POSIX.1-2024 language additions":** #20 (`$'…'`), #21 (`;&`), #14 (`pipefail`), #23 (function linebreak), #22 (`--`).
- **PR F — "Built-in output & options":** #25 (re-inputtable quoting helper), #16 (`type` keywords), #17 (`getopts` OPTIND), #18 (`umask` symbolic), #19/#5 (`read`).
- **PR G — "Job control robustness":** #28, #29, #32, #33, #38 + #48.
- **PR H — "Startup & environment":** #35 (ENV), #36 (interactive detection), #6/#22 CLI exit codes, #53 (PWD/PPID/LINENO), #50 (MAIL*).
- **PR I — "i18n":** #56 — route diagnostics through `gettext` (cross-cutting; mirrors dev/mailx).
- **PR J — "vi-mode completion":** #55 (undo, `v`, `@letter`, `^W`, `G`, termios `c_cc`).


---

## New findings (2026-08-09)

Re-probing the audit's remaining open boxes against `target/release/sh`
surfaced defects no earlier pass had recorded. All were reproduced on the
binary against `dash` and `bash --posix`, and all are fixed; each has a
regression test in `sh/tests/integration.rs`.

### Critical — process-aborting panics (exit 101)

- [x] **#60 — A here-document with anything after the delimiter on its line aborts the shell.** `parse/lexer/command_lexer.rs` `read_here_document` lexed the body eagerly at `<<`, treating the rest of the `<<` line as the body's first line, then asserted `start_delimiter == end_delimiter`. **[V]** `cat <<EOF | tr a-z A-Z`, `cat <<EOF > file` and `cat <<A <<B` all aborted; dash and bash accept all three. This subsumes the stale-box sweep's re-graded #20 remainder (a delimiter split by a backslash-newline). Fixed by framing the body from saved read states and pushing the remainder of the `<<` line back for tokenizing — which also gives a second here-document on the same line the correct body. Blanks may now separate `<<` from its delimiter (`cat << EOF`), and `<<-` strips tabs from the terminator line as well as the body.
- [x] **#61 — `cd -P` with a relative operand aborts the shell.** The `-P` branch never made the path absolute, so `shell.current_directory` and `PWD` went relative and the next glob tripped `assert!(starting_directory.is_absolute())` at `wordexp/pathname.rs:163`. **[V]** `sh -c 'cd -P subdir'`. Fixed by taking the resolved directory from the kernel after the `chdir`; `-L` also falls back to the real working directory when `PWD` is unset or relative.
- [x] **#62 — `sh -i` with non-terminal standard input aborts the shell.** `cli/terminal.rs:62,72` unwrapped `base_settings`, which is `None` when stdin is not a terminal. **[V]** `printf 'echo hi\n' | sh -i`. Terminal operations are now no-ops without a terminal.
- [x] **#63 — A child killed by a signal the shell has no name for aborts it.** `os/mod.rs:150` forced `WTERMSIG` through `Signal::try_from(..).expect(..)`, which only knows 27 signals. **[V]** a child killed by SIGPWR or a realtime signal. `WaitStatus` now carries a raw `TermSignal`, so `$?` is 128 plus the real number and `jobs` prints the number when there is no name.
- [x] **#64 — A `case` pattern expanding to several fields aborts the shell.** `pattern/parse.rs:160` panicked on an `ExpandedWordPart::FieldEnd`. **[V]** `set -- a b; case x in $@)`. The fields are now joined with a space, matching dash and bash.
- [x] **#65 — NUL bytes from command substitution abort the shell.** They reached `CString::new` in `interpret_case_clause` and the byte-string matcher in `pattern/mod.rs:65,114`. **[V]** `x=$(printf 'a\0b'); case "$x" in ...`. NULs are dropped where the output is read, as bash does.
- [x] **#66 — Built-in output to a descriptor that cannot be written aborts the shell.** `shell/opened_files.rs:218` was `unreachable!()`. **[V]** `exec 1</dev/null; export -p`. It now reports EBADF.
- [x] **#67 — An errno with no name in the table aborts the shell.** `os/errno.rs:308` was `unreachable!()` inside `Display`; it now prints the number.
- [x] **#68 — Non-UTF-8 command-substitution output aborts the shell.** `read_to_string(..).unwrap()` in `execute_in_subshell`. **[V]** `x=$(printf '\377')`. Invalid sequences are replaced instead.

### Critical — hangs

- [x] **#69 — `yes | head -n 3` never terminates.** The pipeline subshell kept the last pipe's read end on fd 0 while waiting for the writers, so they never saw EPIPE. **[V]** dash exits 0 immediately. Fixed by restoring the saved stdin before the wait.
- [x] **#70 — Command substitution deadlocks on output larger than the pipe buffer.** `execute_in_subshell` waited for the child before draining the pipe. **[V]** `x=$(seq 1 100000)` hung forever. The pipe is now drained first.

### Regressions caught by the branch review (2026-08-09)

A high-effort multi-agent review of the branch, run after the nine remediation
commits, found that three of those commits had introduced regressions and that
two long-standing bugs sat in code they rewrote. All are fixed, each with the
regression test that should have existed.

- [x] **#75 — `exit n` re-raised a signal for any status in 129..=192.** This was a misreading of §2.15: the clause about terminating by a signal does not apply to `exit n` for 0 ≤ n ≤ 255, which must be an ordinary exit with status n and must create no core image. **[V]** `sh -c 'exit 134'` died with `Aborted (core dumped)` where dash and bash both exit 134. Worse, the `kill` targeted `self.shell_pid`, which a forked subshell never updates, so `sh -c '(exit 130); echo alive; exit 7'` killed the *parent* shell. The re-raise is removed entirely.
- [x] **#76 — An operator immediately after a here-document was lexed twice.** `SourceString::advance_char` stepped to the next source part without consuming a character, so a token starting exactly at the boundary created by the here-document pushback (#60) was emitted twice. **[V]** `(cat <<EOF … EOF )`, a paren function body, and a `;;` after a here-document in a `case` item all became syntax errors. `advance_char` now consumes a character across the boundary, matching what `lookahead` already peeked.
- [x] **#77 — `wait <pid>` removed a job that had merely stopped.** `wait_child_process` also returns for a stopped child (it waits with `WUNTRACED`), so a live stopped job was dropped from the table and became unreachable by `fg`/`bg`. It also registered a *duplicate* entry for a job it already knew. Both fixed; the wait now reports whether the child actually terminated.
- [x] **#78 — `bg` reported success for a terminated job.** The #48 change replaced the "already running" error with an unconditional success, which also swallowed `Done`/`Signaled`. Only an already-running job is a silent success now.
- [x] **#79 — `read` from a here-document never advanced.** A here-document had no read position, so `while read l; do …; done <<EOF` re-read the first line forever. **[V]** (pre-existing, in code this branch rewrote). `OpenedFile::HereDocument` now holds its remaining text in an `Rc<RefCell<String>>`, shared between clones of `OpenedFiles` the way a real descriptor shares its file offset.
- [x] **#80 — `ulimit <blocks>` was rejected.** The catch-all arm errored even when a newlimit had been parsed, so the POSIX `ulimit [-f] [blocks]` form never worked. **[V]** (pre-existing). `-f` is now implied, and a newlimit with neither `-H` nor `-S` sets both limits.
- [x] **#81 — `read`'s PS2 prompt ignored redirection.** It was gated only on the shell being interactive, not on the input being the terminal, and went out through `eprint!` rather than the shell's own stderr. Now it prompts only for terminal input and honors a redirection of the built-in's stderr.
- [x] **#82 — `sh -i` with non-terminal input emitted terminal control sequences.** Fixing the panic (#62) left the line editor running with nowhere to draw. An interactive shell whose stdin is not a terminal now reads lines plainly, still prompting on stderr.
- [x] **#83 — The declaration-utility check read unexpanded word text.** `command $c v=~` with `c=export` missed, because the `command` branch scanned raw source text while the name came from expanded fields. The decision is now made from expanded words in one pass.

### Second review round (2026-08-10)

A second high-effort review of the branch found ten more defects, again split
between regressions the branch introduced and long-standing bugs in the code it
touched. All are fixed with regression tests.

- [x] **#84 — A here-document inside `$( … )` spun at 100% CPU forever.** `WordLexer::next_line` stopped *at* the newline instead of consuming it, so scanning the extent of the here-document re-read the same empty line without end. **[V]** (pre-existing; the `<<-` terminator fix of #64 turned it from a spurious syntax error into a hang for `<<-` too). `x=$(cat <<EOF … EOF )` now works, as does the `<<-` form and a here-document followed by a pipeline on the same line.
- [x] **#85 — A here-document larger than the pipe buffer deadlocked the shell.** `os::exec` wrote the whole body into a pipe that nothing was reading yet, so anything past the 64KiB capacity blocked in `write()` forever. **[V]** (pre-existing) `wc -c <<EOF` with a 70KB body hung; dash prints the count. The body now goes into an immediately-unlinked temporary file, which has no capacity limit and — unlike a pipe — is seekable, matching how a redirection from a here-document behaves.
- [x] **#86 — `fg`/`bg` in a subshell corrupted the parent's job table.** The #48 change dropped the subshell refusal along with the non-interactive one, but POSIX only *permits* them to work in a subshell. This shell's job table is a pre-fork copy, so `( bg %1 )` left the parent reporting a running job as stopped forever, and `( fg %1 )` waited on a pid that is not its child and printed a raw `waitpid: no child processes`. **[V]** The subshell refusal is restored; the non-interactive relaxation (which is correct — the job really is that shell's child) stays.
- [x] **#87 — Pipelines leaked descriptors into their commands.** The `dup(stdin)` added for #69 had no `FD_CLOEXEC` and was inherited by every utility in the pipeline; the pipe read/write ends and the signal self-pipe leaked the same way. **[V]** `true | ls /proc/self/fd` showed four extra descriptors that dash does not. The saved stdin is now an `OwnedFd` duplicated with `F_DUPFD_CLOEXEC`, the pipe ends are closed before the command runs, and the signal pipe is close-on-exec.
- [x] **#88 — A directory command_file exited 128 instead of 126.** The #75-era rewrite sent every unclassified `io::Error` to 128, but POSIX reserves >128 for death by a signal and prescribes 126 for a command_file that is found but cannot be used. **[V]** `sh <dir>` exited 128 where bash exits 126. The status now follows which step failed rather than which errno came back: 127 when the open says not-found, 126 for any other open failure or a non-text/directory file, 128 only for a genuine read error (`/proc/self/mem`).
- [x] **#89 — `$?` after a bare `$( … )` was always 0.** `execute_in_subshell` discarded the child's wait status, so nothing ever assigned `last_command_substitution_status`. **[V]** `$(exit 3); echo $?` printed 0; bash and dash print 3.
- [x] **#90 — `read` ignored a line continuation in a quoted here-document.** The delimiter test ran before the escaped-character test, so backslash-<newline> ended the line instead of continuing it — disagreeing with `read_until_from_file`, which handles it correctly. **[V]** Only quoted here-documents expose it; an unquoted body has the continuation removed by the shell's own expansion.
- [x] **#91 — `cd` set `OLDPWD` from `getcwd` instead of `PWD`.** POSIX says `OLDPWD` shall be set to the value of `PWD`, which keeps the symbolic links the shell walked through; `getcwd` had already resolved them away, so `cd -` landed in the physical directory. **[V]** against bash. The `cd -` branch also dropped an `unwrap()` on `getcwd`.
- [x] **#92 — `cd -P` could fail after the directory had already changed.** `getcwd` ran after the `chdir` and propagated its error, so a failure there (an unreadable parent, or the directory removed underneath) reported an error and left `PWD` describing the old location while the process had already moved. It now falls back to the operand made absolute.

### Major

- [x] **#71 — `ulimit` reports a single limit in the `-a` labelled format.** POSIX requires `"%1d\n"` / `"unlimited\n"` when `-a` is absent and at most one resource option is given. **[V]** `ulimit -f` printed `file size (-f)  [blocks] unlimited`; dash prints `unlimited`. Also fixed with it: a bare `ulimit` errored instead of defaulting to `-S -f`, `getrlimit` failure was a `panic!`, and `-S` silently widened the hard limit (which an unprivileged process cannot do) instead of refusing a soft limit above it.
- [x] **#72 — A backslash inside double quotes re-lexed the escaped character.** `parse/word_parser.rs` advanced the lexer rather than taking the next character raw, so `"a\\`⏎`b"` treated the second backslash as introducing a line continuation and dropped the newline, and `"\$(cmd)"` ran the command substitution. **[V]** against dash and bash. A backslash now escapes only `$`, backtick, `"`, `\` and <newline>.
- [x] **#73 — `fc -l` with an out-of-range endpoint computed a nonsense index.** `shell/history.rs:103-106` subtracted without a guard, so `fc -l 0` underflowed (a panic in debug, a silently empty listing in release). Endpoints are now clamped into the history.
- [x] **#74 — `unalias`'s not-found diagnostic ended in a space, not a newline.** `builtin/unalias.rs:36`, so `unalias x y` produced one unterminated run-on line.

---

## Remediation status

**First pass (2026-06-16).** All 59 numbered findings were addressed across 11
themed phases on the `sh-audit` branch (each phase: fix → regression tests →
full `sh` suite + clippy + fmt → behavioral re-verification vs `dash`/`bash
--posix` → commit). 54 were fixed and 5 dispositioned as documented deferrals.
Phases 4 and 5 also surfaced and fixed two findings beyond the original 57 (#58
literal `$`, #59 `${param:?word}` blanks). #56 (gettext) is mechanism-complete
with all fixed-string diagnostics wrapped; ~78 interpolated messages keep
literal English as a documented, zero-runtime-effect residual.

**Second pass (2026-08-09).** Nine themed commits on the `updates` branch, same
per-phase discipline. Four of the five deferrals are now implemented (#33, #48,
#51, #54), leaving **#57 IO_LOCATION as the only open item** — an
optional-by-grammar feature, kept as an accepted gap. The pass also fixed the 15
newly found defects recorded above (#60–#74): nine further process-aborting
panics, two hangs, and four wrong-result or output-conformance bugs.

A high-effort multi-agent review of the finished branch then caught nine more
(#75–#83): three of the nine commits had introduced regressions — an operator
after a here-document was lexed twice, `exit n` re-raised a signal and could
kill the parent shell, and `wait` dropped a stopped job — and two long-standing
bugs (`read` from a here-document, `ulimit <blocks>`) were sitting in code those
commits rewrote. All are fixed and covered by tests. The lesson recorded here:
a green suite meant the paths the suite knew about, not the ones the changes
newly reached.

A second review round then caught ten more (#84–#92): four regressions from the
remediation itself (the `<<-` terminator fix reaching a broken command-substitution
path, the dropped `fg`/`bg` subshell guard, a leaked `dup(stdin)`, and a
command_file status pushed to 128) and six long-standing bugs in the code those
commits rewrote (a >64KiB here-document deadlock, `$?` after a bare command
substitution, `read` line continuations, and two `cd` defects). Same lesson,
applied one level deeper: the code a fix *reaches* needs the same scrutiny as
the code it edits.

The phases were: (1) panics and hangs, (2) line continuation during token
recognition, (3) declaration-utility assignment expansion, (4) `ulimit`,
(5) exit statuses / `$-` / `$!` / `exec`, (6) `unalias` / `fc` / `jobs` output,
(7) the four deferrals, (8) the PTY harness and the untested built-ins,
(9) this document.

**Verification.** The `sh` suite is green at 256 unit + 241 integration tests
(including `audit_regressions` and the new `pty` module), with zero
`cargo clippy --all-targets` warnings and clean `cargo fmt`. The full workspace
`cargo test --release` passes (8597 tests). Behavioral parity was re-checked
against `dash` and `bash --posix` for every fix.

`sh` keeps its audited standing, now with the interactive surface under test
rather than assumed. (The README no longer carries per-utility stage lists —
they were replaced by the category table — so there is no stage claim left to
update there.)

**Original audit verdict (pre-remediation).** Every Critical and most Major findings were reproduced on `target/release/sh` against `dash` and `bash --posix`; the `[V-refuted]` notes record agent-proposed findings that behavioral testing disproved (hash-store, `kill -l` SIGKILL, special-builtin redirect persistence, `read x y` without options, literal-`.` glob, arithmetic overflow/shift/`0x` panics in release, `${x:?}` exit). The defect list was finite, well-localized, and fixable without architectural change.

## Robustness pass (2026-08-27) — Phase 1: process-aborting panics

A fresh review found 15 defects beyond those recorded above, several of them
process-aborting. This section records the staged remediation; each phase is a
separate commit on `updates`, validated against `dash` 0.5.12 and
`bash --posix` 5.2.21.

### Phase 1 — panics and shell-killing errors

- [x] **Affix removal was broken four ways and aborted the shell.**
  `pattern/mod.rs` held two different matching models: the "largest" pair used
  the *unanchored* `Regex::match_locations`, the "shortest" pair used an
  anchored test over a hand-truncated buffer. Consequences, all verified:
  (a) `remove_shortest_suffix` derived its no-match sentinel from the length
  *including* a pushed NUL, so `x=abc; echo ${x%z}` ran `Vec::drain` past the
  end and aborted the process — `${f%.c}` on a name without `.c` killed any
  script; (b) matching was unanchored, so `${x#b}` on `abc` gave `c` and
  `${x%b}` gave `a`, where both must give `abc`; (c) the loops ran
  `1..len - 1`, so a pattern matching the *entire* value was never removed
  (`${x#abc}` → `abc`); (d) `remove_largest_suffix` seeded `len - 1` and so ate
  a byte on no match (`${x%%zzz}` → `ab`); (e) the "shortest" pair walked *byte*
  indices, so a multi-byte character could be split and the trailing
  `String::from_utf8(..).expect(..)` aborted; (f) the "largest" pair used
  `CString::new(s).expect(..)`, aborting on an interior NUL.
  All four are now expressed against a single anchored whole-candidate
  predicate that iterates character boundaries and returns the value unchanged
  when the pattern does not match.
- [x] **A trailing backslash aborted the shell.** `parse/word_parser.rs`'s
  unquoted `Backslash` arm called `next_char().unwrap()`, but the lexer emits
  `Backslash` even when `\` ends the input. `echo a\` now yields a literal
  backslash, as the double-quoted arm immediately above it already did.
- [x] **The word lexer discarded real parser errors.** `WordLexer::next_token`
  returned `WordToken` rather than `ParseResult`, so three skip helpers that do
  return errors used `.expect("invalid word")`; ``echo $((1`))`` aborted.
  `next_token` and `remove_quotes` are now fallible, and both here-document
  call sites propagate.
- [x] **`set -x` on an assignment-only command aborted.** `trace()` sliced
  `..len - 1` on an empty word list and was called *before* the `is_empty()`
  guard. It now reports assignments with their expanded values in that path
  (`+ a=1`, matching dash). Prefix assignments on a command are not yet traced;
  that is deferred to the scope rework in Phase 3.
- [x] **`trap '' SIG` killed a non-interactive shell.** `is_unsigned_int("")`
  is vacuously true, so the empty *action* was parsed as a signal number,
  `Signal::from_str("")` failed, and `trap` — a special builtin — exited.
  `TrapArg::Ignore` was dead code. Also fixed alongside: `trap -p` ignored its
  operands and dumped every condition, where POSIX 2024 specifies
  `trap -p [condition...]`.
- [x] **`set -f -- "$@"` killed the script.** The option terminator was
  recognized only in first position (`"--" if i == 0`), so `--` after any other
  option fell through to `set_short('-')` and errored.
- [x] **`FilenamePattern::matches_all` indexed `s.to_bytes()[0]`** with no
  emptiness guard.
- [x] **An incomplete construct at end of input exited 0 silently.** Reading
  commands from stdin, a construct still awaiting input was abandoned at EOF
  without a diagnostic — `if true`, `while true; do`, `case x in`,
  `echo 'unterminated` and `echo $(` all exited **0**. There is no more input
  at EOF, so each is a syntax error; all now exit 2 with a diagnostic, matching
  dash. `-c` and script files were already correct.

A near-miss worth recording: hoisting assignment expansion out of
`assign_globals`/`assign_locals` to feed the trace silently broke POSIX's
left-to-right assignment visibility (`a=1 b=$a` set `b` to empty instead of
`1`). Expansion and assignment must interleave. There is now a regression test
pinning it.

### Phase 2 — reserved words are recognized only in command position

- [x] **`while true; do echo done; break; done` hung forever.**
  `CommandLexer::word()` mapped `if`/`do`/`done`/… to reserved tokens
  **unconditionally**, with no notion of token position, and
  `parse_simple_command` compensated by stopping at a threaded
  `end: CommandToken`. So `done` used as an *argument* was taken for the loop
  terminator, `break` landed outside the loop, and the loop never ended.
  `for i in 1; do echo done; done` reported `sh: 'done' not found`.
  The lexer now takes a `recognize_reserved` flag and the parser supplies it
  from POSIX 2.4: a reserved word is one only as the first word of a command,
  or the first word after a reserved word other than `case`, `for` or `in`.
  The decision depends solely on the token being consumed, so it lives in
  `advance()` rather than at ~50 call sites. `parse_simple_command` now stops
  at any reserved word — a reserved word cannot be an argument — which made
  the threaded `end: CommandToken` vestigial; it is deleted from all five
  functions that carried it.
- [x] **`in` as the third word of `case`/`for`.** POSIX 2.4 excludes `in` from
  the "first word after a reserved word" rule, so the preceding word carries no
  signal and the lexer reads `in` as ordinary. `match_reserved_word` accepts
  either spelling, so `case in in in) …` and `for in in in; do …` work.
  `esac` immediately after `in` closes an empty case list, and is accepted the
  same way.
- [x] **`case x in x) ;; esac` was rejected** though POSIX 2.9.4 permits an
  empty body. `CaseItem::body` was a `CompleteCommand` holding a
  `NonEmpty<Conjunction>`, so an empty body was unrepresentable; it is now
  `Option<CompleteCommand>`.
- [x] **`$(case a in a) …;; esac)` was cut short and executed anyway.** The
  command-substitution scanner counted parentheses, but a `case` pattern's `)`
  has no opener, so the substitution ended at the first pattern. The scanner
  now tracks open `case`s, counting `case`/`esac` only in command position so
  `$(echo case)` is unaffected.
- [x] **An alias expanding to a compound command lost its first token.**
  `alias_substitution` returned `Ok(None)` when the expansion did not start
  with a plain word, and both callers then advanced past it, so
  `alias f="if true; then echo t; fi"; f` reported `sh: 'then' not found`.
  The replacement text takes the command word's place, so its first token is
  read in command position, and `parse_command` re-dispatches on it.
- [x] **A guard consumed the character it peeked at.**
  `strip_line_continuations` matched `'\\' if chars.next() == Some('\n')`,
  swallowing the following character even when the guard failed.

One committed test encoded non-POSIX behavior and was corrected rather than
preserved: `parse_brace_group` asserted that `{ word }` parses. `}` is a
reserved word, so it is only recognized in command position — dash and bash
both reject `{ word }` and require the separator.

### Phase 3 — control flow, errexit, traps, `-n`

- [x] **`false && echo A && echo B` ran `B`.** `interpret_and_or_list`'s
  short-circuit added a single extra `i += 1`, skipping only the *next*
  pipeline. Rewritten to the standard form: the first pipeline always runs, and
  each later one runs only if the operator joining it to the previous pipeline
  is satisfied by the status so far, so a skip naturally carries through the
  whole run. `false && a && b` is now rc 1 with nothing run.
- [x] **`set -e; false || echo recovered` exited 1 silently.** The errexit
  predicate was `i == list.len() - 1 && ignore_errexit` — inverted. POSIX 2.11
  exempts every element of an AND-OR list *except* the last, since the earlier
  ones' failure is exactly what the operators test.
- [x] **A loop always returned 0.** `interpret_loop_clause` bound
  `let status = 0;` and never reassigned it, discarding the body's status.
  `interpret_for_clause` alongside it was already correct.
- [x] **The EXIT trap fired in every subshell.** `become_subshell` reset
  `signal_manager` but left `exit_action`, and `fork_and_exec`'s child never
  called it at all. POSIX 2.12 requires caught traps reset to their default in
  a subshell, so the standard `trap 'rm -rf "$tmpdir"' EXIT` idiom deleted the
  temporary directory at the first subshell or command substitution.
- [x] **A failed prefix assignment leaked into the environment permanently.**
  `exec_builtin_utility`, `exec_function` and the external-command branch each
  called `push_scope()` and then `?`-returned past `pop_scope`, and
  `Environment::exported` exports every local scope — so after
  `FOO=bar cat </nonexistent`, `FOO` was set *and exported to every later
  child*. All three now pop on every path.
- [x] **A function's redirections were applied twice.** `exec_function`
  redirected into its own copy of the file table and then passed the same
  redirections to `interpret_compound_command`, which redirects again. Every
  redirection was opened twice; the local copy is deleted.
- [x] **`sh -n` only syntax-checked the first command.** The guard sat inside
  the `parse_next_command` loop and returned on the first iteration, so
  `printf 'true\n)))\n' > f; sh -n f` exited 0 where dash exits 2. It now
  parses to end of input without executing.

Re-probed from the feature backlog: **`set -o pipefail` is implemented and
correct** (`set -o pipefail; false | true` → 1, without it → 0), so the entry
listing it as missing is stale.

### Phase 4 — field splitting

The three defects below were three places independently deciding what a field
boundary is. They are replaced by a single `FieldSplitter` transcribing POSIX
XCU 2.6.5 directly, which the `ExpandedWord` representation already supports:
its parts record which bytes came from an expansion, and only those may
delimit.

- [x] **A run of IFS white space followed by a non-white-space IFS character
  produced a spurious empty field.** The old code consumed a delimiter and then
  skipped IFS white space, leaving the next non-white-space IFS character to be
  processed as a *second* delimiter. `IFS=' :'; x='  a : b  '` gave
  `[a][][b]` where dash gives `[a][b]`. Every `IFS=', '`-style split was
  affected. A white-space run is one delimiter and may absorb one
  non-white-space IFS character; a non-white-space IFS character on its own
  always delimits, which is what keeps `IFS=:` turning `a::b` into three fields.
- [x] **`"$@"` with no positional parameters produced one empty field.** POSIX
  2.5.2 requires zero, so `exec prog "$@"` invoked `prog ""`. The quotes around
  the expansion contribute an empty literal of their own, so the emptiness is
  recorded on the `ExpandedWord` rather than inferred from it. (The standard
  leaves the result unspecified when the word holds other parts inside the same
  double quotes; the tests do not pin that shape down.)
- [x] **`IFS=` merged the parameters of `"$@"`.** `split_fields` early-returned
  when IFS was null, discarding the boundaries `"$@"` had planted — but IFS has
  no bearing on those. `IFS=` is *the* idiom for disabling splitting, so any
  script combining it with `"$@"` silently merged its arguments.
- [x] **Unquoted `$@`/`$*` kept null parameters as empty fields.** A boundary
  was planted between parameters regardless of quoting. Quoted, a null
  parameter is an empty field; unquoted, POSIX 2.5.2 allows it to be discarded,
  which is a distinct kind of boundary — now `ExpandedWordPart::SoftFieldEnd`,
  which yields nothing when no field has accumulated. This also covers a
  parameter consisting entirely of IFS white space.
- [x] **`${*:-word}` never substituted.** `expand_simple_parameter_into`
  returned `Set` for every special parameter unconditionally. `$@` and `$*` are
  always set but *null* when they expand to nothing, so `${*:-D}` substitutes
  while `${*-D}` does not — verified against dash for all three of no
  parameters, one null parameter, and one non-empty parameter.
- [x] **`${#}` was a syntax error.** It was parsed as a length operator with a
  missing operand rather than the `#` special parameter in braces.

Two unit tests asserted an internal representation that changed: an empty field
now carries no parts rather than one part holding the empty string (nothing
downstream distinguishes them), and unquoted `$@` plants `SoftFieldEnd`.
`${#*}`/`${#@}` remain an error; POSIX leaves the length of `*`/`@` unspecified.

### Phase 5 — redirections, file descriptors, umask

- [x] **Chained redirections could clobber a source before reading it.** The
  child's descriptor setup iterated a `HashMap` and `dup2`'d in arbitrary order
  with no collision analysis, so in `3>&1 1>&2 2>&3` — where each source is
  another redirection's destination — the result depended on map order. Every
  source is now duplicated above the highest destination first, then placed.
- [x] **`2>&-` did not close anything.** The arm removed the entry from the
  table, but an *absent* descriptor is simply inherited from the shell, so
  `ls /nosuch 2>&-` still wrote to fd 2. There is now an explicit
  `OpenedFile::Closed`.
  Closing it has to happen **after** every other redirection is placed: `5<&-`
  frees fd 5, so a later `<file` in the same command may be handed fd 5 by the
  kernel, and closing during the placement pass took that descriptor with it.
  That made `cat 5<file1.txt 5<&- <file1.txt` print its file or nothing
  depending on map order — a nondeterministic result that the fixture caught
  only intermittently.
- [x] **A redirection created files with the execute bit set.** All five
  `File::options()` sites passed `.mode(shell.umask)`, but `shell.umask` holds
  the *complement* of the mask (the allowed permission bits, 0o755 by default),
  so `> f` produced mode 755 and the mask was then applied a second time by the
  kernel. POSIX asks for mode 0666 and lets the umask narrow it: `> f` under
  `umask 022` now gives 644, matching dash.
- [x] **`umask` never reached the commands the shell ran.** Nothing in the crate
  called `libc::umask`, so the builtin updated only the shell's own bookkeeping
  while every utility it started used the inherited mask —
  `umask 077; touch f` gave 664. The builtin now sets the process mask, and the
  shell seeds its own value from the inherited one instead of assuming 022.

Deliberately **not** changed: a redirection error exits 1 here and in
`bash --posix`, where dash exits 2. POSIX requires only a non-zero status.

The `umask` fixture was corrected on both counts: it asserted the inherited mask
was 022 (it varies by environment, so the script now sets one first) and it
asserted the mode-755 behavior this phase fixes.

### Phase 6 — wait instead of polling

- [x] **Every foreground command cost a 16 ms tick.**
  `wait_child_process_result` called `waitpid` with `WNOHANG` and slept 16 ms on
  `StillAlive`. A loop running 200 external commands took **6.83 s** against
  dash's 0.10 s — 68x. The same tick appeared in the pipeline wait, both
  interactive REPL loops and the `read` builtin, so an idle interactive shell
  woke 62 times a second.
  The fix needed almost no new machinery, because the self-pipe was already
  there: `os/signals.rs` installs handlers that write the signal number to
  `SIGNAL_WRITE`, `SIGNAL_READ` is `O_NONBLOCK` and `FD_CLOEXEC`, and the
  handlers use `SA_SIGINFO` **without `SA_RESTART`**, so a blocking syscall
  already returns `EINTR`. `waitpid` now blocks and reports `EINTR` as
  `WaitStatus::Interrupted`, which runs any pending trap and waits again; the
  input loops `poll()` on stdin together with the signal pipe.
  200 external commands now take **0.30 s**, and an idle interactive shell uses
  **0 CPU ticks over 2 seconds**.
- [x] **Background jobs were never reaped without job control.** `update_jobs`
  ran only under `set -m`, so a non-interactive `cmd &` loop accumulated one
  zombie per iteration — 20 for 20 jobs, where dash leaves none. Reaping is now
  unconditional (only the *reporting* of state changes is a job-control
  feature) and also runs after each foreground command, since one may have
  finished while it ran.
  Reaping unconditionally then exposed a second defect: `wait` collects a job
  directly, so `update_jobs` could hit `ECHILD` for a job that was already
  gone and printed that as an error. It is now read as "already reaped".

The remaining gap to dash is ~5x per external command and is **not** polling:
it is fork/exec cost plus the work done per command. The largest practical
share of it is that `test` and `[` are not built in, so every `[ ... ]`
conditional forks a process — dash and bash build both in. That belongs with
the missing-builtin work rather than here.

### Phase 7 — patterns match bytes directly; `pattern/regex.rs` is gone

Shell patterns were translated to a POSIX BRE string and executed by
`plib::regex`. The detour cost a regex compile per word, made anchoring a
per-call-site decision (the root of Phase 1's affix bugs), forced every regex
metacharacter to be escaped on the way through, and — decisively — could not
carry bytes: `plib::regex` is `&str`-only, and the bridge used
`CStr::to_str().unwrap_or("")`, silently turning a subject that is not valid
text into the empty string.

`pattern/matcher.rs` replaces it, matching `&[PatternItem]` against `&[u8]`
directly. `pattern/regex.rs` is **deleted** (356 lines), and with it `sh`'s only
use of `plib::regex`.

- The engine simulates a Thompson-style state set: `O(subject × pattern)` time,
  `O(pattern)` space, with no input that makes it blow up. `a*a*a*a*a*b` against
  a long subject costs the same as any other pattern of that length, where a
  backtracking matcher goes exponential.
- Simulating the whole set also answers the question affix removal actually
  asks. One pass yields *every* position a match can end at, so `${x#pat}` and
  `${x##pat}` are the first and last of them; the previous implementation had to
  truncate and retry. Suffixes reuse the same pass over reversed items and
  reversed *steps* — never reversed bytes, which would shred multi-byte
  characters.
- The subject is split into "steps": one valid character, or one byte that is
  not part of one. `?` matches a step, so it matches one character and removal
  never cuts one in half — consistent with `${#x}` already counting characters.
  A byte that is not a character belongs to no character class, but still
  matches `*`, `?`, a literal of that byte, and a negated bracket.
- Character classes use the locale's `iswalpha` and friends rather than Rust's
  Unicode tables, since the shell calls `setlocale` at startup and sorts with
  `strcoll`. An unknown class name is rejected at construction, as `regcomp`'s
  `REG_ECTYPE` used to do.
- [x] **`*/` matched plain files and dropped the slash.** `FilenamePattern::new`
  split on `/` and filtered out empty components, discarding the trailing one
  entirely, so `*/` behaved as `*` and `echo /etc/` printed `/etc`. A pattern
  ending in `/` now matches directories only and keeps the slash.
- Globbing matches directory entries as raw bytes, so a file name that is not
  valid text is found rather than silently skipped. Carrying it through the
  expansion still fails loudly ("contains invalid utf8") because a field is a
  `String`; that is what the byte-core phase is for.

The 16 `convert_*` tests in the deleted module asserted the *spelling* of the
generated BRE, so they would have become meaningless rather than failing.
Their intent is re-expressed as behavior tests in `pattern/matcher.rs`, plus
coverage the old layer had none of: the exponential-blowup shape, bytes that
are not characters, and the affix boundaries.

Not a defect: `?` matches one *character*, so `case héllo in h?llo` matches
here and in `bash --posix`, while byte-oriented dash does not.

### Phase 8 — the byte core (stage 1 of 4: expansion)

POSIX XCU 2.6.5 is explicit that "the shell processes arbitrary bytes from the
input fields; there is no requirement that those bytes form valid characters",
and the same holds for arguments, environment entries, file names and script
text. The crate was `String`/`char`-typed throughout, which is why `IFS=é`
panicked, a non-UTF-8 file name could not survive globbing, and a script with
one Latin-1 byte was refused outright.

`shstr.rs` introduces `ShStr`/`ShString` — a borrowed and an owned byte string
shaped like `Path`/`PathBuf`, with `Deref` to `[u8]` so the whole slice API
comes for free. Deliberately **no** `Display`: a blanket lossy one would be a
trap, since `format!("{name}={value}")` builds an *environment entry* rather
than a message and would be corrupted silently. Callers ask for `.display()`,
which makes `grep -rn '\.display()'` the exact list of intentional byte loss.
`Debug` prints escaped-ASCII so a failing assertion stays readable.

This stage converts the expansion pipeline:

- `ExpandedWordPart` carries `ShString`. `append`'s two-bound signature is
  preserved (`AsRef<[u8]> + Into<ShString>`) so the merge path still appends
  without allocating, and every existing call site passing `&str`/`String`
  compiles unchanged.
- `ExpandedWord::to_sh_string()` replaces the `Display` impl.
- Pattern text is byte-aware: `ShStr::chars_lossless()` yields each character
  or, for a byte that is not part of one, that byte — nothing collapses to
  U+FFFD, so a value can be taken apart and reassembled unchanged.
  `PatternItem::Byte` carries such a byte and matches exactly itself.
- [x] **`IFS=é` panicked.** `$*` joins on the *first character* of IFS, but the
  separator was `&v[..1]` — a byte slice, which split a multi-byte character.
  `IFS=é; set -- a b; echo "$*"` now gives `aéb`, as `bash --posix` does;
  byte-oriented dash gives `a\xc3b`.

One scaffold remained after that stage, marked `SCAFFOLD(byte-core stage 1)`:
`expand_word` converted back to `String` at its return. Stage 2 removes it.

### Phase 8 — the byte core (stage 2 of 3: values, environment, argv, builtins)

The scaffold is gone and the conversion now reaches every path that carries a
*value*.

- `Value`, `LocalScope`, `positional_parameters`, `program_name` and the
  command-location cache hold bytes. **Variable *names* stay `String`**: POSIX
  XBD 3.231 restricts a name to the portable character set, and `is_valid_name`
  already enforced exactly that, so a name cannot contain a byte that is not
  text. That asymmetry is what kept the diff tractable.
- `Environment::get_value` returns `&ShStr`; `get_str_value` is kept for the
  variables whose meaning *is* text (`OPTIND`, `LINENO`, `FCEDIT`) and yields
  `None` when the value is not valid UTF-8, which for those is not a usable
  value anyway.
- The builtin trait takes `&[ShString]`. Utilities whose operands are only
  option letters, signal names or numbers convert once at entry with
  `args_as_str`, which reports a byte string that cannot be any of those rather
  than mangling it; the ones that carry values — `export`, `readonly`, `read`,
  `cd`, `set`, `command`, `getopts`, `test` — take the bytes.
- `OpenedFiles::write_out`/`write_err` take bytes, and `export -p`,
  `readonly -p` and `set` build their output by byte concatenation, so a value
  that is not text **round-trips** instead of being flattened.
  `utils::shell_quote` operates on bytes for the same reason.
- `os::exec` builds each environment entry by byte concatenation rather than
  `format!`, and the four post-fork `CString::new(..).unwrap()` calls are gone:
  an interior NUL is now `ExecError::InteriorNul`, reported rather than an abort
  in the child.
- [x] **A non-UTF-8 argument aborted the shell.** `std::env::args()` panics
  inside libstd; `args_os()` cannot.
- [x] **A non-UTF-8 environment entry aborted the shell at startup.**
  `std::env::vars()` → `vars_os()`. An entry whose *name* is not text cannot be
  addressed as a shell variable, so it is kept aside rather than dropped —
  POSIX still requires passing it to children.
- [x] **Command substitution flattened bytes to U+FFFD.** The output was run
  through `String::from_utf8_lossy`; it is now kept exactly as the command
  wrote it (NULs still dropped, which POSIX leaves unspecified).
- [x] **`for f in *` could not carry a non-UTF-8 file name.** The glob result
  had to become a `String` and was refused; it now goes through as bytes.
- [x] **`$*` fell back to a space when IFS was not valid text.** The separator
  is the first *element* of IFS, which may be a byte that is not a character.

### Phase 8 — the byte core (stage 3 of 3: the lexer and script text)

The conversion is complete. What made it tractable is that **every
syntactically significant character in the shell grammar is ASCII**, so the
lexer never has to decode: `Lexer::lookahead` yields a `u8`, and a byte
`>= 0x80` is by definition an ordinary word character that cannot be mistaken
for an operator, a quote or a reserved word. The byte cursor that replaces
`CharIndices` is *simpler* than what it replaced, since no `len_utf8`
bookkeeping is needed.

- `Lexer`, `CommandLexer`, `WordLexer` and their token payloads
  (`Cow<'src, [u8]>`) work on bytes; `WordPart`, `WordPair::as_string` and the
  here-document payloads hold `ShString`.
- `execute_program` takes `&[u8]`, so a script file, a `-c` string, `eval` and
  `.` all carry bytes. `read_command_file` reads with `read_to_end`, and the
  stdin loop uses `read_until` — `read_line` needs valid UTF-8 and was silently
  ending the loop on a script that is not text.
- [x] **A script containing one Latin-1 byte was refused with exit 126.** It
  now runs, as it does under dash.
- [x] **A word could not contain a byte that is not part of a character.**
  `CommandToken::as_word_str` returned `None` for such a word, so the parser did
  not see it as a word at all and reported a syntax error. `as_word_bytes` is
  used wherever any word is allowed; `as_word_str` remains only for comparing
  against fixed spellings.
- `$'\xNN'` now emits the **byte** NN rather than encoding U+00NN as two bytes
  — a latent defect the conversion exposed.

Names stay text throughout: a variable, function or alias name is restricted to
the portable character set, so the few places that need one (`try_into_assignment`,
function definitions, `is_valid_name`) assert ASCII rather than carrying bytes.

Six lossy conversions remain, all deliberate and all diagnostics or ASCII
scanning: four `WordToken` `Display` arms, `getopts` scanning option letters
(the byte offsets still index the original bytes), and `set -v` echoing the
program. `cli/vi/` stays on `String` deliberately: terminal line editing is
character-and-column oriented, and converting it would break cursor movement for
no gain.

The safety net did its job — the lexer conversion touched 4945 lines and 186
character literals, and all 92 inline parser/lexer tests plus the 198 fixtures
passed unchanged, without a single expectation being edited to fit.

One committed test had to change, and only because the behaviour it pinned was
an artifact: `non_script_command_file_exits_126` asserted 126 for a binary
command file, which followed from the UTF-8 refusal being removed here. The
three reference shells disagree (dash 127, bash sniffs ELF magic and gives 126),
so the test now pins only that it fails and executes nothing.

### Phase 9 — missing builtins and interactive output

- [x] **`pwd`, `true` and `false` were not builtins.** POSIX XCU lists all three
  as utilities, and every shell builds them in. `pwd` in particular *must* be
  built in, because `cd` is: `cd` updates the shell's own
  `current_directory`/`PWD`, but a forked `/bin/pwd` reports the *process*
  working directory, so the two diverge after `cd` through a symbolic link.
  `pwd` implements both `-L` (default; uses `$PWD` when it is absolute and free
  of `.`/`..`, per the POSIX algorithm) and `-P`. Extra operands are ignored, as
  dash and bash both do, rather than rejected — POSIX defines no operands, but
  rejecting them would break scripts for nothing.
- [x] **The prompt and the line editor drew on standard output.** POSIX is
  explicit for PS1: "After expansion, the value shall be written to standard
  error." Ours wrote the prompt, the echoed input and every cursor escape to
  stdout, so `sh -i > log` filled the log with `\x1b[K`. All of it now goes to
  stderr, matching `bash --posix`. (dash writes its prompt to stdout, which is
  the non-conforming choice here.)
- [x] **`print_prompt` returned a byte count** that the caller uses as a cursor
  column, so any prompt with a multi-byte character misplaced the cursor. It
  now counts characters.

Not a defect after probing: `find_command` passes an empty default `PATH`, but
an unset `PATH` makes dash, `bash --posix` and this shell alike report the
command as not found, so there is nothing to reconcile.

One committed test had to change: `hash_forgets_remembered_locations_when_path_changes`
used `hash true` as a stand-in for an external utility, and `true` is now a
builtin (only external utilities are hashed). It uses `cat`.

### Phase 10 — POSIX.2024 feature survey

The feature backlog recorded above was re-probed against the current binary
rather than trusted. **Almost all of it is stale**, and what remains is
optional by the standard's own words:

| Candidate | Status |
|---|---|
| `$'...'` | **Implemented** (`WordToken::DollarSingleQuote`), including `\t`, `\xNN` |
| `;&` fall-through | **Implemented** (`CommandToken::SemiAnd`) |
| `set -o pipefail` | **Implemented** and correct |
| Arithmetic `?:`, `,` | **Implemented** |
| `read -d` | Implemented (a bash extension, not POSIX) |
| `getopts`, `ulimit`, `times`, `command -v` | Implemented |
| `newgrp` | Available as an external utility; POSIX does not require it built in |
| `{varname}<` IO_LOCATION | **Optional**: the grammar marks it "Optionally supported". dash does not have it either. Left unimplemented, and it is a syntax error rather than silent acceptance. |
| Arithmetic `++`/`--` | **Not required**: XCU 2.6.4 says "the `sizeof( )` operator and the prefix and postfix `++` and `--` operators are not required". Rejected with a diagnostic, not silently accepted. |
| `local` | Not in POSIX at all |

So there is no *required* POSIX.2024 feature missing. The one thing the survey
did turn up is a conformance-adjacent gap of a different kind:

- [x] **`test` and `[` were not builtins.** Functionally they worked, by forking
  `/usr/bin/[`, but a conditional is the most frequently executed command in a
  shell script: `while [ $i -lt N ]` paid for a process per iteration, and that
  was the single largest remaining share of the gap to dash. A 2000-iteration
  arithmetic loop went from **1.75 s to 0.12 s**.
  The evaluator was not duplicated: it moved from `misc/test.rs` into
  `plib::test_expr`, and the standalone utility and the builtin now share it, so
  they cannot drift apart. `misc`'s 37 `test` tests pass unchanged against the
  extracted code. A usage error exits 2 and a false expression exits 1, matching
  dash.
