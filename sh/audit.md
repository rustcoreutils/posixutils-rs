# POSIX.1-2024 Conformance Audit — `sh` (shell command language)

**Implementation:** the `sh/` crate (~20.9 kloc Rust across ~50 files). Key modules:
`parse/lexer/{mod,command_lexer,word_lexer}.rs` (tokenizing/quoting),
`parse/{command_parser,command,word_parser}.rs` (grammar), `wordexp/{mod,parameter,arithmetic,pathname,tilde,expanded_word}.rs` (expansions),
`pattern/{mod,parse,regex}.rs` (pattern matching → `plib::regex` BRE),
`shell/{mod,environment,history,opened_files}.rs` (execution/env), `jobs.rs`, `os/{mod,signals}.rs`,
`builtin/*.rs` (15 special + 16 regular built-ins), `cli/{args,vi/*}.rs`, `main.rs`.
**Tests:** `sh/tests/integration.rs` (135 `#[test]`) + `sh/tests/sh/*.sh|*.out` (100 fixtures).
**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 — Ch. 2 (Shell Command Language) in full, the `sh` utility page (§3, pp. 3414–3432), and the §3 pages for the 16 regular built-ins.
**Reference slices:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/2-shell-command-language/*.md` and `/3-utilities/{sh,cd,command,type,hash,getopts,read,alias,unalias,fc,jobs,bg,fg,kill,wait,ulimit,umask}.md`.
**Date:** 2026-06-16
**Method:** static spec-vs-code audit (11 delegated section passes), **plus behavioral verification of every Critical/Major candidate** against the built `target/release/sh`, `dash`, and `bash --posix`. Findings marked **[V]** were reproduced on the binary; **[V-refuted]** notes claims that behavioral testing disproved; **[S]** are static-only (interactive/job-control paths hard to drive in CI).

## TL;DR

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
- [ ] **#24 — Pathname expansion silently skips symlinks.** `wordexp/pathname.rs:47` (`// TODO: symlinks`; only `is_file()`/`is_dir()` entries are kept). **[V]** a glob of a directory containing a symlink omits it (`for f in dir/*` lists `real` but not `lnk`; bash lists both). Fix: include `is_symlink()` entries.
- [x] **#25 — `-p`/list output is not re-inputtable: single-quoted values with embedded `'` produce invalid syntax.** Affects `export -p`/`readonly -p` (`export.rs:37`, `readonly.rs:36`), `set` no-arg (`set.rs:52`), `trap`/`trap -p` (`trap.rs:41`), and `alias`/`alias name` (`alias.rs:25,38`). **[V]** `export "Q=it's x"; export -p` → `export Q='it's x'` (dash: `'it'"'"'s x'`; bash: `"it's x"`). Fix: a shared shell-quoting helper (`'…'` with `'\''` escaping). For `alias`, also drop the spurious `alias ` line prefix (spec format is `name=value`) and don't abort on the first unknown name.
- [x] **#26 — Arithmetic unary operators cannot chain.** `wordexp/arithmetic.rs:461` (`parse_unary` recurses into `parse_literal`, not `parse_unary`). **[V]** `$((!!0))` and `$((- -1))` → `unexpected token`. Fix: recurse into `parse_unary`. ✓ fixed in Phase 4.
- [ ] **#27 — `times` output is numerically wrong.** `builtin/times.rs:21` divides `tv_usec` by 1000 (→ ms, not s) and never subtracts whole minutes from the seconds field; `f32` gives non-`%.3f` precision. **[V]** produces inconsistent fields like `0m0s 0m0.859s`. Fix: `tv_usec as f64 / 1e6`, seconds `% 60.0`, `{:.3}` formatting.
- [ ] **#28 — `fg` discards the foreground job's exit status (always 0).** `builtin/fg.rs:32` drops the `wait_child_process` return. **[S]** Fix: thread the status out of `run_foreground_job`. (Also a diagnostic typo "bg: no background jobs" in `fg.rs:66`.)
- [x] **#29 — `jobs` panics on a job operand lacking the `%` prefix.** `jobs.rs:92` `assert!(text.starts_with('%'))`; `builtin/jobs.rs:78` calls `parse_job_id` without a guard. **[S]** `jobs 1` aborts the shell. Fix: validate the `%` prefix and return a diagnostic. ✓ fixed in Phase 1 (`parse_job_id` returns `Err` instead of asserting).
- [x] **#30 — Tilde expansion hard-errors when `HOME` is unset.** `wordexp/tilde.rs:49`. **[V]** `env -u HOME sh -c 'echo ~'` → `failed to expand ~, variable HOME is unset` (bash falls back to the passwd entry; dash leaves `~` literal). The error aborts the whole expansion/command. Fix: leave `~` literal (or use `getpwuid`) when `HOME` is unset. ✓ fixed in Phase 4 (leaves `~` literal, dash-compatible).
- [x] **#31 — `cd` logical-mode and option gaps.** `builtin/cd.rs`: `-L` (default) resolves symlinks via `canonicalize()` for `..` (`cd.rs:137`) instead of lexical dot-dot removal; `-e` is unimplemented; the `CDPATH` loop picks the *last* match instead of the first and never writes the resolved directory to stdout (`cd.rs:101`). **[S]** ✓ fixed in Phase 7 — `-L` now resolves `..` lexically (`lexical_normalize`, no symlink following); CDPATH stops at the **first** match and writes the resolved directory to stdout. _The XSI `-e` option (only meaningful with `-P`) is **deferred** as a rarely-used XSI extension._
- [ ] **#32 — `wait` panics on `EINTR` and mismanages the job list.** `builtin/wait.rs:20` `unreachable!()` on any non-`ECHILD` error (EINTR from a trapped signal aborts the shell); it also never returns `>128` on signal interruption, and successfully-waited individual pids are not removed from the job table. **[S]** _(Phase 1: `unreachable!()` panic removed — non-`ECHILD` errors now return 127; the `>128`-on-trapped-signal and pid-removal semantics remain for Phase 8.)_
- [ ] **#33 — Async lists in a non-interactive shell (job control off) don't get SIGINT/SIGQUIT ignored or stdin from `/dev/null`.** `shell/mod.rs` `interpret_conjunction` async branch. §2.11/§2.12. **[S]**
- [ ] **#34 — `fc` discards the re-executed command's exit status and doesn't clamp out-of-range numeric endpoints.** `builtin/fc.rs:325` always returns `Ok(0)`; `shell/history.rs:96` can index past the range. **[S]**
- [ ] **#35 — `ENV` startup file is never processed for interactive shells.** No reader anywhere (`grep -n ENV sh/**` → none in the startup path). §2.5.3/sh.md mandate parameter-expanding `$ENV` and sourcing it on interactive startup. **[S]**
- [ ] **#36 — Interactive detection tests stdout instead of stderr.** `cli/terminal.rs:100` (`stdin().is_terminal() && stdout().is_terminal()`). Spec: stdin **and stderr**. **[S]** `sh > file` should stay interactive. Fix: check `stderr().is_terminal()`.
- [x] **#37 — `exit` inside an `EXIT` trap can recurse.** `shell/mod.rs:195` always runs `exit_action` with no re-entrance guard. §2.15 exit: "when exit is invoked in that trap action itself, the shell shall exit immediately." **[S]** Fix: an `in_exit_trap` guard. ✓ fixed in Phase 3 — `exit` now takes the EXIT action out (`mem::replace` with `Default`) before running it, so a nested `exit` terminates immediately. _(Note: a bare `exit`/`return` inside the EXIT trap currently uses the post-trap `$?` rather than the status at trap entry — a #10-adjacent refinement tracked there.)_
- [ ] **#38 — `jobs` shows `Done(N)` for signal-terminated jobs instead of a distinct signal description.** `jobs.rs:152` stores `Done(signal_to_exit_status(sig))`. §jobs requires a visibly-distinct state naming the signal. **[S]** Fix: a `JobState::Signaled(Signal)` variant.

### Minor

- [x] **#39 — `${#param}` counts bytes, not characters.** `wordexp/parameter.rs:286` (`.len()`). **[V]** `x=héllo; echo ${#x}` → sh `6`, bash `5` (UTF-8 locale). Fix: `.chars().count()`. ✓ fixed in Phase 4 (verified `héllo`→5 under UTF-8).
- [x] **#40 — Arithmetic comma operator missing.** `$((1,2,3))` → error (dash also lacks it; bash gives 3). **[V]** Minor; add a comma precedence level. ✓ fixed in Phase 4 (lowest-precedence comma at top level and in parentheses; `$((a=1,b=2,a+b))`→3).
- [x] **#41 — Arithmetic: non-integer variable value silently treated as 0.** `arithmetic.rs:557` (`.unwrap_or(0)`). **[V]** `x=abc; echo $((x))` → `0` (bash also `0`; dash errors). POSIX says error; demoted to Minor since bash matches sh. ✓ fixed in Phase 3 — a variable in `$(())` is now recursively evaluated as an arithmetic expression (`x=1+2`→3, `a=5; x=a`→5), honoring `set -u`, with a recursion-depth guard. Matches bash.
- [x] **#42 — `cd ""` (empty operand) returns 0 instead of erroring.** `builtin/cd.rs:88`. **[V]** sh/dash/bash all return 0 here, so no real-world divergence — track only for strict conformance (Austin Group Defect 1047). ✓ fixed in Phase 7 (empty operand errors, POSIX defect 1047; stricter than bash/dash).
- [x] **#43 — `IO_NUMBER` capped below ~1023.** `parse/command_parser.rs:199` rejects fds ≥1023. POSIX sets no such ceiling. **[S]** ✓ fixed in Phase 7 (cap raised to 65535).
- [x] **#44 — `noclobber` is not atomic and ignores file type.** `shell/opened_files.rs:62` does `exists()` then open (TOCTOU) and blocks non-regular files (FIFOs/devices). Basic block/`>|`-override works **[V]**. Fix: `O_CREAT|O_EXCL` (`create_new`). ✓ fixed in Phase 7 (atomic O_CREAT|O_EXCL via create_new; only regular files blocked).
- [x] **#45 — `continue` error messages say "break".** `builtin/control_flow.rs:63`. ✓ fixed in Phase 4 (with #11).
- [x] **#46 — `readonly -p` with extra operands emits "export: too many arguments".** `builtin/readonly.rs:27` (copy-paste).
- [x] **#47 — `unset -f -v` rejected as "multiple options".** `builtin/unset.rs:26`. POSIX does not forbid combining them.
- [ ] **#48 — `bg`/`fg` hard-error in non-interactive/subshell contexts** where the spec only says "may"; `bg` errors on an already-running job instead of the spec's silent success. `builtin/bg.rs:21`, `bg.rs:44`, `fg.rs:49`.
- [ ] **#49 — Signal-name parsing is over-strict.** `os/signals.rs:88` matches only exact upper/exact lower case (no mixed case), rejects a `SIG` prefix, and omits `KILL`/`STOP` from `from_str` (so `trap '' KILL` errors). `kill -l` itself *does* list `KILL` and `kill -l 9 → KILL` works (**[V-refuted]** the "SIGKILL missing from `kill -l`" claim).
- [ ] **#50 — `MAIL`/`MAILCHECK`/`MAILPATH` mail-notification is unimplemented.** No references in-tree. User Portability Utilities feature; **[S]**.
- [ ] **#51 — `read` emits no `PS2` continuation prompt; `read` field error returns 1 not >1.** `builtin/read.rs`. **[S]** _(DEFERRED, low value: the `PS2` continuation prompt is interactive-only and the field-assignment-error exit code (1 vs >1) is a minor strict-conformance nuance.)_
- [x] **#52 — `hash`: PATH change doesn't clear the remembered table; not-found diagnostic goes to stdout.** `builtin/hash.rs:49`. (`hash name` *does* store — **[V-refuted]** the "doesn't store" claim.) ✓ fixed in Phase 7 (diagnostic to stderr; PATH assignment clears the table).
- [ ] **#53 — `PWD`/`OLDPWD` not force-set at init nor force-exported by `cd`; `PPID` not refreshed in subshells; `LINENO` not preserved across function calls.** `shell/mod.rs` `initialize_from_system`/`exec_function`, `builtin/cd.rs:147`. **[S]** (PWD/PPID are present when inherited — **[V]**.)
- [ ] **#54 — `.` (dot) does not verify the file is readable before reporting "found".** `os/mod.rs:253` uses `is_file()`. Minor diagnostic-quality issue. _(DEFERRED: an unreadable file still produces an error — only the message is less precise; `find_in_path` is shared with command lookup, where readability is the wrong gate, so a dot-specific check would be needed.)_
- [ ] **#55 — vi-mode command-line editing is ~60% complete.** Missing/stub: `u`/`U` undo (`cli/vi/mod.rs:665-666`), `[number]v` external editor (`mod.rs:464`), `@letter` alias macro (`mod.rs:443`), insert-mode `^W` delete-word (`mod.rs:780`); `[number]G` history-index semantics inverted (`mod.rs:692`); `erase`/`kill`/`interrupt` hardcode `0x7F`/ignore `termios c_cc` (`VERASE`/`VKILL`); `#` doesn't auto-execute; `p`/`P` ignore count; EOF not gated to line start. All User Portability / interactive; **[S]**.
- [ ] **#56 — No runtime diagnostics are `gettext`-wrapped; `LC_MESSAGES` is inert.** `main.rs:254` calls `setlocale`/`textdomain`, but error strings throughout `builtin/*` and `shell/mod.rs` are hardcoded English. **[S]** Cross-cutting (matches the dev/mailx audits).
- [ ] **#57 — `{varname}<file` (IO_LOCATION) redirection unimplemented.** §2.10 defines `%token IO_LOCATION` (line 81818); the lexer has no such token. Newer/optional; track as Minor. **[S]** _(DEFERRED: the grammar makes IO_LOCATION optional — "the token identifier IO_LOCATION **may** result" — and the feature (dynamic fd allocation into a named variable) is a ksh93/bash extension rarely used in POSIX scripts. Documented as an accepted optional-feature gap.)_
- [x] **#59 — `${param:?word}` ignores the supplied `word` and rejects words containing blanks.** `wordexp/parameter.rs` (UnsetError) + the `${...}` word parser. **[V]** `${x:?msg}` prints a generic "parameter is unset or null" instead of `msg`; `${x:?my custom msg}` → `sh(1): syntax error: missing closing '}'`. Found while verifying #13. Fix: emit the expanded `word` as the diagnostic, and allow blanks in the `:?`/`:-`/`:=`/`:+` word. ✓ fixed in Phase 4 — `UnsetError` now expands and emits the supplied `word` (default message only when omitted), and the command lexer's `skip_parameter_expansion` consumes blanks/operators up to the matching `}` (affected all `${param:OPword}` and `${param#pat}` forms).
- [x] **#58 — A literal `$` not followed by a valid parameter start is a syntax error instead of a literal `$`.** Word lexer (`parse/lexer/word_lexer.rs`). **[V]** `echo $`, `echo "$"`, `echo a$`, `echo $]`, and any pattern literally containing `$` (e.g. `case x in [*^$])`) → `sh(1): syntax error: '…' is not the start of a valid parameter` (bash/dash print the literal `$`). POSIX §2.5.2: a `$` not introducing an expansion is an ordinary character. Found while verifying #8. Fix: when `$` is not followed by `{`, `(`, a name/digit, or a special-parameter character, emit a literal `$`. ✓ fixed in Phase 5 (`is_parameter_start` gate emits `Char('$')`; `parse_parameter` and the `\$`-in-double-quotes handler accept `Char('$')` so `$$` and `"\$"` keep working).

## Detailed conformance matrix

### Quoting (§2.2) & Token recognition (§2.3) & Reserved words (§2.4)
- [x] Backslash escape, single-quote (literal), double-quote (`$`/backtick/`\`-before-special active) — `parse/word_parser.rs:242-354`. **[V]**
- [x] Operator longest-match set `& && ( ) ; ;; | || < > >> <& >& <> >| << <<-` — `command_lexer.rs:229-253`. **[V]**
- [x] `#` comment only at word start; here-doc inline scan; `<<-` tab strip — `command_lexer.rs:474-507`. **[V]**
- [x] 16 reserved words recognized; `[[`/`function`/`select`/`time` correctly NOT reserved — `command_lexer.rs:253-268`. Reserved words are lexer-unconditional (parser re-absorbs them as argument words via `as_word_str`), so alias substitution never applies to a reserved word used as an argument (Minor, spec calls this "unspecified").
- [ ] **`$'…'` dollar-single-quotes (#20).** Backslash-newline removal mid-token is incomplete (`mod.rs:295`, Minor).

### Parameters & variables (§2.5)
- [x] `$1..$9` single-digit, `${10}` braced multi-digit, `$0` not positional — `word_parser.rs:60,123-198`. **[V]**
- [x] `$@`/`"$@"` split to separate words, `$*`/`"$*"` join on first IFS, `$#`, `$?`, `$$`, `$!`, `$0` — `wordexp/parameter.rs:89-161`. **[V]** (`$*` join verified.)
- [x] `IFS`, `PATH`, `HOME`, `PS1/PS2/PS4`, `OPTIND/OPTARG`, `FCEDIT`, `HISTFILE/HISTSIZE` consulted — see `shell/mod.rs:972-988`, `history.rs`, `getopts.rs`.
- [ ] **`$-` omits `i` for interactive shells** (`set.rs:212`, Minor); **PWD/OLDPWD/PPID/LINENO gaps (#53)**; **ENV unprocessed (#35)**; **`$!` lifetime** — `background_jobs.current()` can change after job cleanup (`jobs.rs:212`, Minor).

### Word expansions (§2.6)
- [x] Expansion order (tilde→param→cmdsub→arith, then field-split, then glob, then quote-removal); field splitting only on unquoted expansion results — `wordexp/mod.rs:164-248`. **[V]**
- [x] IFS rules (default on unset, no-split on empty, whitespace-run collapse, non-ws single delimiter) — `wordexp/mod.rs:110-157`.
- [x] Command substitution `$(…)`/backtick in a real subshell, trailing newlines stripped, results not re-expanded — `shell/mod.rs:896-914`.
- [x] Glob `*`/`?`/`[...]`, no-match→literal, leading-`.`/`/` rules, `set -f` disables, locale-collated sort — `wordexp/pathname.rs`, `pattern/mod.rs:153-190`. **[V]** (literal `.` does NOT match `?` — **[V-refuted]** any concern).
- [ ] **Tilde HOME-unset error (#30); `${x:=}` eager (#9); `${#}` bytes (#39); glob symlinks (#24).** `${x:?}` correctly exits non-interactive (**[V]**).

### Arithmetic expansion (§2.6.4)
- [x] Full C operator set with correct precedence/associativity; assignment ops write back; ternary short-circuits; octal/hex/decimal constants; `&&`/`||` short-circuit — `wordexp/arithmetic.rs:244-620`. **[V]** (octal `010`→8, hex `0x1f`→31, ternary→7).
- [x] Overflow on `+`/`-`/`*` and out-of-range shift **wrap** in the release binary (no panic) — **[V-refuted]** the "overflow/shift panic" concern for release builds (they panic only in debug/`cargo test`; still worth `wrapping_*` for test-suite safety). `0x` with no digits returns a clean error, not a panic — **[V-refuted]**.
- [ ] **Div/mod by zero panic (#3,#4); unary chaining (#26); comma operator (#40); non-integer var silent-0 (#41); `set -u` not honored (#7).**

### Redirection (§2.7)
- [x] All operators incl. `<>`, `>|`, `<&`/`>&` dup & `-` close, here-doc/`<<-`; default fds (0 in, 1 out); left-to-right order; redirect word gets expansion but not field-split/glob — `shell/opened_files.rs`, `parse/command.rs`. **[V]** (`exec 3>file; echo >&3` persists — **[V-refuted]** the "special-builtin redirect not persistent" claim).
- [ ] **noclobber non-atomic/file-type (#44); IO_NUMBER cap (#43); IO_LOCATION (#57).**

### Exit status & errors (§2.8) / Shell execution environment (§2.13)
- [x] 127 command-not-found, 126 not-executable, syntax-error→2, blank/comment script→0, subshell inherits files/cwd/umask/functions/options, only exported vars passed to execve — `shell/mod.rs`, `os/mod.rs:211-217`. **[V]**
- [ ] **Signal status off-by-one (#12); special-builtin expansion-error no-exit (#13); command_file-not-found panic (#6).**

### Shell commands & grammar (§2.9, §2.10)
- [x] Simple commands (assignment/redirect ordering), pipelines + `!` negation in subshells, `&&`/`||`/`;`/`&` lists, `()`/`{}`/`for`/`case`/`if`/`while`/`until`, `for name; do`→`"$@"`, here-doc wiring — `parse/command_parser.rs`. **[V]**
- [ ] **`;&` (#21); function linebreak/trailing-redirect (#23); `pipefail` exit selection (#14); declaration-utility assignment-expansion** (`export a=~` etc., §2.9.1, Minor-Major).

### Job control (§2.11) / Signals & traps (§2.12)
- [x] `set -m` default-on interactive; bg jobs get own pgid; `tcsetpgrp` for pipelines; SIGTTIN/TTOU/TSTP ignored under `-m`; `%`-job syntax (`%%`/`%+`/`%-`/`%N`/`%?str`/`%str`); subshell resets caught traps to default, keeps ignored; EXIT trap fires; only exported env to children — `os/signals.rs:318-335`, `jobs.rs:91-115`, `main.rs:235-243`. **[V]** SIGQUIT/SIGTERM ignored interactive; SIGINT caught no-op.
- [ ] **Async signal/stdin (#33); wait EINTR (#32); exit-in-EXIT-trap (#37); jobs signaled-state (#38); notification timing without `set -b`** (Minor).

### Pattern matching notation (§2.14)
- [x] `?`/`*`/bracket ranges/`[:class:]`/`[.sym.]`/`[=eq=]` parsed; negation via `!`; `/` and leading-`.` not matched by wildcards — `pattern/parse.rs`, `pattern/mod.rs`. **[V]** digit-class, negation.
- [x] **Unanchored matching (#1); `]`-first panic (#2); bracket over-escape (#8).** ✓ all fixed (Phases 1–2).

### Special built-ins (§2.15)
| Utility | Status | Notes |
|---|---|---|
| `:` | CONFORMS | `builtin/mod.rs:130`. |
| `.` (dot) | PARTIAL | readability check (#54); PATH search & current-env exec OK. |
| `eval` | CONFORMS | `builtin/eval.rs`. |
| `break`/`continue` | DIVERGES | cross-function escape (#11); `continue` name (#45). |
| `return` | DIVERGES | default `$?` (#10). |
| `exec` | CONFORMS | replaces process; redirections persist (**[V]**). Interactive-non-subshell exec-failure exits (Minor). |
| `exit` | PARTIAL | EXIT-trap recursion (#37); no n>256 signal mapping (Minor). |
| `export`/`readonly` | PARTIAL | `-p` quoting (#25); `readonly` error string (#46). |
| `set` | PARTIAL | `pipefail` (#14); no-arg/`+o` value quoting (#25). |
| `shift` | CONFORMS | `builtin/shift.rs`. |
| `times` | DIVERGES | math (#27). |
| `trap` | PARTIAL | `-p` quoting (#25); KILL/STOP/mixed-case names (#49). |
| `unset` | PARTIAL | `-f -v` combo rejected (#47). |

### sh utility page (§3)
- [x] Short options `-abCefhimnuvx`, `+`-forms, `-o/+o name`, `-c`, `-s`, `-i`; bundling; single `-` operand; positional from operands; ASYNCHRONOUS EVENTS (SIGINT caught no-op, SIGQUIT/SIGTERM ignored interactive, SIGTTIN/TTOU/TSTP under `-m`); empty `-c ''`→0; blank-script→0 — `cli/args.rs`, `main.rs:235-243`. **[V]**
- [ ] **`--` end-of-options (#22); command_file-not-found→127 (#6); 126 ENOEXEC for the script operand; 128 on unrecoverable read error; interactive detection stderr (#36); ENV (#35); MAIL* (#50).**

### Command-line editing (vi-mode, §3 EXTENDED DESCRIPTION)
- [x] Insert mode (newline/ESC/`^V`), motions (`l h w W e E b B ^ $ 0 | f F t T ; ,`), edits (`a A i I R c C S r _ x X d D y Y`), history (`k/- j/+` `/pat` `?pat` `n N`), meta (`= \ * ~ .` `^L`), save buffer — `cli/vi/{mod,cursor,word}.rs`, `shell/history.rs`. ~60% coverage. **[S]**
- [ ] **vi-mode gaps (#55).**

### Regular built-ins (§3) — per-utility verdicts
| Utility | Verdict | Headline finding(s) |
|---|---|---|
| `cd` | PARTIAL | logical `..`/`-e`/CDPATH (#31). |
| `command` | PARTIAL | exit 127 (#15). |
| `type` | PARTIAL | keywords (#16). |
| `hash` | CONFORMS* | stores (**[V]**); PATH-clear/stderr (#52). |
| `getopts` | DIVERGES | OPTIND format (#17). |
| `read` | DIVERGES | `-r` panic (#5); line-continuation (#19); PS2 (#51). |
| `alias`/`unalias` | PARTIAL | output format/quoting/abort (#25); `unalias -a name` over-strict. |
| `fc` | PARTIAL | exit status & clamp (#34); `-nl` order; EDITOR fallback (N/A). |
| `jobs` | DIVERGES | bare-id panic (#29); signaled state (#38); spacing format. |
| `bg`/`fg` | PARTIAL | `fg` exit status (#28); strictness/typo (#48). |
| `kill` | PARTIAL | signal-name parsing (#49); `kill -l`/`-l 9` OK (**[V]**). |
| `wait` | PARTIAL | EINTR panic / pid removal (#32). |
| `ulimit` | PARTIAL | bare `ulimit` should default `-f` (errors instead); `panic!` on getrlimit failure; soft-raise auto-bumps hard. |
| `umask` | PARTIAL | symbolic input (#18); `-S` output OK (**[V]**). |

## Test coverage signal

The 135 integration tests + 100 fixtures cover the conforming golden paths well but have **no behavioral coverage** for the defect clusters below — every Critical reproduced trivially yet none is caught:
- [ ] `case`/pattern anchoring and bracket edge cases (#1, #2, #8) — no test asserts `case ab in a)` does *not* match, or that `[]]`/`[.*^]` work.
- [ ] Arithmetic error paths (#3, #4) and unary chaining/comma (#26, #40).
- [ ] `read -r`/multi-var/line-continuation (#5, #19); `set -u` enforcement (#7).
- [ ] CLI: missing script file (#6), `--` (#22), exit codes 126/127/128.
- [ ] `return $?` (#10), `break`/`continue` across functions (#11), signal exit status (#12).
- [ ] Job control: bg/fg/jobs/wait/kill-as-job-control, `fc`/history, `hash`, `ulimit`, `cd` CDPATH/`-P`, `getopts` OPTIND.
- [ ] POSIX.1-2024 additions: `$'…'`, `;&`, `set -o pipefail`.
- [ ] vi-mode commands (interactive; consider a scripted PTY harness).

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

**Audit only — no code was modified.** Every Critical and most Major findings were reproduced on `target/release/sh` against `dash` and `bash --posix`; the `[V-refuted]` notes record agent-proposed findings that behavioral testing disproved (hash-store, `kill -l` SIGKILL, special-builtin redirect persistence, `read x y` without options, literal-`.` glob, arithmetic overflow/shift/`0x` panics in release, `${x:?}` exit). The shell is not yet strictly conforming, but the defect list is finite, well-localized, and almost entirely fixable without architectural change.
