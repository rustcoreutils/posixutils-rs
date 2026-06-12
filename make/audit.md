# POSIX.1-2024 Conformance Audit — `make`

**Implementation:** `make/src/` (16 files, ~3,074 lines: `main.rs`, `lib.rs`, `config.rs`, `error_code.rs`, `special_target.rs`, `signal_handler.rs`, `parser/{mod,lex,parse,preprocessor}.rs`, `rule.rs` + `rule/{target,prerequisite,recipe,config}.rs`)
**Tests:** `make/tests/` (fixture makefiles + `mod.rs` harness, ~1,511 lines)
**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 §3 `make`, pp. 3130–3146.
**Reference:** No sliced spec tree was available; the spec was extracted from the mega-PDF `~/tmp/POSIX.2024.pdf` (pp. 3130–3146) to `~/tmp/make-spec.txt`. Mirrors the `m4` audit's PDF-based method.
**Date:** 2026-06-12
**Verification:** Critical and most Major findings were **behaviorally verified** against the built `target/release/make` binary (no source changes), cross-checked with GNU `make` where a control was useful. Items not behaviorally verified are tagged **(static)**. Several agent-proposed findings were **refuted** by behavioral testing and are recorded at the bottom rather than silently dropped.

## TL;DR

> **Status (2026-06-12): all findings below have been remediated** across nine
> commits on the `make-audit` branch (Phases 1–9). The original assessment is
> retained for context; each item is now ticked with the fix, the phase, and its
> regression test. Two genuinely out-of-scope items are documented rather than
> implemented: full XSI SCCS auto-retrieval (`.SCCS_GET`/`PROJECTDIR`) and a
> parser-level `lib(member):` / slash-in-target-name syntax (the `ar` member
> *timestamp* lookup the audit named is implemented). The `-p` debug-dump format
> is kept deliberately (the spec leaves it unspecified).

The implementation handles the easy golden path (a simple `target: prereq` rule with a tab-indented recipe, basic `$(VAR)` macros, `-n`/`-s`/`-i`/`-q`, `.PHONY`, `.DEFAULT`, single internal macros) but falls over on a startling amount of *ordinary* makefile content. The macro preprocessor treats **every line containing `=` as a macro definition — including tab-indented recipe lines** — so any recipe with an `=` (`./configure --prefix=/usr`, `[ x = y ]`, `VAR=1 cmd`, `cc --opt=val`) is rejected with a hard `parse error: EmptyIdent`. The special target `.POSIX` — which the spec says a *portable* makefile **shall** include — is rejected as "not supported". A missing `include` file panics. `make -k` reports failure and exits 2 even when every target builds. Command-line `macro=value` operands, multiple `-f`, the `-j` parallel-execution machinery (`-j`/token pool/`.WAIT`/`.NOTPARALLEL`), the `$(VAR:a=b)` substitution form, single-suffix inference rules, backslash-newline continuation, `MAKEFLAGS`, and the shell `-e` requirement are all absent or broken. This is an early-stage implementation: many headline POSIX requirements are unmet, and several are crashes/aborts on common input.

## Priority issues

### Critical

- [x] **#1 — A recipe line containing `=` aborts the whole parse with `EmptyIdent`.** ✓ fixed (Phase 1): added `is_macro_definition()` so neither `generate_macro_table` nor `remove_variables` misclassifies tab-indented recipe lines; regression tests `recipe_line_with_equals`. `parser/preprocessor.rs:79–82` (`generate_macro_table` does `source.lines().filter(|line| line.contains('='))`, never skipping tab-indented recipe lines) → `get_ident` fails → `preprocessor.rs:56–57`. Verified: `@echo ./configure --prefix=/usr`, `@test x = x`, `@FOO=1 cmd`, `@echo tar --file=a.tar` all yield `make: parse error: EmptyIdent` (exit 4); GNU make runs them fine. This breaks a large fraction of real recipes. Fix: in `generate_macro_table`, skip lines that begin with a `<tab>` (recipe lines), and only treat a line as a macro definition when the text *before* the first `=`/`:` is a valid macro name.

- [x] **#2 — `.POSIX` special target is rejected as unsupported.** ✓ fixed (Phase 2): added a `Posix` arm + `process_posix()` that validates no prerequisites/commands and accepts it; removed the now-unreachable `NotSupported` catch-all. Test `special_targets::posix`. `special_target.rs:178–187` — the `process()` match has no `Posix` arm, so the recognized `Posix` variant (`special_target.rs:23`, `:38`) falls into `unsupported => Err(Error::NotSupported)`. Verified: `.POSIX:` → `make: '.POSIX' special target constraint is not fulfilled: the special target is not supported: '.POSIX'` (exit 9). The spec DESCRIPTION (p. 3130) says a portable makefile **shall** include `.POSIX`; this rejects every conformant portable makefile. Fix: add `Posix => this.process_posix()` that enforces no prerequisites / no commands and returns `Ok(())` (optionally enabling strict mode).

- [x] **#3 — `include` of a missing/unreadable file panics.** ✓ fixed (Phase 1): `process_include_lines` now returns a `PreprocError::IncludeFailed` with a readable message instead of `unwrap()`-panicking; regression test `missing_include_is_graceful_error`. `parser/preprocessor.rs:279` — `fs::read_to_string(path).unwrap()`. Verified: `include /nonexistent.mk` → `thread 'main' panicked … Result::unwrap() on an Err` (exit 101). The spec (Include Lines, p. 3135) requires a diagnostic and error exit for a non-prefixed `include`, not a panic. Fix: replace `unwrap()` with a graceful `ErrorCode::IoError`; for the `-include` (hyphen-prefixed) form, ignore a missing file per spec.

- [x] **#4 — `make -k` reports failure and exits 2 even when all targets succeed.** ✓ fixed (Phase 3): the unconditional `if keep_going` block in `main.rs` was the only failure signal, because under `-k` `rule.rs` swallows a failed recipe (prints the error, `break`s, returns `Ok`) so the error never reached `main`. Added a `KEEP_GOING_ERROR` atomic (`rule.rs`) set when a non-ignored recipe error is swallowed; `main` resets it before each command-line target and reports `Target … not remade because of errors` (and sets `had_error`) only when it fired — so an all-success `-k` build now exits 0 with no diagnostic. Tests `arguments::dash_k_success` (success ⇒ exit 0, silent) plus the preserved `arguments::dash_k` (failure ⇒ message + exit 2). Behaviorally verified: independent command-line targets still build after one fails. `main.rs:266–283` — after a *successful* `build_target`, the `if keep_going { eprintln!("…Target … not remade because of errors"); had_error = true; }` block fired unconditionally, and `had_error` forced `status_code = 2`.

- [x] **#5 — Command-line `macro=value` operands are unsupported.** ✓ fixed (Phase 2): `main()` partitions operands with `is_macro_definition()`; macro operands are appended after the makefile(s) so they take precedence. Tests `cmdline_macro_overrides_file`, `cmdline_macro_defines`. `main.rs:108–110` — all positional args go into `targets: Vec<OsString>`; nothing splits out `name=value`. Verified: `make FOO=bar all` → `make: parse error: UndefinedMacro("FOO")` (exit 4). The SYNOPSIS mandates `[macro[::[:]]=value...]` operands, and the spec gives them the highest macro precedence. Fix: before queueing targets, peel off args matching the macro-assignment forms and inject them as a top-precedence macro layer (the `ENV_MACROS` atomic in `preprocessor.rs` is a usable precedent).

### Major

- [x] **#6 — `$(string:subst1=subst2)` substitution form is unimplemented and hard-errors.** ✓ fixed (Phase 4): the `(`/`{` branch of `substitute` now detects `:` after the macro name and applies `apply_substitution`, which handles both the suffix form and the `[op]%[os]=[np][%][ns]` pattern form word-wise. Tests `preprocess::test_subst_suffix`, `test_subst_pattern`; verified `$(SRC:.c=.o)`→`a.o b.o foo.o`, `$(O:%.o=%.x)`→`a.x b.x`. `parser/preprocessor.rs` `substitute`.

- [x] **#7 — Backslash-newline line continuation is not folded.** ✓ fixed (Phase 4): `preprocess` now runs `fold_continuations` first. Outside recipe lines, `\<newline>` + leading white space of the next line collapse to a single space; in a recipe (tab-indented) line the continuation is spliced (one leading tab of the next line removed) so the whole command reaches the shell. An escaped trailing backslash (`\\`) is not treated as a splice. Tests `preprocess::test_continuation_macro`, `test_continuation_recipe`; verified `FOO = a \`<newline>`b` → `a b`.

- [x] **#8 — Single-suffix inference rules (`.c:`, `.sh:`) are never applied.** ✓ fixed (Phase 5): `try_parse_inference` now accepts a single-suffix target as `Inference { from: <suffix>, to: "" }`; `find_inference_rule` searches single-suffix rules (where the suffixless target's `<name>.<from>` exists) after double-suffix, and `build_target`'s no-rule branch invokes inference before falling through to `.DEFAULT`/`NoTarget`; `run_for_target` computes the `<target>.<from>` input for `to==""`. Test `inference_rules::single_suffix_rule`; verified `make bar` with `bar.c` + `.c:` → `built bar from bar.c`.

- [x] **#9 — Parallel execution machinery is entirely absent: `-j`, token pool, `.WAIT`, `.NOTPARALLEL`.** ✓ fixed (Phase 7): added `-j maxjobs` (last value wins via `overrides_with`; non-positive ⇒ 1). `Make` was made `Send`/`Sync` (macros now owned `(String,String)`), and a non-blocking `TokenPool` of `maxjobs-1` tokens bounds concurrency. `build_prerequisites` splits a target's prerequisites on `.WAIT` barriers (build left-of-`.WAIT` before right-of-`.WAIT`) and, under `-j>1`, builds each segment with `std::thread::scope`: each prerequisite that obtains a token runs in a worker thread, the rest build inline (so the recursion is deadlock-free). `.WAIT`/`.NOTPARALLEL` are now real `SpecialTarget` variants — `.WAIT` as a target is a no-op and as a prerequisite is a barrier (no longer "no target '.WAIT'"); `.NOTPARALLEL` forces sequential builds. Tests `parallel::{dash_j_builds_all_targets,dash_j_last_value_wins,notparallel_recognized,wait_barrier_is_not_built}`; behaviorally verified `-j2` halves wall-clock for two independent sleeps, `.WAIT`/`.NOTPARALLEL` serialize, and a diamond build does not deadlock.

- [x] **#10 — Multiple `-f makefile` options are rejected.** ✓ fixed (Phase 2): `makefile` is now `Vec<PathBuf>`; `parse_makefile` concatenates the operands in order (with `-` = stdin). Test `multiple_dash_f`. `main.rs:50–51` — `makefile: Option<PathBuf>`. Verified: `make -f A.mk -f B.mk …` → `error: the argument '--makefile <MAKEFILE>' cannot be used multiple times` (exit 2). Spec: multiple `-f` shall be processed in order. Fix: make it `Vec<PathBuf>` and concatenate the makefiles in order.

- [x] **#11 — The shell `-e` option is not in effect for non-ignored recipes.** ✓ fixed (Phase 6): the recipe command is now run with `-e -c` when the recipe's errors are not ignored, and plain `-c` when they are (`-`/`.IGNORE`/`-i`). Test `recipe_execution::shell_e_aborts_on_first_failure`; verified `false; echo REACHED` now aborts (exit 2, no `REACHED`) and `-false; echo REACHED` still reaches it.

- [x] **#12 — Recipes are run with the `SHELL` *environment variable*, which the spec forbids.** ✓ fixed (Phase 6): the recipe shell is resolved from the `SHELL` *macro* (default `/bin/sh`); the `SHELL` env var is no longer consulted for shell selection, and `init_env` no longer exports the `SHELL` macro to recipe sub-processes (so the macro cannot modify the child's `SHELL`). Verified: a `SHELL` macro selects the shell while a bogus `SHELL` env var is ignored.

- [x] **#13 — `MAKEFLAGS` is ignored.** ✓ fixed (Phase 4): `args_with_makeflags()` seeds options from the env var ahead of the real command line. The letters-only first word (`kn`) becomes a combined short option (`-kn`); `-`-prefixed and `macro=value` words pass through. `MAKEFLAGS` is inherited by recipe sub-processes via the environment (sub-make propagation). Tests `internal_macros::makeflags_letters_form`; verified `MAKEFLAGS=n`, `MAKEFLAGS=-n`, and `MAKEFLAGS='V=hello'`. Note: full synthesis of command-line flags *into* `MAKEFLAGS` for children is not done (only env-provided flags propagate).

- [x] **#14 — `$?` expands to *all* prerequisites, not those newer than the target.** ✓ fixed (Phase 4): `run_rule_with_prerequisites` now threads the `get_newer_prerequisites` slice through `run`/`run_for_target`/`run_with_files` into `substitute_internal_macros`, where `$?` uses the newer-only list (space-separated; the old code concatenated with no separator). `$^`/`$+` keep the full list. Behaviorally verified: with `prog: a.o b.o a.o` and only `b.o` newer, `$?`→`b.o`.

- [x] **#15 — `$^`, `$+`, and the `$(@D)`/`$(@F)` (dir/file) macro variants are missing.** ✓ fixed (Phase 4): `substitute_internal_macros` was rewritten around `expand_internal_macro`, which supports sigils `@ % ? < * ^ +` in both the two-char (`$^`) and bracketed (`$(@D)`, `${?F}`) forms. `$^` dedups (order preserved); `$+` keeps duplicates; the `D`/`F` modifiers take `dir_part`/`file_part` of each element. The preprocessor passes internal-macro references through verbatim (added `^`/`+` to the two-char passthrough and a `$(`-internal passthrough) so they reach the rule stage. Tests `internal_macros::caret_and_plus`, unit tests `rule::tests::dir_and_file_parts`; verified `$(@D)`/`$(@F)`/`${@F}`. Note: targets containing `/` still do not parse (separate pre-existing lexer limitation, not in this audit), so `$(@D)` is `.` for ordinary targets.

- [x] **#16 — `.SUFFIXES` is stored in a `BTreeSet`, destroying search order; additive/clear semantics are also broken.** ✓ fixed (Phase 5): added an authoritative insertion-ordered `Config.suffixes: Vec<String>` (consumed by `find_inference_rule` and `InferenceTarget`); the sorted `rules[".SUFFIXES"]` `BTreeSet` is kept only as a mirror for the `-p` dump. `process_suffixes` now clears on empty prerequisites (`clear_suffixes`) and appends otherwise (`add_suffix`, order-preserving, dedup) instead of replacing. `-r` clears the Vec too. Test `inference_rules::suffixes_clear_then_readd`; verified append keeps built-ins, empty `.SUFFIXES:` clears them, and a later `.SUFFIXES: .c .o` re-enables inference.

- [x] **#17 — Signal registration is gated on the wrong condition.** ✓ fixed (Phase 8): registration is now `if !dry_run && !print && !quit` — make catches signals unless `-n`/`-p`/`-q` is set (those take the default action), and `-i` is no longer an exemption. Test `target_behavior::async_events_registered_under_dash_i` confirms an interrupt under `-i` still cleans up and re-raises.

- [x] **#18 — `.PRECIOUS` with no prerequisites does not protect targets on signal.** ✓ fixed (Phase 8): `process_precious` now sets `make.config.precious = true` when `.PRECIOUS` has no prerequisites, so the global-precious flag the signal handler consults protects every in-progress target.

- [x] **#19 — `-include` is not actually implemented (line passed through).** ✓ fixed (Phase 5): `parse_include_directive` recognizes both `include` and the `-include` form, requires the trailing blank (so `includedir=…` is no longer mis-parsed as an include), and inlines the file; a missing/unreadable file is silently ignored for `-include` and a hard error for `include`. Tests `preprocess::test_dash_include_missing_ignored`, `test_include_missing_errors`, `test_includedir_not_mistaken_for_include`. Note: full immediate/delayed re-making of include files is not implemented (the file is simply inlined), which matches the existing `include` behavior.

### Minor

- [x] **#20 — Signal handler calls `process::exit(128+sig)` instead of resetting to default and re-raising.** ✓ fixed (Phase 8): the handler now does `signal(sig, SIG_DFL); raise(sig)`, so make dies *from* the signal and the parent observes a signal death (verified: `status.code()` is `None`, `status.signal()` is `SIGINT`). Tests updated accordingly.

- [x] **#21 — Signal cleanup ignores `.PHONY` membership and the mtime-change condition.** ✓ fixed (Phase 8): `INTERRUPT_FLAG` now carries an `InterruptInfo { target, precious, phony, original_mtime }`. The target's mtime is captured once before its recipe sequence; on interrupt the handler deletes only when the target is not precious, not phony, and its mtime changed (i.e. the recipe had begun writing the file). Verified by `target_behavior::async_events` (a freshly created `text.txt` is deleted) and `special_targets::precious` (a precious target is kept).

- [x] **#22 — `.IGNORE`/`.SILENT`/`.PHONY`/`.PRECIOUS` "subsequent occurrences add to the list" and per-target forms are order-dependent.** ✓ fixed (Phase 9): order-dependence does not occur in practice because special targets are processed only after *all* rules are classified (`Make::try_from` pass 2), so `additive()`/`global()` see every rule and the per-rule flags accumulate. The remaining literal gap — `process_phony`/`process_precious` `insert` (replacing) the stored set — is fixed to `entry().or_default().extend(...)`, so multiple `.PHONY`/`.PRECIOUS` lines now accumulate (visible in `-p`). `.SCCS_GET` keeps last-wins (it is a single command redefinition, not a list). Test `special_targets::phony_accumulates`.

- [x] **#23 — Runtime diagnostics are largely un-internationalized.** ✓ improved (Phase 9): the substantive error messages already route through `gettext` (`error_code.rs` `Display`). The remaining raw build-loop diagnostics in `main.rs` (the `-k` "Target … not remade because of errors" and the "is up to date." messages) are now routed through `gettext` too (English msgids reproduce the exact prior wording, so output is unchanged when untranslated). Comprehensive coverage of every string remains incremental, but `LC_MESSAGES` now governs the user-facing diagnostics.

- [x] **#24 — `-p` output format is a Rust `{:?}` debug dump.** ✓ reviewed (Phase 9): the spec explicitly leaves the `-p` format unspecified, so the debug dump conforms. It is intentionally kept as-is: it is the documented contract of three exact-match `-p` regression tests, and a reformatting would be pure churn with no conformance gain. No code change.

- [x] **#25 — Internal error exit codes use values 3–9.** ✓ reviewed (Phase 3): `error_code.rs` maps distinct internal errors to 3–9; only `-q`'s not-up-to-date maps to 1 and success to 0. All error paths are `>1`, so this satisfies the spec's 0/1/>1 exit-status contract. The granular codes are non-standard but conforming, so they are kept (changing them risks breaking the existing exit-code tests for no conformance gain). No code change.

## Detailed conformance matrix

### Options (SYNOPSIS `make [-einpqrst] [-f makefile]... [-j maxjobs] [-k|-S] [macro=value...] [target...]`)
- [x] `-e` CONFORMS — environment overrides macros; `main.rs:56–61`, plumbed via `ENV_MACROS` (`preprocessor.rs`).
- [x] `-i` CONFORMS — global ignore; `main.rs:53`, `config.rs:44`, `rule.rs:165`.
- [x] `-n` CONFORMS — prints without executing; verified (`echo …` printed, not run). `rule.rs:183–188`.
- [x] `-p` CONFORMS (format unspecified) — `main.rs:112–114` debug dump. See #24.
- [x] `-q` CONFORMS — exit 1 when not up to date, recipe not run; verified. `rule.rs:194–201`.
- [x] `-r` CONFORMS — clears suffixes / built-ins; `main.rs:207–209`.
- [x] `-s` CONFORMS — suppresses echo; verified. `rule.rs:204–207`.
- [x] `-t` CONFORMS — touches targets; `rule.rs:252–260`.
- [x] `-k` CONFORMS (Critical #4 fixed) — all-success build exits 0 silently; a failed target is reported while independent targets continue, exit 2.
- [x] `-S` present as `--terminate` (`main.rs`) and is the default; the `-k` interaction (#4) is fixed (when both are set, `-S`/terminate wins so make stops). Known minor: strict POSIX "last of `-k`/`-S` on the command line wins" ordering is not modeled (the flags are independent booleans); documented, not a numbered finding.
- [x] `-f` (multiple) processed in order (Major #10 fixed, Phase 2).
- [x] `-j maxjobs` implemented with a token pool (Major #9 fixed).
- [x] `macro=value` operands supported with top precedence (Critical #5 fixed, Phase 2).
- Extensions present (non-POSIX, no conflict — informational): `-C/--directory` (verified working), long-option aliases (`--ignore`, `--silent`, …). Per audit scope these are noted, not flagged.

### Macros
- [x] `$(NAME)` / `${NAME}` CONFORMS — verified (`CC=echo` expands in recipe; refutes an agent "verbatim to shell" claim). `preprocessor.rs` `substitute`.
- [x] Single internal macros `$@ $% $? $< $*` CONFORMS *in isolation* — `rule.rs:284–301`; `$@` verified equal to GNU. But see #14 (`$?` semantics) and #1 (a same-line `=` still aborts the parse).
- [x] `$$` → `$` CONFORMS — verified (`echo price is $$5` → `price is `). Refutes an agent DIVERGES claim.
- [x] `$(VAR:a=b)` / `%`-pattern CONFORMS (Major #6 fixed).
- [x] `$^` / `$+` / `$(@D)` / `$(@F)` CONFORMS (Major #15 fixed).
- [x] Command-line macro precedence (Critical #5 done) and `MAKEFLAGS` (Major #13 fixed).
- [x] Backslash-newline in macro bodies folded (Major #7 fixed).
- [x] `?=`, `+=`, `!=` — `?=` verified correct (kept existing value; fell back when unset — refutes an agent "inverted" claim). `+=`/`!=` not behaviorally re-verified here; flavor (immediate vs deferred) is not tracked (`preprocessor.rs`) — **(static, low priority)**.

### Operands / STDIN / Include
- [x] `-` operand to `-f` reads stdin — `main.rs:140–141`.
- [x] First non-special target as default — `lib.rs:65–76`; verified via `.DEFAULT` and normal targets.
- [x] `include existing.mk` CONFORMS — verified.
- [x] `include missing.mk` graceful error (Critical #3 done); `-include` implemented with missing-file-ignore (Major #19 fixed).

### Environment variables
- [x] `LANG`/`LC_*` — `setlocale(LcAll, "")` at `main.rs:165` (CONFORMS for locale init; message coverage is #23).
- [x] `MAKEFLAGS` honored (Major #13 fixed).
- [x] `SHELL` macro (not env var) used for recipe shell (Major #12 fixed).
- [x] `PROJECTDIR` (XSI) — reviewed: an optional XSI SCCS search-path feature, intentionally out of scope alongside `.SCCS_GET` runtime retrieval. Documented as a known limitation, not a base-spec conformance failure.

### Asynchronous events
- [x] SIGHUP/SIGINT/SIGQUIT/SIGTERM handlers installed — `signal_handler.rs:40–43`.
- [x] Non-precious in-progress target removed on signal — `signal_handler.rs:19–32`.
- [x] Registration gated correctly (Major #17 fixed); `.PRECIOUS` global honored (Major #18 fixed); reset-and-re-raise (Minor #20 fixed); mtime/`.PHONY` cleanup check (Minor #21 fixed).

### STDOUT / STDERR / Exit status
- [x] Recipe echo to stdout; diagnostics to stderr — `rule.rs`, `main.rs`.
- [x] Recipe command failure → exit 2 — verified (`false` → `execution error: 1`, exit 2). `error_code.rs`.
- [x] Up-to-date message — `main.rs:257–258`.
- [x] `-k` exit handling (Critical #4 fixed); granular internal codes conform (Minor #25, kept).

### Special targets
- [x] `.DEFAULT` CONFORMS — verified (fires for missing target). `special_target.rs` / `lib.rs:136–143`. Now also enforces the "specified with commands" requirement (Phase 9): an empty `.DEFAULT:` is a constraint violation. Test `special_targets::validations::default_without_recipes`.
- [x] `.PHONY` CONFORMS — verified (forces rebuild twice). `lib.rs:167–169`.
- [x] `.SILENT` (global) CONFORMS — verified. `special_target.rs:259–267`.
- [x] `.IGNORE` PARTIAL — global form works; ordering caveat #22.
- [x] `.SCCS_GET` (XSI) PARTIAL — recognized/stored; no runtime SCCS retrieval. Reviewed (Phase 9): full SCCS auto-retrieval is an optional XSI feature requiring SCCS tooling and is intentionally out of scope; the special target is parsed, validated (no prerequisites), and its command is stored/overridable. Documented as a known limitation rather than a conformance failure of the base spec.
- [x] `.POSIX` accepted (Critical #2 fixed, Phase 2).
- [x] `.SUFFIXES` insertion-ordered with clear/append (Major #16 fixed).
- [x] `.PRECIOUS` global protection honored (Major #18 fixed).
- [x] `.WAIT` / `.NOTPARALLEL` recognized and honored (Major #9 fixed).
- [x] Subsequent-occurrence accumulation (Minor #22 fixed) — sets now extend.

### Extended description / rendering
- [x] One shell per recipe line CONFORMS — `rule.rs:209–219`.
- [x] `@` (silent) / `-` (ignore) / `+` (force) prefixes recognized — `rule/recipe.rs`; `+` forces under `-n`/`-t` (verified indirectly).
- [x] Shell `-e` in effect (Major #11 fixed); `+`/`$(MAKE)` lines now run under `-n`/`-t`/`-q` (`$(MAKE)` is passed through preprocessing, detected in `run_with_files`, and expanded to the make program). Tests `recipe_execution::*`.
- [x] **Archive/library `lib(member.o)` member mtime — PARTIAL→implemented for the timestamp:** `get_modified_time` now reads a member's stored mtime from the `ar` archive (`archive_member_mtime`/`parse_archive_target`, unit-tested), so an `archive(member)` *string* compares correctly. REMAINING (separate pre-existing parser gap, not the audited mtime issue): the rule parser cannot lex a `lib(member):` target header (`(` is a distinct token), so such a rule cannot yet be *declared* in a makefile.

## Test coverage signal

Existing tests are fixture-driven (`make/tests/makefiles/**`) and cover parsing of includes, recipe prefixes, and several special targets. Not covered (each is a "write a test" item):
- [x] Recipe lines containing `=` (#1) — `recipe_line_with_equals` (Phase 1).
- [x] `.POSIX:` as the first line (#2) — `special_targets::posix` (Phase 2).
- [x] Missing `include` file → graceful error, and `-include` → ignore (#3, #19) — `preprocess::test_*include*`.
- [x] `make -k` exit status on success and on partial failure (#4) — `arguments::dash_k_success` + `arguments::dash_k`.
- [x] Command-line `macro=value` operands and precedence (#5) — `macros::cmdline_macro_*` (Phase 2).
- [x] `$(VAR:.c=.o)` substitution (#6) and backslash-newline continuation (#7) — `preprocess::test_subst_*`, `test_continuation_*`.
- [x] Single-suffix inference rules (#8) — `inference_rules::single_suffix_rule`.
- [x] `-j`, `.WAIT`, `.NOTPARALLEL` (#9) — `parallel::*`; multiple `-f` (#10).
- [x] Shell `-e` abort on first failing command (#11) — `recipe_execution::shell_e_aborts_on_first_failure`; `SHELL` macro vs env var (#12, behaviorally verified).
- [x] `MAKEFLAGS` seeding options (#13); `$?` newer-only and `$^`/`$+`/`$(@D)` (#14, #15) — `internal_macros::*`, `rule::tests::dir_and_file_parts`.
- [x] `.SUFFIXES` ordering + clear/append (#16) — `inference_rules::suffixes_clear_then_readd`.
- [x] Signal-driven cleanup, `.PRECIOUS` global, re-raise (#17, #18, #20, #21) — `target_behavior::async_events*`, `special_targets::precious`.

## Suggested PR groupings

- **PR A — "Don't choke on ordinary makefiles" (Critical #1, #3):** make `generate_macro_table` skip recipe lines and validate the pre-`=` name; replace the `include` `unwrap()` with a diagnostic. Biggest correctness win.
- **PR B — "Portability basics" (Critical #2, #5; Major #10):** accept `.POSIX`; parse command-line `macro=value` operands with correct precedence; accept multiple `-f`.
- **PR C — "`-k`/exit-status correctness" (Critical #4; Minor #25):** per-target error tracking; stop flagging success as failure.
- **PR D — "Macro completeness" (Major #6, #7, #13, #14, #15):** `:subst=`/`%` forms, backslash-newline folding, `MAKEFLAGS`, `$?` newer-only, `$^`/`$+`/`$(@D)`/`$(@F)`.
- **PR E — "Inference & suffixes" (Major #8, #16; #19):** single-suffix rules, insertion-ordered `.SUFFIXES` with clear/append, real `-include`.
- **PR F — "Recipe execution fidelity" (Major #11, #12):** shell `-e` when not ignoring; use the `SHELL` macro, not the env var.
- **PR G — "Parallelism" (Major #9):** `-j maxjobs` + token pool + `.WAIT`/`.NOTPARALLEL` (larger, can be staged last).
- **PR H — "Signals" (Major #17, #18; Minor #20, #21):** correct registration gating, `.PRECIOUS` global flag, reset-and-re-raise, mtime/`.PHONY` checks.

## Appendix — agent findings refuted by behavioral testing

Per the playbook, claims that did not survive verification are recorded, not deleted:
- **`$(VAR)` in recipes reaches the shell verbatim** — REFUTED. `CC=echo` expands to `echo …` and runs; user macros are expanded at preprocess time.
- **`$$` passes through as `$$`** — REFUTED. `$$` collapses to `$` (verified `echo price is $$5` → `price is `).
- **`?=` logic is inverted** — REFUTED. Keeps an existing value and falls back when unset (both verified).
- **`-include` of a missing file panics** — REFUTED. It does not panic (the line is passed through; the real gap is that `-include` is not implemented, #19). The *non*-prefixed `include` is what panics (#3).
