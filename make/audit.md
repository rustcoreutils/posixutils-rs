# POSIX.1-2024 Conformance Audit — `make`

**Implementation:** `make/src/` (16 files, ~3,074 lines: `main.rs`, `lib.rs`, `config.rs`, `error_code.rs`, `special_target.rs`, `signal_handler.rs`, `parser/{mod,lex,parse,preprocessor}.rs`, `rule.rs` + `rule/{target,prerequisite,recipe,config}.rs`)
**Tests:** `make/tests/` (fixture makefiles + `mod.rs` harness, ~1,511 lines)
**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 §3 `make`, pp. 3130–3146.
**Reference:** No sliced spec tree was available; the spec was extracted from the mega-PDF `~/tmp/POSIX.2024.pdf` (pp. 3130–3146) to `~/tmp/make-spec.txt`. Mirrors the `m4` audit's PDF-based method.
**Date:** 2026-06-12
**Verification:** Critical and most Major findings were **behaviorally verified** against the built `target/release/make` binary (no source changes), cross-checked with GNU `make` where a control was useful. Items not behaviorally verified are tagged **(static)**. Several agent-proposed findings were **refuted** by behavioral testing and are recorded at the bottom rather than silently dropped.

## TL;DR

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

- [ ] **#9 — Parallel execution machinery is entirely absent: `-j`, token pool, `.WAIT`, `.NOTPARALLEL`.** `main.rs` `Args` has no `-j`; `special_target.rs:20–39` enum omits `.WAIT`/`.NOTPARALLEL` (silently ignored at `:173–176`). Verified: `make -j 2 …` → `error: unexpected argument '-j'` (exit 2). These are POSIX.1-2024 requirements, not extensions. Fix: implement `-j maxjobs` with the token-pool model, and validate/honor `.WAIT`/`.NOTPARALLEL`.

- [x] **#10 — Multiple `-f makefile` options are rejected.** ✓ fixed (Phase 2): `makefile` is now `Vec<PathBuf>`; `parse_makefile` concatenates the operands in order (with `-` = stdin). Test `multiple_dash_f`. `main.rs:50–51` — `makefile: Option<PathBuf>`. Verified: `make -f A.mk -f B.mk …` → `error: the argument '--makefile <MAKEFILE>' cannot be used multiple times` (exit 2). Spec: multiple `-f` shall be processed in order. Fix: make it `Vec<PathBuf>` and concatenate the makefiles in order.

- [ ] **#11 — The shell `-e` option is not in effect for non-ignored recipes.** `rule.rs:209–219` runs each line as `sh -c <line>` with no `-e`. Verified: a recipe `false; echo REACHED` prints `REACHED` and exits 0 (should abort). Spec Makefile Execution: "if errors are not being ignored then the shell −e option shall also be in effect." Fix: invoke `sh -ce <line>` when `!ignore`; plain `sh -c` when ignoring.

- [ ] **#12 — Recipes are run with the `SHELL` *environment variable*, which the spec forbids.** `rule.rs:209–213` uses `env::var("SHELL")`. Spec ENVIRONMENT VARIABLES (p. 3132): "The value of the SHELL environment variable shall not be used as a macro and shall not be modified by defining the SHELL macro …". The recipe shell must come from the `SHELL` macro (default `/bin/sh`), never the env var. Fix: resolve the `SHELL` macro from the parsed macro set; ignore the `SHELL` env var for shell selection.

- [x] **#13 — `MAKEFLAGS` is ignored.** ✓ fixed (Phase 4): `args_with_makeflags()` seeds options from the env var ahead of the real command line. The letters-only first word (`kn`) becomes a combined short option (`-kn`); `-`-prefixed and `macro=value` words pass through. `MAKEFLAGS` is inherited by recipe sub-processes via the environment (sub-make propagation). Tests `internal_macros::makeflags_letters_form`; verified `MAKEFLAGS=n`, `MAKEFLAGS=-n`, and `MAKEFLAGS='V=hello'`. Note: full synthesis of command-line flags *into* `MAKEFLAGS` for children is not done (only env-provided flags propagate).

- [x] **#14 — `$?` expands to *all* prerequisites, not those newer than the target.** ✓ fixed (Phase 4): `run_rule_with_prerequisites` now threads the `get_newer_prerequisites` slice through `run`/`run_for_target`/`run_with_files` into `substitute_internal_macros`, where `$?` uses the newer-only list (space-separated; the old code concatenated with no separator). `$^`/`$+` keep the full list. Behaviorally verified: with `prog: a.o b.o a.o` and only `b.o` newer, `$?`→`b.o`.

- [x] **#15 — `$^`, `$+`, and the `$(@D)`/`$(@F)` (dir/file) macro variants are missing.** ✓ fixed (Phase 4): `substitute_internal_macros` was rewritten around `expand_internal_macro`, which supports sigils `@ % ? < * ^ +` in both the two-char (`$^`) and bracketed (`$(@D)`, `${?F}`) forms. `$^` dedups (order preserved); `$+` keeps duplicates; the `D`/`F` modifiers take `dir_part`/`file_part` of each element. The preprocessor passes internal-macro references through verbatim (added `^`/`+` to the two-char passthrough and a `$(`-internal passthrough) so they reach the rule stage. Tests `internal_macros::caret_and_plus`, unit tests `rule::tests::dir_and_file_parts`; verified `$(@D)`/`$(@F)`/`${@F}`. Note: targets containing `/` still do not parse (separate pre-existing lexer limitation, not in this audit), so `$(@D)` is `.` for ordinary targets.

- [x] **#16 — `.SUFFIXES` is stored in a `BTreeSet`, destroying search order; additive/clear semantics are also broken.** ✓ fixed (Phase 5): added an authoritative insertion-ordered `Config.suffixes: Vec<String>` (consumed by `find_inference_rule` and `InferenceTarget`); the sorted `rules[".SUFFIXES"]` `BTreeSet` is kept only as a mirror for the `-p` dump. `process_suffixes` now clears on empty prerequisites (`clear_suffixes`) and appends otherwise (`add_suffix`, order-preserving, dedup) instead of replacing. `-r` clears the Vec too. Test `inference_rules::suffixes_clear_then_readd`; verified append keeps built-ins, empty `.SUFFIXES:` clears them, and a later `.SUFFIXES: .c .o` re-enables inference.

- [ ] **#17 — Signal registration is gated on the wrong condition.** `rule.rs:179–181` registers signals only when `!ignore || print || quit || dry_run`. Under `-i` alone, signals are *not* registered; under `-n`/`-p`/`-q`, the spec says make shall take the *default* action (i.e. not catch), yet the code registers when those are set. **(static — signal timing not behaviorally tested.)** Fix: register unless `-n`/`-p`/`-q` is set; `-i` is not an exemption.

- [ ] **#18 — `.PRECIOUS` with no prerequisites does not protect targets on signal.** `special_target.rs` `process_precious` never sets the global `config.precious`; the signal cleanup reads `global_precious || rule_precious` (`rule.rs:173`). So the "no prerequisites ⇒ all targets precious" case fails and a partially built target may be deleted on SIGINT/SIGTERM. **(static.)** Fix: set `make.config.precious = true` when `.PRECIOUS` has no prerequisites.

- [x] **#19 — `-include` is not actually implemented (line passed through).** ✓ fixed (Phase 5): `parse_include_directive` recognizes both `include` and the `-include` form, requires the trailing blank (so `includedir=…` is no longer mis-parsed as an include), and inlines the file; a missing/unreadable file is silently ignored for `-include` and a hard error for `include`. Tests `preprocess::test_dash_include_missing_ignored`, `test_include_missing_errors`, `test_includedir_not_mistaken_for_include`. Note: full immediate/delayed re-making of include files is not implemented (the file is simply inlined), which matches the existing `include` behavior.

### Minor

- [ ] **#20 — Signal handler calls `process::exit(128+sig)` instead of resetting to default and re-raising.** `signal_handler.rs:35`. Spec: "set the signal to default and re-signal itself" so the parent sees a signal death. Fix: `signal(sig, SIG_DFL); raise(sig)`.

- [ ] **#21 — Signal cleanup ignores `.PHONY` membership and the mtime-change condition.** `signal_handler.rs:19–32` deletes unconditionally (no record of the target's pre-recipe mtime) and has no awareness of the phony set. Spec deletes only if the target's mtime changed and it is not a `.PHONY`/`.PRECIOUS` prerequisite. **(static.)**

- [ ] **#22 — `.IGNORE`/`.SILENT`/`.PHONY`/`.PRECIOUS` "subsequent occurrences add to the list" and per-target forms are order-dependent.** `special_target.rs` `additive()`/`global()` only mutate rules already parsed at process time, and several handlers `insert` (replace) rather than accumulate. A target defined *after* the special target may not be affected. **(static; `.SILENT:` global form was behaviorally confirmed working.)**

- [ ] **#23 — Runtime diagnostics are largely un-internationalized.** `setlocale` is called (`main.rs:165`) and some strings use `gettext`, but most `eprintln!`/error text is raw English. Spec: `LC_MESSAGES` shall affect diagnostics. **(static.)**

- [ ] **#24 — `-p` output format is a Rust `{:?}` debug dump.** `main.rs:112–114` (`print!("{:?}", rules)`). The spec leaves the format unspecified, so this conforms, but it is not a usable macro/target description. Low priority.

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
- [ ] **`-S` PARTIAL** — present as `--terminate` (`main.rs:43–48`) and is the default; the `-k` interaction (#4) is fixed, but `-S` overriding a prior `-k` on the command line is not separately verified.
- [ ] **`-f` (multiple) MISSING (Major #10)** — only one accepted.
- [ ] **`-j` MISSING (Major #9)** — not a recognized option.
- [ ] **`macro=value` operands MISSING (Critical #5)**.
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
- [ ] **`SHELL` env var misused for recipe shell (Major #12)** — spec forbids.
- [ ] **`PROJECTDIR` (XSI) MISSING** — no SCCS search-path support **(static, N/A-ish)**.

### Asynchronous events
- [x] SIGHUP/SIGINT/SIGQUIT/SIGTERM handlers installed — `signal_handler.rs:40–43`.
- [x] Non-precious in-progress target removed on signal — `signal_handler.rs:19–32`.
- [ ] **Registration gated on wrong condition (Major #17)**; **`.PRECIOUS` global not honored (Major #18)**; **exit instead of re-raise (Minor #20)**; **no mtime/`.PHONY` check (Minor #21)**.

### STDOUT / STDERR / Exit status
- [x] Recipe echo to stdout; diagnostics to stderr — `rule.rs`, `main.rs`.
- [x] Recipe command failure → exit 2 — verified (`false` → `execution error: 1`, exit 2). `error_code.rs`.
- [x] Up-to-date message — `main.rs:257–258`.
- [x] `-k` exit handling (Critical #4 fixed); granular internal codes conform (Minor #25, kept).

### Special targets
- [x] `.DEFAULT` CONFORMS — verified (fires for missing target). `special_target.rs` / `lib.rs:136–143`. (Does not enforce "must have commands" — minor.)
- [x] `.PHONY` CONFORMS — verified (forces rebuild twice). `lib.rs:167–169`.
- [x] `.SILENT` (global) CONFORMS — verified. `special_target.rs:259–267`.
- [x] `.IGNORE` PARTIAL — global form works; ordering caveat #22.
- [x] `.SCCS_GET` (XSI) PARTIAL — recognized/stored; no runtime SCCS retrieval traced. **(static.)**
- [ ] **`.POSIX` rejected (Critical #2)**.
- [x] `.SUFFIXES` insertion-ordered with clear/append (Major #16 fixed).
- [ ] **`.PRECIOUS` global protection broken (Major #18)**.
- [ ] **`.WAIT` / `.NOTPARALLEL` unrecognized (Major #9)** — silently ignored.
- [ ] **Subsequent-occurrence accumulation order-dependent (Minor #22)**.

### Extended description / rendering
- [x] One shell per recipe line CONFORMS — `rule.rs:209–219`.
- [x] `@` (silent) / `-` (ignore) / `+` (force) prefixes recognized — `rule/recipe.rs`; `+` forces under `-n`/`-t` (verified indirectly).
- [ ] **Shell `-e` missing (Major #11)**; **`+`/MAKE-macro lines under `-n`/`-q` not special-cased** — `rule.rs:183–202` has no `$(MAKE)` detection **(static, Major)**.
- [ ] **Archive/library `lib(member.o)` targets PARTIAL** — `$@`/`$%` parse the form (`rule.rs:284–292`) but no `ar`-based member mtime lookup exists, so such targets cannot be brought up to date **(static, Major)**.

## Test coverage signal

Existing tests are fixture-driven (`make/tests/makefiles/**`) and cover parsing of includes, recipe prefixes, and several special targets. Not covered (each is a "write a test" item):
- [ ] Recipe lines containing `=` (the #1 regression — no fixture exercises this).
- [ ] `.POSIX:` as the first line (#2).
- [x] Missing `include` file → graceful error, and `-include` → ignore (#3, #19) — `preprocess::test_*include*`.
- [x] `make -k` exit status on success and on partial failure (#4) — `arguments::dash_k_success` + `arguments::dash_k`.
- [ ] Command-line `macro=value` operands and precedence (#5).
- [x] `$(VAR:.c=.o)` substitution (#6) and backslash-newline continuation (#7) — `preprocess::test_subst_*`, `test_continuation_*`.
- [x] Single-suffix inference rules (#8) — `inference_rules::single_suffix_rule`.
- [ ] `-j`, `.WAIT`, `.NOTPARALLEL` (#9); multiple `-f` (#10).
- [ ] Shell `-e` abort on first failing command (#11); `SHELL` macro vs env var (#12).
- [x] `MAKEFLAGS` seeding options (#13); `$?` newer-only and `$^`/`$+`/`$(@D)` (#14, #15) — `internal_macros::*`, `rule::tests::dir_and_file_parts`.
- [x] `.SUFFIXES` ordering + clear/append (#16) — `inference_rules::suffixes_clear_then_readd`.
- [ ] Signal-driven cleanup, `.PRECIOUS` global, re-raise (#17, #18, #20, #21).

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
