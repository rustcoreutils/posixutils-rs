# POSIX.1-2024 Conformance Audit — `make`

Open items only. The 2026-06-12 audit and its 25 numbered findings are in git
history — `git show 49de59d3^:make/audit.md` recovers the file as it stood
before it was deleted on 2026-08-27, and `git log --follow -- make/audit.md`
walks its whole history.

**Implementation:** `make/src/` (~3,074 lines across 16 files)
**Tests:** `make/tests/` (fixture makefiles + `mod.rs` harness, ~1,511 lines)
**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 §3 `make`, pp. 3130–3146.
No sliced spec tree covers `make`; the section was extracted from the mega-PDF.

## Why this file came back

It was deleted on 2026-08-27 as a crate with nothing open. A crate-wide review
the same day found 21 defects, every one reproduced against a built
`target/release/make`. Two of them were already in the old file and ticked:

- **Slash in a target or prerequisite** (now #26) was dispositioned
  "genuinely out-of-scope — parser-level `lib(member):` / slash-in-target-name
  syntax." That framing reads as an XSI archive-member corner. It is not: it
  means no makefile with subdirectories can be built.
- **`MAKEFLAGS`** (old #13, now #40) was ticked ✓ fixed with a trailing note
  that "full synthesis of command-line flags *into* `MAKEFLAGS` for children is
  not done." That note *is* the finding.

New findings are numbered from #26 so the old numbers keep resolving in git.

**All 23 remaining old findings were then re-probed (2026-08-27)** against the
same binary, with GNU Make 4.3 as the reference wherever POSIX leaves room.
Eighteen hold, three were refuted, two are partial. The results are recorded at
the bottom of this file so the next reader does not have to redo them.

## Critical

Each of these blocks ordinary use, crashes, hangs, or silently produces the
wrong build.

- [x] **#26 — The lexer omits `/`, so no path works.** ✓ fixed 2026-08-27 (P1).
  The token lexer is gone; the scanner treats every character that is not a
  `<blank>`, `:`, `;` or `#` as an ordinary name character, so `/`, `(`, `)`,
  `~`, `%`, `"` and `=` all survive in both positions. Tests
  `test_slash_in_prerequisite`, `test_slash_in_target`,
  `test_archive_member_target`, `scan::accepts_slashes_in_names`.
- [x] **#27 — A rule may name only one target.** ✓ fixed 2026-08-27 (P1).
  `split_rule_line` splits every `<blank>`-separated word left of the first
  `:` into a target. Tests `test_multiple_targets_per_rule`,
  `scan::accepts_multiple_targets`.
- [x] **#28 — An indirect dependency cycle overflows the stack.** ✓ fixed
  2026-08-27 (P4). `graph::find_cycle` is an iterative DFS with an explicit
  stack that inserts into its on-stack and done sets as it walks, so a cycle
  anywhere in the graph is reported rather than only one passing back through
  the root. `a: b` / `b: c` / `c: b` now exits 8 with
  `recursive prerequisite found trying to build 'b'`. Tests
  `indirect_cycle_not_through_the_root_is_found`, `deep_chain_does_not_overflow`
  (10,000 deep), `indirect_cycle_is_diagnosed_not_a_stack_overflow`.
- [x] **#29 — A shared prerequisite is built repeatedly, and concurrently under
  `-j`.** ✓ fixed 2026-08-27 (P4). `graph::Ledger` moves a target through
  `Unvisited → Running → Finished` under one mutex, so only the thread that
  observes it unclaimed runs the recipe; a second arrival either replays the
  recorded outcome or waits on a condvar. The wait cannot deadlock because the
  cycle check runs before any worker is spawned. Tests
  `only_one_thread_builds_a_target`, `a_shared_prerequisite_builds_once`,
  `a_shared_prerequisite_builds_once_under_dash_j`.
- [x] **#30 — Only the first rule for a target is used; later ones are silently
  dropped.** ✓ fixed 2026-08-27 (P4). Rules naming the same target are merged at
  construction via `Rule::absorb`: prerequisites accumulate (not deduplicated —
  `$+` keeps duplicates), and a second rule carrying commands warns and wins, as
  GNU does. POSIX 105653 permits only one commanded rule but attaches no "shall
  be an error", so refusing the makefile would reject input other makes accept.
  Test `prerequisites_accumulate_across_rules`.
- [x] **#31 — `get_newer_prerequisites` has no memoization; DAG traversal is
  exponential.** ✓ fixed 2026-08-27 (P4). The ledger records each target's
  outcome, so a second visit replays it instead of re-walking the subtree. The
  23-node diamond chain that did not finish in 30 s now completes in 0.01 s,
  matching GNU Make 4.3.
- [x] **#32 — Both preprocessor fixpoint loops are unbounded and hang.** ✓ fixed
  2026-08-27 (P2). Include splicing is capped at 64 levels (POSIX 105611 requires
  at least 16), so a self-including file reports a cycle instead of looping;
  macro expansion is capped at 256 rounds, so `A = $(A)x` reports
  `macro expansion does not terminate` instead of growing the text forever.
  Tests `include_recursion_is_capped`, `recursive_macro_is_capped`.

- [x] **#33 — Under `-k`, a target whose prerequisite failed still runs its
  recipe.** ✓ fixed 2026-08-27 (P6). A failed recipe now fails its target
  whatever `-k` says; `-k` means keep building the *other* targets, which the
  traversal decides. `KEEP_GOING_ERROR`, the process-global flag that could not
  say which target failed under `-j`, is deleted. Test
  `keep_going_skips_a_target_whose_prerequisite_failed`, which also pins that an
  independent sibling still builds.
## Major

- [x] **#34 — An undefined macro is fatal, environment variables are not macros,
  and comments are expanded.** ✓ fixed 2026-08-27 (P2). `lookup_macro` returns
  the empty string for an unknown name (POSIX 105833) and consults the
  environment unconditionally as macro source 3 (105845) -- `-e` now only
  decides which source wins, rather than whether the environment is read at all.
  A bare `$` in a comment no longer aborts the parse, since the reference simply
  expands to nothing. Tests `undefined_macro_expands_to_empty`,
  `environment_is_a_macro_source_without_dash_e`,
  `a_dollar_in_a_comment_does_not_abort`.
  _Residual: comment text is still expanded rather than skipped. It is now
  harmless, but `$(shell ...)` in a comment would still run. Recorded as **#51**._
- [x] **#51 — Comment text is expanded rather than skipped.** ✓ fixed 2026-08-27
  (P7). The comment is stripped before expansion, so a reference inside one is
  never evaluated — a commented-out `$(shell rm -rf ...)` does not run. Command
  lines are exempt: they reach the shell verbatim, where `#` is meaningful.
- [x] **#35 — Include processing is handed an empty macro table.** ✓ fixed
  2026-08-27 (P1 step 1). `expand_includes` threads the real macro table through
  `process_include_lines`, which was being handed `&HashMap::new()`, so
  `include $(TOP)/inc.mk` now resolves. Test `test_include_path_may_use_a_macro`.
- [x] **#36 — No `VARIABLE` node is ever built, so `SHELL` and the recipe
  environment are dead.** ✓ fixed 2026-08-27 (P1). Macros never reached the
  tree: the preprocessor consumed them textually and blanked their lines before
  lexing, so `variable_definitions()` was always empty and `Make::macros` always
  `[]`. `preprocess` now returns the macro table and `Make` takes it directly —
  no node kind was needed. Probed with `SHELL = ./fakesh`: the recipe runs under
  `fakesh`, matching GNU. Test `shell_macro_selects_the_recipe_shell`.
  _Landing this exposed **#49**, which had to be fixed alongside it._

- [x] **#37 — `$*` expands to the whole target name, not the stem.** ✓ fixed
  2026-08-27 (P5). `$*` is now the target with its suffix removed, keeping any
  directory part. `.c.o` applied to `f.o` gives `f`. Test `star_is_the_stem`.
- [x] **#38 — The inference-rule classifier misfires on any dot-target beginning
  with a known suffix.** ✓ fixed 2026-08-27 (P5). Classification now requires the
  name to *parse* as `.s1` or `.s1.s2` **and** its suffixes to be in
  `.SUFFIXES`, instead of asking whether the name merely starts with one, so
  `.config:` is an ordinary rule. `InferenceTarget::from`/`to` no longer compute
  the same expression. The second half was in `Rule::run`, whose inference branch
  scanned the working directory: a dot-target that only looked like an inference
  rule found no files and silently ran no recipe. Tests
  `a_dot_target_is_not_an_inference_rule`, `default_target_skips_a_dot_target`.
- [x] **#39 — A non-UTF-8 target operand panics.** ✓ fixed 2026-08-27 (P6).
  Diagnosed and skipped rather than unwrapped, so `make $'\xff'` exits 2 instead
  of 101. The crate is `String`-based throughout, so such a target still cannot
  be *built*; it can now at least be reported. Test
  `a_non_utf8_target_is_diagnosed_not_a_panic`.
- [x] **#40 — `MAKEFLAGS` is never synthesized, so it does not reach
  sub-makes.** ✓ fixed 2026-08-27 (P6). `makeflags_for_children` builds the
  letters-only form POSIX 105866 describes — and that `args_with_makeflags`
  already parses back — from the inheritable options. `-f`, `-C` and the target
  operands are excluded as specific to the invocation. Verified end to end: a
  sub-make invoked through `$(MAKE)` sees `MAKEFLAGS=[k]`. Tests
  `makeflags_carries_options_to_children`,
  `makeflags_is_empty_when_no_options_are_given`.
- [x] **#41 — `register_signals()` overrides an inherited `SIG_IGN`, and runs
  once per recipe.** ✓ fixed 2026-08-27 (P6). `sigaction` replaces `signal`, the
  existing disposition is read first and left alone when it is `SIG_IGN`, and
  registration happens once via `Once` rather than per recipe line. Probed: make
  run under `trap '' INT` survives a SIGINT that used to kill it.
- [x] **#42 — `-t` touches `.PHONY` targets.** ✓ fixed 2026-08-27 (P6). A phony
  target names no file, so `-t` skips it instead of creating one that made every
  later `make <target>` report it up to date forever. Test
  `touch_does_not_materialize_a_phony_target`.
- [x] **#47 — `.SUFFIXES` order does not drive inference-rule selection.**
  ✓ fixed 2026-08-27 (P5). `find_inference_rule` iterates `Config::suffixes` and
  looks up the rule for each, rather than iterating the rules in file order.
  POSIX 105920: "The order in which the suffixes are specified defines the order
  in which the inference rules ... are used." With `.SUFFIXES: .sh .c` both rule
  orderings now pick `.sh`, matching GNU Make 4.3. Test
  `suffixes_order_decides_not_rule_order`.
- [x] **#49 — Makefile macros leak into every recipe's environment.** ✓ fixed
  2026-08-27 (P1), in the commit that closed #36. `init_env` did
  `command.envs(macros)` unconditionally — inert only while `Make::macros` was
  empty, and a live leak the moment #36 wired it up. POSIX 105869: macros
  defined in a makefile "shall not be added to the environment of make if they
  are not already in its environment". `exported_macros` now filters to names
  make already inherited, and `SHELL` is excluded outright. Whether an
  already-present variable is *updated* is unspecified (105871); we update it,
  matching GNU, except under `-e` where the environment wins. Tests
  `makefile_macro_absent_from_env_is_not_exported`,
  `makefile_macro_present_in_env_is_updated`, `dash_e_lets_the_environment_win`.
  _Known residual: `main.rs` appends command-line macros to the makefile text,
  so they are indistinguishable from makefile macros and are also not exported.
  POSIX 105866 requires that they be. Recorded as **#50**._
- [x] **#50 — Command-line `macro=value` operands are not exported to recipes.**
  ✓ fixed 2026-08-27 (P7). POSIX 105866 requires them in make's environment;
  putting them there is also what makes them visible to recipes, since #49's
  filter exports a macro only if make already inherited its name. `MAKEFLAGS`
  and `SHELL` are excluded as the spec requires.
- [x] **#52 — `$(eval ...)` is unimplemented.** ✓ fixed 2026-08-27. It expands
  its argument and hands the text to the reader to be read back as makefile
  source, so it can define macros, rules and conditionals.

  A function cannot reach the reader — the reader owns the macro table and the
  output buffer, and expansion runs inside a `&mut self` call on it — so eval
  leaves text in a queue on the shared `func::Expansion` and the reader drains
  it, re-entering `read` the way `include` already does. Four things this design
  had to get right, each now a test:
  * **Ordering.** The drain runs *before* the producing line is emitted. A rule
    header emitted after it would close the enclosing rule and steal its recipe.
  * **`$$`.** One `$` level is consumed, matching GNU. `substitute` passes `$$`
    through untouched, so a template's `$$(CC)` would otherwise keep both
    dollars and reach the shell as a command substitution — a silent wrong
    build, not an error.
  * **Isolation.** Eval'd text gets its own conditional stack; sharing let an
    eval'd `endif` close a conditional belonging to the enclosing file.
  * **Laziness.** eval takes one argument, so its text is never split on commas.

  Verified byte-identical to GNU Make 4.3 on the generated-rules idiom
  `$(foreach p,foo bar,$(eval $(call tpl,$(p))))`. Thirteen tests in
  `parser.rs` `mod eval`.
  _Recipe lines are expanded after the reader has finished, so `$(eval ...)`
  there is refused rather than silently dropped._

- [x] **#53 — `VPATH`/`vpath` are unimplemented.** ✓ partially fixed 2026-08-27
  (P7). The `VPATH` macro is honoured: a prerequisite that does not exist as
  written is looked up in each listed directory, for both staleness checks and
  inference-rule matching, and `$<` names where the file was actually found —
  byte-identical to GNU on the probe. _Residual: the `vpath` **directive** with
  its per-pattern search paths is still unimplemented; recorded as **#55**._
- [x] **#55 — The `vpath` directive is unimplemented.** ✓ fixed 2026-08-27.
  All three GNU forms: `vpath PATTERN DIRS` appends directories for a pattern,
  `vpath PATTERN` clears that pattern, and a bare `vpath` clears every pattern.
  Entries are recorded by the reader in declaration order and carried through to
  `Make`; `resolve_vpath` consults them before the blanket `VPATH` macro, first
  matching pattern winning. Verified against GNU Make 4.3 on all three forms.
  Tests: nine in `parser.rs` `mod vpath`, plus
  `vpath_directive_finds_a_prerequisite`.
  _Note: `vpath` was previously not recognized at all — it fell through to being
  treated as an ordinary line, so a makefile using it was silently mis-parsed._

- [x] **#54 — The built-in inference rules are display strings, not rules.**
  ✓ fixed 2026-08-27 (P7). POSIX's default rules (`.c.o`, `.c`, `.sh`, `.y.o`,
  `.l.o`, `.y.c`, `.l.c`, `.c.a`) and the macros they use (`CC`, `CFLAGS`, `AR`,
  `ARFLAGS`, `YACC`, `LEX` …) existed only inside the `-p` dump table, so
  `make f.o` with an `f.c` present reported `no target 'f.o'`. Nearly every real
  makefile leans on the built-in `.c.o`. They are now real rules, seeded from
  makefile text so they go through the same reader, appended after the
  makefile's own so a user rule of the same name wins, and suppressed by `-r`.
  Found by the P7 corpus run, not by the audit. Tests
  `builtin_c_to_o_rule_applies`, `dash_r_suppresses_the_builtin_rules`.
- [x] **#56 — A `<tab>`-indented line outside any rule is a parse error.**
  ✓ fixed 2026-08-27 (P7). A tab-indented line is a command line only inside a
  rule; outside one it is ordinary text. Real makefiles indent continuation and
  comment lines with tabs — Lua's makefile does — so `\t# note` between two macro
  definitions must not be `command line before any target`. Found by the corpus.
- [x] **#57 — The echoed recipe is not the recipe that runs.** ✓ fixed 2026-08-27
  (P7). Internal macros were expanded *after* the line was printed, so `-n` and
  the non-silent echo showed `cc -c -o $@ $<` while executing the right command.
  Since telling the user what will run is exactly what `-n` is for, the
  expansion now happens first. Found by the corpus.

- [x] **#58 — A self-referential `$(call ...)` exhausts the stack.** ✓ fixed
  2026-08-27. `A = $(call A)` aborted with `fatal runtime error: stack overflow`
  and dumped core. `MAX_EXPANSION_ROUNDS` bounds the rounds *within* one
  expansion frame, but `substitute → func::call → expand → substitute` is a real
  recursion and had no depth bound at all. A shared `func::Expansion` carries a
  depth counter through every nested `Ctx`, capped at 200, and reports a
  recursive definition instead. Found while designing #52, which multiplies the
  ways to reach it. Tests `self_referential_call_is_capped_not_a_crash`,
  `mutually_recursive_calls_are_capped`, `recursion_through_foreach_is_capped`,
  `finite_nesting_still_expands`.
- [x] **#59 — A function error grows a newline per nesting level.** ✓ fixed
  2026-08-27. The error crosses the `Result<String, String>` boundary between
  `func` and the preprocessor once per level, and `FunctionFailed`'s `Display`
  used `writeln!`, so each `to_string()` appended another newline — a 200-deep
  recursion printed its one-line message followed by 202 blank lines. Test
  `a_depth_error_is_reported_once`.

- [x] **#60 — `$<` is empty in an ordinary rule.** ✓ fixed 2026-08-27. POSIX
  defines `$<` only for inference rules, but GNU sets it to the first
  prerequisite in an explicit rule too, and generated rules lean on it: a
  template emitting `$(1).o: $(1).c` with `$<` in its recipe is the common
  shape. `Rule::run` now supplies the first prerequisite as the input file,
  which is what `run_for_pattern` already did — so that near-duplicate collapses
  into it. Found while checking #52 against GNU.

- [ ] **#61 — `.SCCS_GET` is accepted but inert.** The special target is parsed
  and validated, but this make performs no SCCS retrieval, so nothing ever runs
  its recipe. Until now the recipe was stored in the `-p` mirror table and read
  by nothing, which made the gap look like a feature; deleting that table for
  #43 exposed it. Either implement SCCS retrieval or reject the target, but do
  not keep accepting it silently.

- [x] **#62 — A function applied to an automatic variable sees its literal text.**
  ✓ fixed 2026-08-28. A call whose argument mentions `$@`/`$<`/`$^`/… is now left
  verbatim while reading — only the rule stage can supply one — and evaluated
  there, after the automatic variables have values. `substitute_internal_macros`
  also had to stop re-emitting a bracketed non-internal form *verbatim*: it was
  skipping the `$^` nested inside `$(notdir $^)`, which is the whole point of
  that pass. Byte-identical to GNU on `notdir`, `dir` and `basename` over `$^`.
  Tests `functions_see_automatic_variables`, `a_shell_variable_in_a_recipe_survives`.
  _Only the deferred call is evaluated at rule time, not the whole line: a
  line-wide pass turned a shell `$MAKEFLAGS` into `$M` followed by `AKEFLAGS`._

## Minor

- [x] **#43 — `-p` prints a `Debug` dump of the built-in table, never the
  makefile.** ✓ fixed 2026-08-27. `-p` now writes the real database in makefile
  syntax: the makefile's own macros alongside the built-ins, its rules with
  their recipes, the inference rules, `.DEFAULT`, and any `vpath` search paths.
  POSIX 105395 mandates "the complete set of macro definitions and target
  descriptions" and leaves the format unspecified; makefile syntax was chosen so
  the dump round-trips — `make -p` output re-parses as a makefile and its rules
  still fire. The trailing newline is there, and `-p` no longer suppresses the
  build (POSIX says that only of `-q`).

  POSIX Example 8, `make -p -f /dev/null 2>/dev/null`, keeps working — but
  deliberately now. It used to succeed only because an empty makefile is a
  *parse error* and the error path happened to print the defaults; that branch
  now builds an empty makefile so the built-ins are seeded and dumped on purpose.

  `Config.rules`, the `BTreeMap` mirror whose own doc comment said it existed
  "for the `-p` dump", is deleted along with the four sites that maintained it
  and the malformed `"XSI GET=get"` entry, which was never `NAME=value` syntax
  and never a real macro. Tests `dash_p`, `dash_p_reports_the_parsed_makefile`,
  `dash_p_output_is_a_makefile`.

- [x] **#44 — `dbg!()` calls are shipped, and `parse_include` hardcodes
  `"variables.mk"`.** ✓ fixed 2026-08-27 (P1). Both `dbg!()` calls and the
  `parse_include` scaffolding were deleted with the old parser, as recommended
  rather than repaired.
- [x] **#45 — `Target::new` calls `String::leak()` on every construction.**
  ✓ fixed 2026-08-27 (P4). `Target` owns `String`s and `name()`/`AsRef<str>`
  borrow from `&self`, so nothing leaks per visit. No test changed.
- [x] **#46 — `find_files_with_extension` builds a walk queue it never pushes
  to.** ✓ fixed 2026-08-27 (P5). Deleted with the working-directory scan it
  served. An inference rule applied to a real target goes through
  `run_for_target`, which knows the stem; scanning for every file with the source
  suffix was both dead structure and the mechanism behind #38's silent no-op.
- [x] **#48 — Backslash-newline folding leaves two spaces where POSIX and GNU
  leave one.** ✓ fixed 2026-08-27 (P7). Any blank already sitting before the
  backslash is dropped, so the continuation collapses to exactly one space.
  `SRC = one \` + `      two` now gives `one two`, matching GNU Make 4.3.
## Design note

The crate has no dependency graph: `Make` holds a `Vec<Rule>` and answers every
question by linear scan and re-traversal, which is what produces #28, #29, #30
and #31 together. An explicit `HashMap<TargetId, Node>` DAG built once, with
`Unvisited`/`InProgress`/`Done` colouring, gives cycle detection, memoization
and `-j` double-build safety from the same state.

The `rowan` CST is the wrong tool for a line-oriented format, and the parser
built on it cannot express two targets on a line (#27), a slash in a filename
(#26), or a variable definition (#36). Macro expansion should be lazy and
scoped per rule rather than a whole-file textual fixpoint (#32, #34, #35), and
target names should be `OsStr`/`PathBuf` rather than `String` + `leak()`
(#39, #45).

## From the 2026-08-27 branch review

Thirteen defects found by review, three more found while verifying them. All
reproduce against the built binary and were diffed against GNU Make 4.3.

**#75-#78 were planned and dropped.** The design pass named them N2-N5 and the
plan listed them for its execution phase; that phase shipped the numbered
findings and reported complete without them. They are unchanged since.

### Critical

- [x] **#63 — Inference rules never compare timestamps.** ✓ fixed 2026-08-28.
  The inference branch now goes through `run_rule_with_prerequisites` like every
  other path. That needed the staleness check to stop re-deriving the rule by
  name — `rule_by_target_name` excludes pattern rules and never sees
  `inference_rules`, so it reported no prerequisites for exactly these targets,
  and routing alone would have flipped "always rebuilds" into "never rebuilds".
  Prerequisites now come from the resolved rule, with an inference rule's
  implicit source derived from its suffixes. Test
  `an_inference_rule_is_not_rerun_when_current`.
- [x] **#64 — Pattern rules never compare timestamps, and `$?` is wrong.**
  ✓ fixed 2026-08-28. Same routing. `$?` is now the prerequisites newer than the
  target rather than all of them, and prerequisites are brought up to date before
  staleness is judged so a rebuilt one counts whatever the timestamps say. Tests
  `a_pattern_rule_is_not_rerun_when_current`,
  `question_mark_lists_only_newer_prerequisites`.
- [x] **#65 — A self-referential pattern rule deadlocks.** ✓ fixed 2026-08-28.
  Two halves: the pattern branch never called `find_cycle`, and
  `Edges::prerequisites_of` went through `rule_by_target_name`, which filters out
  pattern rules — so even calling the check would have seen no edge. The edge
  source now falls back to instantiating a matching pattern, and the branch runs
  the check. `%.a: %.a` reports a recursive prerequisite instead of blocking on
  the ledger forever. Test `self_referential_pattern_is_a_cycle_not_a_hang`.
  _Divergence noted: GNU drops a circular dependency with a warning and
  continues; we error, consistent with how named-rule cycles already behave._

- [x] **#67 — A multi-line macro deletes every built-in rule.** ✓ fixed
  2026-08-28. The round-trip through generated makefile text is gone: the rule
  text is a constant, so only its recipe lines need the makefile's macros, and
  those are expanded directly. Nothing carries a macro value back through a
  parser, which was the whole failure mode — a `define` body has a newline in
  it, so the generated text stopped parsing and `if let Ok` threw the error
  away. Any expansion failure is now reported. Tests
  `a_multi_line_macro_does_not_delete_the_builtin_rules`,
  `a_broken_builtin_expansion_is_reported`.
- [x] **#68 — `$(if)`/`$(or)`/`$(and)` recurse unbounded into a crash.** ✓ fixed
  2026-08-28. All three now take the depth guard the other lazy functions had.
  #58 added it to `eval`, `foreach` and `call` and stopped there, so half the
  class stayed open. Tests `recursion_through_if_is_capped`,
  `recursion_through_or_and_is_capped`.
- [x] **#69 — `$(wordlist)` panics on an inverted range.** ✓ fixed 2026-08-28.
  An inverted span is an empty list, as GNU. Test
  `wordlist_with_an_inverted_range_is_empty`, which also pins the single-word
  and past-the-end cases.
### Major

- [x] **#66 — `VPATH` is skipped for a prerequisite with no rule of its own.**
  ✓ fixed 2026-08-28. The up-to-date probe and the staleness comparison both
  resolve through `resolve_vpath`, and `Rule::run` takes the same resolver
  `run_for_target` already had, so `$<` names where the file was found.
  Byte-identical to GNU on the probe. Test
  `vpath_supplies_a_prerequisite_with_no_rule`.
- [x] **#70 — A nested reference in a `$(x:a=b)` replacement is rejected.**
  ✓ fixed 2026-08-28. The spec is read with `read_balanced`, the same reader the
  function path uses, so `$(SRC:%.c=$(D)/%.o)` yields `obj/a.o obj/b.o`. Test
  `test_subst_replacement_may_nest_a_reference`.
- [x] **#71 — Command-line macros do not override macros used in rule headers.**
  ✓ fixed 2026-08-28. They are now seeded into the reader's macro table before
  the read, so a header earlier in the file sees them. Seeding alone would have
  been the same bug in the other direction — a later `OBJ = a.o` in the makefile
  would clobber the command line — so the table carries a `locked` set that an
  ordinary assignment cannot write through, with GNU's `override` directive as
  the documented way past it. `Makefile::parse_with_macros` is the entry point.
  Tests `a_command_line_macro_overrides_a_rule_header`,
  `a_later_assignment_does_not_clobber_a_command_line_macro`,
  `override_defeats_a_command_line_macro`.
- [x] **#72 — A lone `$` rejects the whole makefile.** ✓ fixed 2026-08-28. `$X`
  for any single character is a reference to a macro named `X`, matching GNU;
  an unknown one expands to nothing rather than erroring. `@echo "cost 5 $ each"`
  now prints. Test `test_a_lone_dollar_is_a_single_char_reference`.
- [x] **#73 — `?=` and `+=` ignore the environment.** ✓ fixed 2026-08-28. Both
  operators now resolve their existing value through the macro table *then* the
  environment, so `CC ?= gcc` with `CC=clang` inherited yields `clang` and
  `CC += -Wall` yields `clang -Wall`, matching GNU. Test
  `conditional_and_append_see_the_environment`.
- [x] **#74 — A bare `export NAME` fails to parse.** ✓ fixed 2026-08-28.
  `export`/`unexport` are directives now, like `vpath`, and the names they carry
  are threaded through `Preprocessed` to `Rule::run` so the directive actually
  adds to the recipe environment rather than merely parsing. Bare `export` marks
  every macro. Tests `export_directive_puts_a_macro_in_the_environment`,
  `a_macro_is_not_exported_without_the_directive`.
- [x] **#75 — `INTERRUPT_FLAG` is never cleared and is shared under `-j`.**
  ✓ fixed 2026-08-28. It is a list of in-flight targets now, entered once per
  target by an RAII guard so every exit path from the recipe loop clears it,
  and the handler cleans up all of them. Probed old against new: a SIGINT
  during a two-worker build left one partial file on disk before, none now.
  Test `an_interrupt_cleans_up_every_target_in_flight`.
- [x] **#76 — `-q` calls `process::exit` from a worker thread.** ✓ fixed
  2026-08-28. The recipe loop reports "not up to date" and `main` chooses the
  status. _Correction to the finding: the abandoned-siblings scenario is not
  reachable — `build_prerequisites` already excludes `-q` from its parallel
  predicate, so a `-q` build is serial whatever `-j` says, and a probe of old
  against new shows no behavioural difference. What is fixed is the structure:
  the exit status is decided where the other exit statuses are decided._
  Test `dash_q_answers_with_a_status_and_no_output`.
- [ ] **#77 — A bare `.PHONY:` marks every rule phony.** POSIX 105677 says a
  `.PHONY` with no prerequisites shall be ignored; instead nothing is ever
  up to date. Planned as N4.
- [ ] **#78 — `.IGNORE`/`.SILENT`/`.PRECIOUS` apply per rule, not per target.**
  `a b: dep` with `.IGNORE: a` silences `b` too, because both targets live on one
  rule. Planned as N5.

## Acceptance gate

`/tmp/makecorpus.sh` (rebuilt per session, per the project's convention for gate
scripts). Three checks per corpus project:

| check | what it does | catches |
|---|---|---|
| build | make succeeds, artifact exists, artifact runs | outright breakage |
| noop | `make -n` straight after a build prints nothing | always-rebuild |
| diff | `make -n` on a clean tree matches `gmake -n` | silent divergence |

Two things the earlier single-check gate got wrong, both worth keeping:

- **The corpus needs a phony target directly over an inferred one.** Both original
  projects put a real-file intermediate between the phony root and the pattern
  rule, so the traversal short-circuits on a rerun and an always-rebuild bug
  stays invisible. The `phony` project exists to expose exactly that.
- **The differential is opt-in per project.** A project relying on the *built-in*
  `.c.o` legitimately differs from GNU: POSIX specifies
  `$(CC) $(CFLAGS) -c $<`, GNU's is `-c -o $@ $<`. Lua is exempted for that
  reason, with the reason recorded in the run output rather than silently.

A gate is only worth having if it can fail. On the tree of 2026-08-28 this one
fails on two real defects (`phony`/noop, `synthetic`/diff) and exits nonzero.

## Re-probe of the 2026-06-12 findings (2026-08-27)

All 25 old findings were ticked. #13 and #24 were refuted by the crate review
(they are #40 and #43 above). The remaining 23 were re-probed against
`target/release/make`, with GNU Make 4.3 as the reference wherever POSIX leaves
room. **Eighteen hold, three are refuted, two are partial.**

Recorded so the next reader does not repeat the work. A row here is a claim
about the binary as of 2026-08-27, not about the source.

| Old | Claim | Result |
|---|---|---|
| #1 | recipe line with `=` no longer aborts the parse | holds — `echo VAR=1` and `[ x = y ]` both run |
| #2 | `.POSIX` accepted | holds |
| #3 | missing `include` errors instead of panicking | holds — `cannot open include file …`, exit 4 |
| #4 | `-k` exits 0 when everything succeeds | holds |
| #5 | command-line `macro=value`, taking precedence | holds — overrides the makefile's own value |
| #6 | `$(SRC:.c=.o)` substitution | holds — `a.c b.c` → `a.o b.o` |
| #7 | continuation folds to a single space | **refuted** — folds to two; see #48 |
| #8 | single-suffix inference rules | holds — `.c:` applied to `foo` |
| #9 | `-j`, `.WAIT`, `.NOTPARALLEL` | holds — and `.WAIT` genuinely orders under `-j4` |
| #10 | multiple `-f`, concatenated in order | holds |
| #11 | shell `-e` for non-ignored recipes | holds — `false; echo …` does not leak; `-` prefix still ignores |
| #12 | recipe shell from the `SHELL` macro | **refuted** — runs under `/bin/sh`; see #36 |
| #14 | `$?` is newer-only | holds — one of two prerequisites listed |
| #15 | `$^`, `$+`, `$(@D)`, `$(@F)` | holds — byte-identical to GNU |
| #16 | `.SUFFIXES` order authoritative | **partial** — membership and clear work, order does not; see #47 |
| #17 | signal registration correctly gated | **refuted** — overrides an inherited `SIG_IGN`; see #41 |
| #18 | bare `.PRECIOUS` protects every target | holds — verified against a control where a plain target *is* deleted |
| #19 | `-include` implemented; `includedir=` not mistaken for it | holds |
| #20 | handler resets to `SIG_DFL` and re-raises | holds — `WIFSIGNALED`, signal 2, not `exit(130)` |
| #21 | cleanup honors `.PHONY` | holds — same control as #18 |
| #22 | special targets additive across occurrences | holds — two `.PHONY:` lines both take effect |
| #23 | diagnostics internationalized | **partial, as its own text admits** — 34 `gettext` sites, 10 raw `eprintln!` remain, mostly `main.rs` error paths |
| #25 | every error path exits >1 | holds — 2 / 4 / 6 for recipe / parse / no-target, and `-q` correctly exits 1 |

Three probes were confounded on the first pass and needed a second: #15 failed
only because of #26 (the slash in `sub/out`); #18 and #21 both "passed" until a
plain-target control proved the cleanup path runs at all; and #20 needed
`waitpid` to tell death-by-signal from `exit(128+n)`, which a shell's `$?`
cannot distinguish.
