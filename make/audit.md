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

## Critical

Each of these blocks ordinary use, crashes, hangs, or silently produces the
wrong build.

- [ ] **#26 — The lexer omits `/` from `is_valid_identifier_char`, so no path
  works.** `parser/lex.rs`. `all: src/foo.c` lexes as IDENTIFIER `src`, ERROR
  `/`, IDENTIFIER `foo.c`; `Rule::prerequisites()` keeps only IDENTIFIERs, so
  the list becomes `["src", "foo.c"]` → `make: no target 'foo.c'` (exit 6). A
  slash in *target* position is a hard parse error. Nothing with subdirectories
  builds.
- [ ] **#27 — A rule may name only one target.** `parser/parse.rs`. `parse_rule`
  does one `expect(IDENTIFIER)` then `self.tokens.pop()`, discarding whatever it
  popped before reporting. `a b:` + `\techo hi` → `make: parse error: expected
  ':'` (exit 4). Multiple targets per rule is core POSIX syntax.
- [ ] **#28 — An indirect dependency cycle overflows the stack.** `lib.rs`.
  `_are_prerequisites_recursive` seeds `visited`/`stack` with the root and never
  inserts during the DFS, so only cycles returning to the root are caught.
  `a: b` / `b: c` / `c: b` → `thread 'main' has overflowed its stack / fatal
  runtime error`, exit 134. `RecursivePrerequisite` is unreachable otherwise.
- [ ] **#29 — A shared prerequisite is built repeatedly, and concurrently under
  `-j`.** `lib.rs`. There is no "already built" set. Diamond `all: x y`,
  `x: shared`, `y: shared`: the `shared` recipe runs twice serially, and under
  `-j4` two shells run it simultaneously. Two processes writing the same output
  file corrupts the target.
- [ ] **#30 — Only the first rule for a target is used; later ones are silently
  dropped.** `lib.rs` `rule_by_target_name`. POSIX allows a target's
  prerequisites to be split across rules. `all: a` then `all: b` + `echo done`
  builds only `a`; `b` is never built and `echo done` never runs. Exit 0.
- [ ] **#31 — `get_newer_prerequisites` has no memoization; DAG traversal is
  exponential.** `lib.rs`. Each prerequisite re-walks the whole subgraph. A
  23-node diamond chain (`n{i}: n{i+1} n{i+1}`) does not finish in 30 s where
  GNU make is instantaneous. With #29, any makefile with shared headers is
  unusable.
- [ ] **#32 — Both preprocessor fixpoint loops are unbounded and hang.**
  `parser/preprocessor.rs`. Neither the include-expansion loop nor the
  macro-substitution loop has an iteration cap or self-reference detection. A
  file that includes itself loops forever appending content; `A = $(A)x` never
  terminates.
- [ ] **#33 — Under `-k`, a target whose prerequisite failed still runs its
  recipe.** `rule.rs`. The failed prerequisite `break`s out with `Ok`, so the
  dependent builds from inputs that were never produced. `all: a b` with
  `a:` → `false` prints `make: execution error: 1` and then runs `echo all`.
  `-k` must skip such targets. The `KEEP_GOING_ERROR` global also cannot say
  *which* target failed under `-j`.

## Major

- [ ] **#34 — An undefined macro is fatal, environment variables are not macros,
  and comments are expanded.** `parser/preprocessor.rs`. POSIX requires an
  undefined macro to expand to the empty string and makes environment variables
  macros unconditionally. `echo $(UNDEF)` → `parse error:
  UndefinedMacro("UNDEF")`; `echo $(HOME)` fails identically without `-e`; and
  because substitution runs over comment text, `# price is $5` aborts the whole
  parse with `UndefinedMacro("5")`.
- [ ] **#35 — Include processing is handed an empty macro table.**
  `parser/preprocessor.rs` calls `process_include_lines(&source, &HashMap::new())`,
  discarding the `table` computed on the line above. `include $(TOP)/inc.mk`
  fails with `UndefinedMacro("TOP")` even when `TOP` is defined immediately
  before it.
- [ ] **#36 — No `VARIABLE` node is ever built, so `SHELL` and the recipe
  environment are dead.** `parser/parse.rs`. `Makefile::variable_definitions()`
  always returns empty and `Make::macros` is always `[]`, which makes `Macro`,
  `Rule::init_env` and the `SHELL` lookup in `rule.rs` unreachable machinery.
  `SHELL=/bin/echo` is silently ignored — the recipe still runs under `/bin/sh`.
- [ ] **#37 — `$*` expands to the whole target name, not the stem.** `rule.rs`.
  POSIX: the target with the suffix deleted. A `.c.o:` rule on `f.o` yields
  `STAR=[f.o]`, should be `f`. This also breaks the crate's own built-in `.c.a`
  rule in `config.rs`, whose `$(AR) $(ARFLAGS) $@ $*.o` archives the wrong
  object.
- [ ] **#38 — The inference-rule classifier misfires on any dot-target beginning
  with a known suffix.** `special_target.rs`. Classification is "starts with a
  suffix in `.SUFFIXES`", so `.config:` is filed as an inference rule: with it
  first in the file, a bare `make` builds `real` instead. `.cargo/…` and any
  `.o`- or `.a`-prefixed dot-target are affected. `InferenceTarget::from` and
  `to` also compute the identical expression (both `strip_prefix`), so for
  `.c.o` both return `.o` and both accessors are dead.
- [ ] **#39 — A non-UTF-8 target operand panics.** `main.rs` calls
  `target.into_string().unwrap()`. `make $'\xff'` → `panicked at … called
  Result::unwrap() on an Err value: "\xFF"`, exit 101. Targets are filenames and
  must not be required to be UTF-8; `Target` is `String`-based throughout.
- [ ] **#40 — `MAKEFLAGS` is never synthesized from the command line, so it does
  not reach sub-makes.** `rule.rs`. POSIX requires make to propagate its options.
  `make -k` invoking `$(MAKE)` gives the sub-make `MAKEFLAGS=[]`. Only
  env-provided flags propagate today (see the provenance note above).
- [ ] **#41 — `register_signals()` overrides an inherited `SIG_IGN`, and runs
  once per recipe.** `rule.rs` installs handlers unconditionally inside the
  per-recipe loop. POSIX requires a signal ignored on entry to stay ignored, so
  make invoked with SIGINT masked dies on a Ctrl-C the caller deliberately
  suppressed.
- [ ] **#42 — `-t` touches `.PHONY` targets.** `rule.rs`. `make -t clean`
  creates a file named `clean`, after which every `make clean` reports it up to
  date and never runs the recipe.

## Minor

- [ ] **#43 — `-p` prints a `Debug` dump of the built-in table, never the
  makefile.** `main.rs` emits `{:?}` of a `BTreeMap` with no trailing newline,
  containing only the hardcoded built-ins — none of the makefile's own macros or
  targets — and its `.MACROS` entry carries a literal `"XSI GET=get"` key. The
  spec leaves the format unspecified, but not the content.
- [ ] **#44 — `dbg!()` calls are shipped, and `parse_include` hardcodes
  `"variables.mk"`.** `parser/parse.rs`. A bare `include` line dumps the whole
  token vector to stderr and injects a literal `variables.mk` identifier into
  the tree. The path is unreachable-by-luck scaffolding — the preprocessor
  normally consumes `include` — and should be deleted rather than fixed.
- [ ] **#45 — `Target::new` calls `String::leak()` on every construction.**
  `rule/target.rs`. `build_target` constructs one per visit, so the leak scales
  with traversal count rather than with distinct targets — and #31 makes that
  count exponential.
- [ ] **#46 — `find_files_with_extension` builds a walk queue it never pushes
  to.** `rule.rs`. The `VecDeque` is initialized and drained but never appended
  to, so the loop always runs exactly once. The structure implies a recursive
  directory walk that does not happen.

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
