# POSIX.1-2024 Conformance Audits — `cc/` utilities

This file collects per-utility POSIX conformance audits for the C compiler
crate. Each audit follows the playbook in `audits.md`.

**Date:** 2026-08-01
**Crate:** `cc/` — 89,049 lines of implementation (excluding tests) +
18,459 lines of tests (231 `#[test]` functions)
**Spec slices:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/{c17,cflow,ctags,cxref}.md`

| Utility | Binary | Implementation | Spec slice | Spec pages |
|---|---|---|---|---|
| `c17` | `c17` (was `pcc`) | `cc/main.rs` (1488) + the whole compiler (~89 kloc) | `c17.md` (588 lines) | 2717–2730 |
| `cflow` | `cflow` | `cc/cflow.rs` | `cflow.md` (138 lines) | 2637–2640 |
| `ctags` | `ctags` | `cc/ctags.rs` | `ctags.md` (220 lines) | 2865–2869 |
| `cxref` | `cxref` | `cc/cxref.rs` | `cxref.md` (114 lines) | 2879–2881 |

## Status

> **`cflow`, `ctags`, and `cxref`: all 32 findings closed (2026-08-01).**
> Landed across six commits on branch `updates`; each item is ticked below with
> a one-line note. All three were promoted to *Stage 6 — Audited* in
> `README.md`. New conformance tests live in `cc/tests/tools/posix.rs`.
>
> **Stale-box sweep (2026-08-08).** The findings were ticked in 2026-08-01, but
> the downstream conformance-matrix and test-coverage rows in those three
> sections were never re-ticked. Each was re-verified against the current source
> and by probing the built binaries. Of the 46 rows in those sections, **37 are
> now ticked**; the remaining **nine** were narrowed to name only what is still
> missing — a `.y` operand test and a locale-blind `LC_COLLATE` test (`cflow`);
> a pinned `LC_CTYPE`, an `LC_COLLATE` test, the default `-f tags` filename,
> pattern escaping, and `-a` append-boundary sort order (`ctags`); a
> locale-blind `LC_COLLATE` test and the `LC_MESSAGES` half that depends on
> `c17`'s #U7 (`cxref`).
>
> The `c17` section was swept the same way and **nothing there was stale** —
> every one of its findings was re-probed against the built binary and remains
> genuinely open, consistent with the note below.
> Cross-cutting note 4 was corrected — `gettext()` is no longer a stub.
>
> **`c17`: remediation complete (2026-08-08), nine phases.** Each closed
> finding is ticked below with a `✓ fixed (Phase N)` note naming the change and
> its tests. **79 open boxes at the start; 13 remain**, and every one of those
> is either an explicit out-of-scope decision, a reclassified finding whose
> premise did not survive checking, or a defect discovered *during* the work.
> Nothing was ticked without a test.
>
> Four new suites exist that had no counterpart before:
> `cc/tests/driver/` (operand handling — the previous harness could only ever
> compile one source with one `-o`, which is why #U1 survived 231 tests),
> `cc/tests/diagnostics/` (programs that must be **rejected** — nothing could
> express that before), `cc/tests/preprocessor/conformance.rs` (asserts on `-E`
> *text*, where every Phase 3 defect had been hiding), and
> `cc/tests/c11/{literals,headers}.rs`.
>
> `c17` stays at *Stage 3* in `README.md`: #X1, #X3 and #P1/#X2 are open by
> decision, and #C1/#C2 are live codegen defects.
>
> Four items are **deliberately out of scope** for this effort and stay open:
> **#X3** (`_Generic`) and **#X1** (`_Atomic` via ordinary operators) are not
> being implemented; **#P1/#X2** stays at `201112L` — the version constant will
> not be raised until full C17 is available, and **no `-std=` multi-version
> switching will be added**; **#L7** was never reproduced. Because #P1/#X2 is out
> of scope, the row pinning `cc/tests/c11/core.rs:301` to `201112L` stays as-is.
>
> **Phase 1 (#U1, #U2, #U10) — landed.** The driver was restructured to compile
> every operand and then link once. Nine boxes closed. New suite:
> `cc/tests/driver/mod.rs`.
>
> **Phase 2 (#U3, #U4, #U6, #U9) — landed.** Link-line order recovered via the
> new `cc/linkargs.rs`; the four missing mandated options added; scratch files
> moved into one `TMPDIR`-aware temporary directory. Nine more boxes closed.
>
> **Phase 9 (tools tail + documentation) — landed. Remediation complete.**
> Two of the residual tool rows turned out to be real defects, not missing
> tests: `ctags` corrupted its own search patterns on any non-UTF-8 source
> (`from_utf8_lossy` put U+FFFD *inside* the pattern, so it no longer matched
> the line it pointed at), and `ctags -a` left the tags file a run of
> separately-sorted blocks. Both fixed. The three locale tests that could not
> distinguish fixed from broken — they asserted only the POSIX locale, which
> *is* byte order — were replaced by one two-locale test. #L8, #X10, #H11 and
> #P12 closed by correcting the documents they are about.
>
> **Phase 8 (#U5, #U7) — landed.** `-E` now emits the mandated line markers.
> #U7 is *partly* closed: the diagnostic labels and every driver message are
> translated, but the message bodies are `format!`-built and need a separate
> restructuring pass, which is recorded rather than pretended away.
>
> **Phase 7 (#H1, #X6, #X7, #X8, #P7, #H9, #H10, #H12) — landed.** `stdint.h`
> bundled; `stdatomic.h` completed; `CMPLX` added; the conditional feature
> macros defined; POSIX levels bumped to 2024. Ten more boxes closed, and
> **#H8 and #H4 were reclassified rather than closed** — #H8's premise turned
> out to be factually wrong, and #H4 cannot be adjudicated on x86_64.
> Remediation surfaced three defects the audit never recorded, filed as
> #C1-#C3.
>
> **Phase 6 (#X5, #L4, #P11) — landed.** All five encoding-prefixed literal
> forms implemented with correct element widths and values; adjacent literals
> of different encodings concatenate; trigraphs added behind `--trigraphs`.
> Five more boxes closed. New suite `cc/tests/c11/literals.rs`.
>
> **Phase 5 (#L1, #L2, #L3, #L5, #L6, #X4, #X9) — landed.** C constraint
> diagnostics: the compiler now rejects code it used to accept silently. Nine
> more boxes closed, including the "largest structural gap" — there was no way
> to assert a program is *rejected*, so `cc/tests/diagnostics/` and the
> `compile_expect_error`/`compile_expect_ok` helpers were built first.
>
> **Phase 4 (#P3, #P4, #P5, #P15, #P16) — landed.** Preprocessor diagnostics:
> macro redefinition, argument count, and `#`/`##` placement are all checked;
> `-D` can define a function-like macro; GNU named variadics bind every
> trailing argument. Seven more boxes closed.
>
> **Phase 3 (#P2, #P6, #P9, #P10, #P13, #P14) — landed.** Preprocessor
> correctness: the null directive no longer deletes a line, `#if` arithmetic is
> `intmax_t`/`uintmax_t`, bundled headers no longer shadow the user's own,
> a comment counts as whitespace, `#line` diagnoses bad operands, and `&&`/`||`
> short-circuit. Eight more boxes closed. New suite
> `cc/tests/preprocessor/conformance.rs` asserts on `-E` text, which no test
> did before — every defect in this phase was invisible to an exit-code test.

---

## Naming: `pcc` → `c17`

This audit was written against the name `c17` throughout, while the binary was
still shipped as `pcc`. The rename **has since landed** (see #U8): the binary,
its internal `--c17-*` flags, its diagnostics prefix, its `__VERSION__` string,
its assembly/DWARF producer strings, and its temp-file names all say `c17` now.
Everything below reads literally.

The rename was deliberately kept cosmetic. In particular it did **not** bring
the language level with it — #P1/#X2 is still open, so the binary named `c17`
advertises `__STDC_VERSION__ == 201112L`. That is the one remaining place where
the name overpromises, and closing it is PR D's job.

Note the spec's own FUTURE DIRECTIONS (c17.md 88407–88411): *"Unlike all of the
other non-OB-shaded utilities in this standard, a utility by this name probably
will not appear in the next version of this standard. This utility's name is
tied to the current revision of the ISO C standard."* The name is a moving
target by design; `c17` is correct for POSIX.1-2024.

---

## Method and verification status

Eight read-only subagents performed the spec-vs-source pass (one per utility,
plus four covering distinct areas of the compiler: driver surface,
preprocessor, C89/C99, C11/C17, headers/runtime). **All of them ran without a
shell**, so every claim they produced was static analysis only.

All Critical and Major findings below were therefore **independently
re-verified by executing the freshly built binaries** at HEAD `005047eb`.
Findings confirmed by execution are marked **[probed]**. Findings that are
source-derived only are marked **[static]** — they are not less likely to be
true, but they have not been demonstrated end-to-end.

One agent claim did **not** survive verification and is recorded as such
(#L7). Two prompt premises supplied to the agents were themselves wrong and
were corrected by the agents against the spec (see the `ctags` section).

---

## Cross-cutting findings

Four themes recur across all four utilities.

**1. Exit status is not propagated.** `cflow`, `ctags`, and `cxref` all print
diagnostics to stderr and then return 0. Every one of the three has an
`EXIT STATUS` section reading *"0 Successful completion. >0 An error
occurred."* `c17` is the only one of the four that gets this right.
Items: #F1, #T1, #R3.

**2. `LC_COLLATE` is never consulted.** `ctags -x`, `cflow -r`, and the `cxref`
symbol listing are all sorted with Rust's byte-order `Ord` (via
`BTreeMap<String, _>`), never `strcoll`. `plib::locale::strcoll` already exists
and is used by `sort`, `join`, `comm`, and `nm`. Items: #T4, #F8, #R7.

**3. `-D`/`-I`/`-U` ordering.** `cflow` and `cxref` both carry an explicit
spec exception — *"the order of the −D, −I, and −U options ... is
significant"* (cflow.md 88920–88921, cxref.md 91587–91588) — which is a
deliberate departure from `c17`, where `-U` wins *regardless* of order
(c17.md 87878–87880). All three utilities share one code path that implements
the `c17` rule. So the same behavior **conforms for `c17`** and **diverges for
`cflow` and `cxref`**. Items: #F4, #R6 (and `c17`'s corresponding item
CONFORMS).

**4. `gettext()` — no longer a no-op (revised 2026-08-08).** The audit as
written said the vendored `gettext-rs` shim made `gettext()` an identity
passthrough, so wrapping a diagnostic changed nothing. **That is no longer
true**: `gettext-rs/src/catalog.rs:123-178` parses real GNU `.mo` catalogs and
resolves them through `NLSPATH` templates, `bindtextdomain`, `TEXTDOMAINDIR`,
and the system locale directories, honoring `LC_ALL`/`LC_MESSAGES`/`LANG`
precedence. Verified by probe: a hand-built catalog reached via `NLSPATH`
translates `cflow`, `ctags`, and `cxref` diagnostics.

So #T5, #F11, and #R10 are **closed** — those three utilities route their
diagnostics through `gettext()`. #U7 remains **open** for a different reason:
`cc/diag.rs` emits the compiler front end's diagnostics without wrapping them,
so front-end errors surface untranslated even in the three closed utilities.
`io::Error` text is likewise passed through raw everywhere.

---

# `c17`

**Implementation:** `cc/main.rs` (1488) — driver; the compiler proper is
`cc/token/` (7.6k), `cc/parse/` (14k), `cc/ir/` (23k), `cc/arch/` (32k),
plus `cc/include/`, `cc/os/`, `cc/abi/`, `cc/target.rs`, `cc/rtlib.rs`.
**Tests:** `cc/tests/` — 231 `#[test]`s, 18,459 lines.

## TL;DR

Single-translation-unit compilation is genuinely good: `-c`, `-o`, `-E`, `-g`,
`-I`, `-D`/`-U`, `-O` all behave to spec, the `""`-vs-`<>` header search order
is exactly right, `-U` correctly beats `-D` regardless of order, `a.out`
permissions are correct, and error exits are non-zero. But **multi-file
compilation is broken outright** — `c17 a.c b.c` and `c17 a.c b.o` both fail to
link, because each source operand is compiled *and linked in isolation*, so
cross-TU references are unresolvable and `.o`/`.a`/`.so` operands are dropped
entirely. That breaks two of the spec's six worked EXAMPLES. Four mandated
options (`-B`, `-G`, `-R`, `-s`) are absent, `-E` emits none of the mandated
`# <line> "<file>"` markers (so `c17 -E` cannot drive makefile dependency
generation, the RATIONALE's stated purpose), and `TMPDIR` is ignored in favor
of predictable `/tmp/c17_<pid>.*` paths.

On the language side, the compiler advertises `__STDC_VERSION__ == 201112L`
(C11), not C17's `201710L`, so **it cannot claim to be `c17` today**. Trigraphs
— still mandatory in C17, and explicitly called out in the POSIX RATIONALE —
are unimplemented. `_Generic` is unimplemented. Most seriously, `_Atomic`
objects accessed through ordinary operators (`x += 1`) compile to plain
non-atomic loads and stores while the `atomic_*()` API correctly emits `lock`
prefixes — a silent data race, not a missing feature. A cluster of C99/C11
constraint violations (implicit-int, duplicate `case` labels, `return`-type
mismatches, call arity mismatches) are accepted with no diagnostic at all,
which violates C17 5.1.1.3's requirement of a diagnostic for every constraint
violation.

## Priority issues

### Critical

- [x] **#U1 — Multi-translation-unit compilation is broken; `.o`/`.a`/`.so` operands are dropped.** **✓ fixed (Phase 1).** `main()` now classifies operands in one ordered pass (`Operand::classify`), compiles each source to an object via `process_file` — which no longer links — and performs exactly one `link_objects` call over every produced object plus every `.o`/`.a`/`.so` operand, in argument order. Objects with no `-o` now link to `a.out` instead of doing nothing. Tests: `cc/tests/driver/mod.rs` (`driver_links_multiple_sources`, `driver_links_source_with_object_operand`, `driver_object_operand_may_come_first`, `driver_links_objects_without_dash_o`). Original finding: `cc/main.rs:918-1057` (esp. 920-922, 1049-1054) and `596-648`. With any `.c`/`.i` operand present, the driver loops `for path in &source_files { process_file(...) }`, and each iteration independently assembles *that file alone* and links it *alone* to `exe_file`. `object_files` (line 922) is only consumed in the `source_files.is_empty()` branch. **[probed]** `c17 m1.c m2.c -o mA` → `undefined reference to 'helper'`, no output produced, exit 1. `c17 m1.c m2.o -o mB` → same. This breaks spec EXAMPLE 1 (`c17 foo.c bar.o`) and EXAMPLE 3 (`c17 -L /a/b/c main.o a.c -l Q b.c -l p`) outright; `c17 -o foobar foo.o bar.o` (objects only) does work. Fix: restructure `main()` into two phases — compile every source operand to a `.o`, then perform exactly one link that includes every produced object plus every `.o`/`.a`/`.so` operand, in argument order.
- [x] **#U2 — A compile error on one source operand aborts the run instead of continuing.** **✓ fixed (Phase 1).** The operand loop records a `failed` flag and continues; the link is gated on it and the process exits 1 at the end. `diag::reset_counts()` lost its `#[cfg(test)]` gate and is called between translation units, because the error state is a sticky process-global that would otherwise fail every file after the first. Tests: `driver_continues_past_a_failing_operand`, `driver_error_state_does_not_leak_between_operands`. Original finding: `cc/main.rs:1049-1054` calls `std::process::exit(1)` on the first `Err`. CONSEQUENCES OF ERRORS (88185-88187): *"it shall write a diagnostic ... and continue to compile other source code operands, but it shall not perform the link phase and it shall return a non-zero exit status."* **[probed]** `c17 -c bad.c good.c` produces **neither** `bad.o` nor `good.o`; `good.c` is never attempted. Fix: accumulate an error flag, `continue` past a failing file, gate the link on that flag, exit non-zero at the end.
- [x] **#P2 — The null directive `#` deletes the following line of source.** **✓ fixed (Phase 3).** `handle_directive` now peeks before consuming: if the next token carries `pos.newline` (or the stream is exhausted) this is the null directive of C17 6.10p7 and it returns. Tests: `preprocessor_null_directive_keeps_the_following_line`, `preprocessor_null_directive_between_real_directives`. Original finding: `cc/token/preprocess.rs:892-917`. `handle_directive` unconditionally does `iter.next()` to fetch the directive name without checking `pos.newline`, so a bare `#` (a valid no-op per C17 6.10p7) consumes the *first token of the next line*, misclassifies it as a directive name, and `skip_to_eol` eats the rest of that line. **[probed]** input `#\nint kept_one;\nint kept_two;` produces only `int kept_two;` plus a bogus `warning: unknown preprocessor directive #int`. Silent source deletion. Fix: if the fetched token has `pos.newline == true` (or `iter.next()` is `None`), treat as the null directive and return.
- [ ] **#X1 — `_Atomic` objects accessed via ordinary operators compile to non-atomic code.** `cc/ir/ssa.rs:148` is the only consumer of `is_atomic` anywhere in the IR or codegen (zero hits under `cc/arch/`), and its sole effect is to block SSA register promotion — the same treatment as `volatile`. **[probed]** on `atomic_int g`: `g += 1` emits `movl (%r11),%eax` / `movl %ecx,(%r11)` with **no `lock` prefix and no fence**, while `atomic_fetch_add(&g,1)` correctly emits `lock xaddl`. `g = 5` emits a plain `movl` (not a seq_cst store). This violates C11/C17 6.5.16.2p3 (atomic compound assignment is a single atomic RMW) and the basic data-race freedom guarantee of 6.7.2.4/6.7.3. Code that looks correct races silently. Fix: in the linearizer, detect an `_Atomic`-qualified lvalue in assignment/compound-assignment/`++`/`--` and emit `AtomicStore`/`AtomicFetchAdd`/`AtomicFetchSub`/CAS-loop IR (those opcodes already exist) with `memory_order_seq_cst`.
- [ ] **#P1/#X2 — `__STDC_VERSION__` is `201112L`; the compiler cannot claim C17.** `cc/token/preprocess.rs:549`, hardcoded, with no `-std=` gate (`-std=` is accepted and discarded at `cc/main.rs:710-712`). **[probed]** `int v = __STDC_VERSION__;` preprocesses to `201112L`. The `pcc`→`c17` rename (#U8) has since landed *without* this fix, so the binary now advertises a name it does not live up to: any source doing `#if __STDC_VERSION__ >= 201710L` takes the wrong branch. That makes this the highest-priority open item in the file. Fix: add `-std=c17|c11|c99|gnu*` threading a `CStd` enum into `init_predefined_macros`, defaulting to `201710L`.
- [x] **#L1 — Implicit-int is silently accepted for declarations and function definitions.** **✓ fixed (Phase 5).** `parse_type_specifier` records `saw_explicit_type`, which the two declaration entry points (`parse_external_decl`, `parse_declaration_and_bind_impl`) turn into a diagnostic at their own position. The flag rather than a check inside the specifier parser, because it has seven callers and some — an abstract parameter declarator, a K&R identifier list — legitimately reach it with no specifier. The predicate is subtler than the finding states: `signed`/`unsigned` name a type while setting only a modifier, so `base_kind.is_none()` alone would reject `unsigned x;`. Tests: `diagnostics_implicit_int_is_rejected`, `diagnostics_explicit_types_are_accepted` (eight legal spellings). Original finding: `cc/parse/parser.rs:1954` (`base_kind.unwrap_or(TypeKind::Int)`), reached unconditionally from `parse_external_decl` at `cc/parse/parser.rs:3031`. C99 removed implicit-int; 6.7.2p2 makes "at least one type specifier shall be given" a constraint requiring a diagnostic. **[probed]** `f(void){return 1;}` compiles clean with zero diagnostics. Fix: diagnose when `base_kind.is_none() && typedef_base.is_none()` before defaulting.

### Major

- [x] **#U3 — `-L`/`-l`/`-R` ordering relative to each other and to pathname operands is architecturally lost.** **✓ fixed (Phase 2).** New `cc/linkargs.rs` rescans the normalized argument vector into one ordered `Vec<LinkArg>`; `build_link_line` then substitutes each source operand's compiled object at its own position, so a library is searched where its name is encountered. If the rescan ever disagrees with what clap collected — the drift risk in its `VALUE_OPTIONS` table — it falls back to the old unordered shape rather than emitting a scrambled link line. Tests: `driver_library_order_is_significant` (two archives defining the same symbol; swapping `-lQ`/`-lP` changes the program's result) and `driver_library_named_before_its_user_does_not_resolve`. Original finding: `cc/main.rs:78` (`files: Vec<String>`), `199-203`, `624-636`, `1016-1031`: clap collects each flag into its own `Vec`, then the link line emits *all* `-L` then *all* `-l`, always after all objects. The spec deviates from XBD 12.2 specifically to say this order is significant (87866-87867), and EXAMPLE 3 depends on it. **[static]** — the source structure makes the interleaving unrecoverable after parsing. Fix: pre-scan raw argv for `-L`/`-l`/`-R`, preserving a single ordered stream mirrored onto the link command.
- [x] **#U4 — `-B mode`, `-G`, `-R directory`, and `-s` are unimplemented.** **✓ fixed (Phase 2).** All four added. `-G` shares the existing `--shared` path via `producing_shared()`; `-R` becomes `-Wl,-rpath,dir` in place; `-s` passes `-s` to the link; `-B dynamic|static` becomes `-Wl,-Bdynamic`/`-Wl,-Bstatic` ahead of the libraries it governs, and static binding is restored to dynamic before the host driver's implicit libc/libgcc_s, which are not shipped as archives. An unknown `-B` mode is diagnosed. Tests: `driver_accepts_mandated_options`, `driver_rejects_unknown_binding_mode`, `driver_dash_s_strips_symbols`. Original finding: 4 of the 14 mandated options. **[probed]** each is rejected by clap as `unexpected argument`. `--shared`/`-fPIC` exist as GCC-compat long flags but are not the POSIX short options, and there is no runpath emission (`-R`) or strip (`-s`) path at all. Fix: add all four; wire `-G` to the existing `--shared`/PIC path, `-R` to `-Wl,-rpath`, `-s` to a strip step, `-B dynamic` to library-selection preference.
- [x] **#U5/#P8 — `-E` emits none of the mandated `# <line> "<file>"` markers.** **✓ fixed (Phase 8).** The `-E` emitter opens with a marker naming the primary source and emits another on every stream transition, flagged `1` on entering a file and `2` on returning — GCC's convention, so existing consumers work. `include_file` strips the included stream's begin/end tokens, so transitions are detected from `pos.stream` rather than those markers. **Physical line numbers only**: `#line` sets state on the preprocessor and is never recorded in the stream registry, so `effective_position` cannot see it — the same pre-existing gap that keeps parser diagnostics on physical lines. Tests: `preprocessor_emits_line_markers_for_includes`, `preprocessor_emits_a_marker_without_includes`, `preprocessor_output_with_markers_still_compiles` (the `.i` must stay a valid operand). Original finding: `cc/main.rs:356-361`, `401-408` explicitly skip tokens whose text starts with `<STREAM`. STDOUT (88032-88038) says the `-E` output *shall* contain at least one such line for each file processed via `#include`; RATIONALE (88370-88374) states the purpose is makefile dependency generation. **[probed]** `c17 -E t.c | grep -c '^# [0-9]'` → `0`. Fix: emit `# {line} "{path}"` on stream transitions instead of discarding the markers.
- [x] **#U6 — `TMPDIR` is ignored; temp files use predictable names in a world-writable directory.** **✓ fixed (Phase 2).** One `tempfile::TempDir` per run holds every intermediate. `tempfile` places it under `TMPDIR` when set and creates it `O_EXCL` with a random name, which closes the symlink/pre-creation hazard; dropping it removes the tree, so the five best-effort `remove_file` calls that every `?` and `process::exit` used to bypass are gone. The one `process::exit` that outlives it drops it explicitly first. Test: `driver_honors_tmpdir_and_cleans_up`. Original finding: `cc/main.rs:567,600,941,947` hardcode `format!("/tmp/c17_{}.s", process::id())` etc. **[probed]** grep for `TMPDIR` across `cc/*.rs` returns zero matches; a run leaves `/tmp/c17_<pid>.o`. ENVIRONMENT VARIABLES (88020-88022) requires `TMPDIR` to override the temp directory. Separately, a predictable name in `/tmp` is a symlink/pre-creation hazard. Fix: honor `TMPDIR`, and create temp files with `O_EXCL` + randomized names (or use the `tempfile` crate already in dev-dependencies).
- [x] **#P6 — `#if` arithmetic is signed-`i64` only; large unsigned constants silently become 0.** **✓ fixed (Phase 3).** A new `PpValue { v: i128, unsigned: bool }` threads through all sixteen evaluator methods. Carrying both domains in an `i128` makes a `u64` representable exactly, so once `promote()` has applied the usual arithmetic conversions a plain signed comparison of the carrier *is* the correct unsigned comparison; results wrap back to 64 bits after each operation. `parse_number` parses into `u64` and marks a constant unsigned on a `u`/`U` suffix or when it exceeds `INTMAX_MAX` (C17 6.4.4.1p5). Tests: `preprocessor_if_handles_full_width_unsigned_constants`, `preprocessor_if_uint64_max_is_not_zero`, `preprocessor_if_signed_comparison_is_signed`, `preprocessor_if_unsigned_operand_promotes_the_comparison`. Original finding: `cc/token/preprocess.rs:3732-3744` (`i64::from_str_radix(...).unwrap_or(0)`). C17 6.10.1p4 requires `intmax_t`/`uintmax_t` arithmetic. **[probed]** `#if 0xFFFFFFFFFFFFFFFF` takes the *false* branch. Any `SIZE_MAX`/`UINTMAX_MAX`-style feature test silently misfires. Fix: parse into `u64`/`i128` and thread signedness through `ExprEvaluator`.
- [x] **#P9 — Builtin headers shadow the user's own headers for quoted includes.** **✓ fixed (Phase 3).** `find_include_file` now consults the bundled headers *after* the absolute-path check, the quote-form dir-of-file search and the `-I` list, so they stand in for the compiler's own include directory rather than pre-empting the whole search. `#include_next` still skips them. Tests: `preprocessor_local_header_wins_over_builtin`, `preprocessor_dash_i_wins_over_builtin`, and `preprocessor_builtin_headers_still_resolve` for the other direction. Original finding: `cc/token/preprocess.rs:1694-1762`: `get_builtin_header` is consulted at 1701-1709, *before* the quote-form same-directory lookup at 1721-1726. The `""` form must search the including file's directory first (87905-87910). **[probed]** a local `stddef.h` next to the source is ignored in favor of the builtin. Any project with its own `limits.h`/`float.h`/`stdbool.h`/`complex.h`/`iso646.h`/`stddef.h` is silently miscompiled. Fix: for quote-form includes, search dir-of-file and `-I` dirs before builtins.
- [ ] **#X3 — `_Generic` is unimplemented.** Zero matches in `cc/parse/expression.rs`, `ast.rs`, `parser.rs`. **[probed]** `_Generic(1, int: 1, default: 0)` → `error: undeclared identifier '_Generic'` then a cascade of parse errors. Blocks strictly-conforming C11/C17 source and any real `<tgmath.h>`. Fix: add as a primary-expression production, lvalue-convert the controlling expression's type per 6.5.1.1p2, match by type compatibility, enforce the "no two compatible associations / at most one default" constraints, and emit only the selected branch.
- [x] **#L2 — Duplicate `switch` case labels are not diagnosed.** **✓ fixed (Phase 5).** `collect_cases_from_stmt` rejects a repeated case value, a second `default`, and — newly noticed here — a case label that is not a constant expression, which could never match and was being dropped without a word. Tests: `diagnostics_duplicate_switch_labels_are_rejected`, `diagnostics_non_constant_case_label_is_rejected`, `diagnostics_distinct_switch_labels_are_accepted` (including Duff's device, where labels nest inside a loop). Original finding: `cc/ir/linearize_stmt.rs:1189-1220`, map built at `1142-1146`, no uniqueness check. C99 6.8.4.2p3 is a constraint. **[probed]** `switch(x){case 1: ...; case 1: ...}` compiles clean; which block wins is arbitrary. Fix: detect duplicates in `collect_switch_cases` and diagnose.
- [x] **#L3 — `return` is never checked against the function's return type.** **✓ fixed (Phase 5).** Both halves of 6.8.6.4p1 are checked in `linearize_stmt` against `current_func.return_type`. `Stmt` carries no position, so `return expr;` reports at the expression and a bare `return;` falls back to `current_pos`. Tests: `diagnostics_return_type_mismatch_is_rejected`, `diagnostics_matching_returns_are_accepted`. Original finding: `cc/ir/linearize_stmt.rs:70-113`. **[probed]** both `void f(void){ return 1; }` and `int f(void){ return; }` compile clean. C99 6.8.6.4p1 makes both constraint violations. Fix: add the two symmetric checks.
- [x] **#L6 — Call argument count is never validated against a visible prototype.** **✓ fixed (Phase 5)** at parse time in `check_call_arity`, not in the linearizer: the same `TypeId` is available at both, but the parser still has the call's position and each argument's, while by linearization `current_pos` points at whichever sub-expression was lowered last. Resolves through a function pointer, skips `params == None` (an unprototyped declaration, where no check is permitted, or an undeclared callee that already diagnosed). **One case is deliberately not checked**: `int f(void)` and `int f()` both intern as an empty parameter list, and telling them apart needs a no-prototype marker that the ABI, inliner and linearizer all read — too much reach for one diagnostic — so a zero-parameter prototype is skipped and `int f(void); f(1);` goes undiagnosed. Every prototype with at least one parameter is checked. Tests: `diagnostics_call_arity_mismatch_is_rejected`, `diagnostics_correct_calls_are_accepted` (six legal shapes). Original finding: `cc/parse/expression.rs:1442-1512` builds the `Call` node with no arity check; `cc/ir/linearize.rs:2325` bounds-checks `arg_idx < params.len()` (written knowing they can diverge) but never flags it. C99 6.5.2.2p2 constraint. **[probed]** both `g(1)` against `int g(int,int)` and `g(1,2,3)` against `int g(int)` compile clean. Fix: compare `args.len()` to `params.len()` (accounting for variadics) and diagnose.
- [x] **#L5 — `typedef` of a VLA type silently loses the size expression.** **✓ fixed (Phase 5)** by rejecting it, as the finding's second option suggests. The miscompile was worse than 'indistinguishable from `int a[]`': `linearize_local_decl` checks `STATIC` and `vla_sizes.is_empty()` but never `TYPEDEF`, so a block-scope `typedef int A[n];` was lowered as a *runtime VLA allocation*. File scope already rejected it. Tests: `diagnostics_typedef_of_vla_is_rejected`, `diagnostics_plain_vla_is_accepted`. Original finding: `cc/parse/parser.rs:1599-1611`: array declarators store non-constant dimensions as `array_size: None` plus a side-channel `vla_sizes: Vec<Expr>`, which every ordinary declarator path forwards but the `is_typedef` branch never inspects. The typedef becomes indistinguishable from an incomplete array `int a[]`. **[probed]** `typedef int arr_t[n]; arr_t x; x[0]=1;` compiles clean with no diagnostic. Fix: thread `vla_sizes` through the typedef symbol, or reject typedef'd VLAs with a diagnostic.
- [x] **#X4 — Incompatible `typedef` redefinition is silently accepted, keeping the first type.** **✓ fixed (Phase 5).** `check_typedef_redefinition` runs at all six `Symbol::typedef` binding sites and compares against the existing binding with `TypeTable::types_compatible`, which already existed (it backed `__builtin_types_compatible_p` and nothing else). Redefinition to a *compatible* type stays legal per C11 6.7p3, which two headers declaring the same alias rely on. Tests: `diagnostics_incompatible_typedef_redefinition_is_rejected`, `diagnostics_compatible_typedef_redefinition_is_accepted`. Original finding: `cc/parse/parser.rs:3651-3657` (pattern repeated at ~3156-3170, 3355, 3510, 3730): on `declare()` returning `Err`, the new declaration is discarded and the *existing* symbol reused, with no type-compatibility check. C11/C17 6.7p3 legalizes redefinition only for *compatible* types. **[probed]** `typedef int foo; typedef char foo; foo x;` compiles clean — strictly worse than C89, where any redefinition was flagged. Fix: compare against the existing type and diagnose a mismatch.
- [x] **#X5 — Unicode literal prefixes `u8""`, `u""`, `U""`, `u''`, `U''` are not lexed.** **✓ fixed (Phase 6), all five.** A `LiteralEncoding` replaces the lexer's `wide: bool`; `u8"..."` needs no new machinery because C11 6.4.5p6 gives it type `char[]` and the source is already UTF-8. `u`/`U` add `Utf16String`/`Utf32String`/`Utf16Char`/`Utf32Char` token types, `Utf16StringLit(Vec<u16>)`/`Utf32StringLit(Vec<u32>)` AST nodes, matching `Initializer` variants and module tables, and 2-byte/4-byte rodata emitters on both backends. The AST carries *code units*, not text, because the lexer keeps one `char` per source byte — so the parser decodes the UTF-8 first, and a code point outside the BMP becomes a surrogate pair for `char16_t`. `char16_t`/`char32_t` map to `unsigned short`/`unsigned int`; they are not declared as names because `<uchar.h>` is deliberately not bundled (see #H1). Tests: `cc/tests/c11/literals.rs` — sizes, values, non-ASCII decoding, static initializers — plus lexer unit tests including that `u8'x'` is *not* a character prefix. Original finding: `cc/token/lexer.rs:634-638` special-cases only `name == "L"`. **[probed]** `u8"x"` → `error: undeclared identifier 'u8'` followed by a confusing parse error, rather than a clear diagnostic. `<uchar.h>` is not bundled (the system one is picked up). Fix: recognize `u8`/`u`/`U` alongside `L`; either implement `char16_t`/`char32_t` semantics or reject with an explicit diagnostic.
- [x] **#X8 — No `__STDC_NO_*` macro is ever defined, including for genuinely absent features.** **✓ fixed (Phase 7).** `__STDC_NO_THREADS__` is defined when the host has no `<threads.h>`, probed per compilation rather than assumed — glibc gained it in 2.28 and macOS still lacks it, so the answer varies by host even for one target. `__STDC_UTF_16__`/`__STDC_UTF_32__`/`__STDC_IEC_559__`/`__STDC_ISO_10646__` are also defined now (see #P7). The finding is right that atomics, complex and VLA are supported and so must *not* be flagged. Original finding: **[probed]** `__STDC_NO_ATOMICS__`, `__STDC_NO_THREADS__`, `__STDC_NO_COMPLEX__`, `__STDC_NO_VLA__`, `__STDC_UTF_16__`, `__STDC_UTF_32__`, `__STDC_IEC_559__`, `__STDC_ISO_10646__` are all undefined. For atomics/complex/VLA that is correct (they are supported). But C17 requires `__STDC_NO_THREADS__` to be defined when `<threads.h>` is unavailable, and c17 bundles no `threads.h` — it relies entirely on the host libc, so on a host without one, portable code cannot feature-test and falls off a cliff. Fix: probe host `<threads.h>` per-target and define `__STDC_NO_THREADS__` when absent.
- [x] **#P7 — `__STDC_IEC_559__`, `__STDC_IEC_559_COMPLEX__`, `__STDC_ISO_10646__` are documented as implemented but do not exist.** **✓ fixed (Phase 7).** `__STDC_IEC_559__`, `__STDC_ISO_10646__`, `__STDC_UTF_16__` and `__STDC_UTF_32__` are now defined. **`__STDC_IEC_559_COMPLEX__` is deliberately left undefined**: it asserts Annex G conformance, and #C1/#C2 below show complex support does not meet that bar — defining it would be a false claim, which is the same failure mode as the checklist this finding is about. Tests: `c17_conditional_feature_macros`, `c17_does_not_claim_annex_g_complex`. Original finding: `cc/doc/c99-checklist.md:793-795` marks all three `[x]`; a project-wide grep finds zero hits outside that doc. **[probed]** all three undefined. Since c17's floats are native IEEE-754, `__STDC_IEC_559__` *should* legitimately be defined; its absence makes conforming numeric code take a needlessly conservative path. Fix: define them, or correct the checklist.
- [x] **#P3 — Incompatible macro redefinition is not diagnosed.** **✓ fixed (Phase 4).** `handle_define` compares against any existing definition via `macro_redefinition_conflict`, which checks kind, parameter count and spelling, variadic form, and the replacement list. Reported as a **warning**: 6.10.3p2 requires only a diagnostic, and rejecting outright would break a great deal of code that redefines a macro benignly. Two exemptions keep it honest rather than noisy — whitespace *before* the first replacement token is not a separation within the list, and a macro the implementation predefined was not defined by a `#define` directive, so the constraint does not reach it. Without either, every compilation against glibc warned: `features.h` redefines `__GLIBC__` (same value, different leading space) and `__GLIBC_MINOR__` (we hardcode 17, the host says 39). Tests: `preprocessor_incompatible_redefinition_is_diagnosed`, `preprocessor_identical_redefinition_is_silent`, `preprocessor_redefining_a_predefine_is_silent`, `preprocessor_including_a_system_header_is_warning_free`, plus unit tests `test_macro_redefinition_conflict_detection` and `test_replacement_lists_ignore_leading_whitespace`. The pinning test noted below was rewritten. Original finding: `cc/token/preprocess.rs:1104-1207` overwrites via `HashMap::insert` unconditionally; the existing test `test_macro_redefinition` (4439-4446) documents the silent-override behavior as intended. C17 6.10.3p2 is a constraint requiring a diagnostic unless replacement lists are identical. **[static]** Fix: compare old vs. new on redefinition.
- [x] **#P4 — Function-like macro argument-count mismatch is not diagnosed.** **✓ fixed (Phase 4).** `check_macro_arity` runs at the top of `expand_function_macro`, which both expansion paths funnel through. It recovers the distinction `collect_macro_args` cannot see — `F()` supplies *one empty argument*, not zero, unless the macro takes no parameters — and allows an empty variadic tail, which is a GNU extension C23 adopted. Tests: `preprocessor_macro_arity_mismatch_is_diagnosed`, `preprocessor_legal_macro_arities_are_accepted`. Original finding: `cc/token/preprocess.rs:2701-2763`, `2765-2924` use `args.get(idx).cloned().unwrap_or_default()` with no count check. C17 6.10.3p4 constraint. **[static]** Fix: validate arity at the `expand_function_macro` call sites.
- [x] **#P5 — `#`/`##` placement constraints are unchecked.** **✓ fixed (Phase 4).** `tokens_to_macro_body` rejects `##` at either end of a replacement list (6.10.3.3p1) and, in a function-like macro, a `#` not followed by a parameter (6.10.3.2p1). In an object-like macro `#` stays an ordinary token, which is correct and is pinned by a test. Tests: `preprocessor_paste_at_either_end_is_diagnosed`, `preprocessor_stringify_without_a_parameter_is_diagnosed`, `preprocessor_legal_hash_forms_are_accepted`. Original finding: `cc/token/preprocess.rs:1210-1330` never rejects `##` at the start/end of a replacement list (6.10.3.3p1) or a `#` not followed by a parameter in a function-like macro (6.10.3.2p1); both become literal tokens. **[static]** Fix: add both checks in `tokens_to_macro_body`.
- [x] **#X6 — `<stdatomic.h>` is missing most mandated typedefs and `atomic_is_lock_free`.** **✓ fixed (Phase 7).** All of C11 7.17.6.1 added, spelled against the compiler's own `__*_TYPE__` predefines so that including `<stdatomic.h>` alone suffices. `atomic_is_lock_free` follows from the object's size: both targets are lock-free up to pointer width and no wider. Test: `c11_stdatomic_provides_the_mandated_typedefs`. Original finding: `cc/include/stdatomic.h:25-37` defines only the fixed-width base types. Absent: `atomic_intptr_t`, `atomic_uintptr_t`, `atomic_size_t`, `atomic_ptrdiff_t`, `atomic_wchar_t`, `atomic_char16_t`, `atomic_char32_t`, all `atomic_{u,}int_{least,fast}N_t` (C11 7.17.6.1), and `atomic_is_lock_free()` (7.17.5.1). **[static]** `atomic_size_t` in particular is common in lock-free code. Fix: add the typedefs and the lock-free query.
- [x] **#X7 — `<complex.h>` is missing the `CMPLX`/`CMPLXF`/`CMPLXL` macros.** **✓ fixed (Phase 7)**, all three, via the existing `__builtin_complex`. Test `c11_cmplx_constructs_exactly` proves the point of the macro: `CMPLX(1.0, INFINITY)` keeps the real part at 1.0, while `1.0 + INFINITY*I` makes it NaN. **Only the `double` form is verified**, because writing the test surfaced two pre-existing codegen defects recorded as #C1/#C2 below — `CMPLXF` and `CMPLXL` are correct macros sitting on broken lowering. Original finding: `cc/include/complex.h` defines `I` via `__builtin_complex` but not the C11 7.3.9.5-7 constructors, which exist precisely so `x + y*I` (which can corrupt `NaN`/`Inf` operands) has an exact alternative. **[static]** `cc/doc/c99-checklist.md:645` claims them done; zero hits for `CMPLX` anywhere under `cc/tests`. Fix: define all three in terms of the existing `__builtin_complex`, and correct the checklist.
- [ ] **#H4 — aarch64 `long double` load/store collapses to 64-bit, contradicting its own 128-bit ABI.** **Test written, verdict deferred to CI (Phase 7).** `c17_long_double_round_trip` round-trips a value needing more than 53 bits of mantissa through a static, a function return and an array — the shapes that would expose a 64-bit slot. It passes on x86_64; aarch64 CI adjudicates the claim. Deliberately **not** "fixed" on the static reading, per the finding's own advice. Original finding: `cc/arch/aarch64/float.rs:24-33` maps `TypeKind::Double | TypeKind::LongDouble => FpSize::Double`, while the generic default at `cc/arch/lir.rs:114-115` uses `FpSize::Extended`. Everything around it is consistent with 128-bit: `cc/arch/aarch64/macros.rs:32` advertises `__LDBL_MANT_DIG__ = 113`, `cc/types.rs:1096-1101` sizes `long double` at 128 bits on aarch64/Linux, and `cc/arch/aarch64/mapping.rs:38-215` routes long-double arithmetic to libgcc's IEEE-quad `__addtf3`/`__multf3`/… family. If the load/store path feeding those calls truncates to 64 bits, `long double` is corrupted round-trip on aarch64/Linux. **[static — NOT VERIFIED; this audit ran on x86_64 and the claim is aarch64-only.]** Treat as a lead, not a confirmed defect: write a `long double` round-trip test and run it on aarch64 before scheduling work.

### Minor

- [x] **#P10 — A comment is not treated as whitespace for `#` stringification.** **✓ fixed (Phase 3).** Both comment-skipping paths in `get_special` set `self.whitespace`, so the following token records that phase 3 put a space there. Test: `preprocessor_comment_is_whitespace_for_stringification`, which also pins the real-whitespace control case. Original finding: `cc/token/lexer.rs:760-794` consume comments without setting `self.whitespace`. **[probed]** `#define S(x) #x` with `S(a/**/b)` yields `"ab"`; C17 phase 3 requires the comment to become one space, so it must be `"a b"`. (`S(c d)` correctly yields `"c d"`, confirming the mechanism works and only the comment path is missing.) Fix: set `whitespace = true` when skipping a comment.
- [x] **#P11 — Trigraphs are unimplemented (still mandatory in C17), and the docs misstate the standard.** **✓ fixed (Phase 6)**, behind `--trigraphs`, off by default. Implemented as a whole-buffer pre-pass (`replace_trigraphs`) rather than inside `nextchar`, because phase 1 precedes line splicing — so `??/` at end of line must be able to *become* a splice — and because `peekchar` is a separate non-mutating scanner that would have to mirror the rule exactly. Applied to included files too. Off by default because the replacement reaches inside string literals: `"What??!"` becomes `"What|"`, which a test pins in both directions. `cc/README.md` and `doc/c99-checklist.md` corrected — trigraphs went away in **C23**, not C11. Original finding: No `??` handling anywhere in `cc/token/lexer.rs`. **[probed]** `int main(void)??<return 0;??>` → parse error. Trigraphs were deprecated in C99 and removed only in **C23**; they remain mandatory through C17. POSIX's own RATIONALE (88224) notes that *"Some c17 compilers not conforming to POSIX.1-2024 do not support trigraphs by default"* — i.e. supporting them is the conforming behavior. Real-world impact is near zero, hence Minor. `cc/README.md:117` and `cc/doc/c99-checklist.md:505` both claim removal "in C11", which is wrong regardless of whether trigraphs get implemented. Fix: correct the docs; optionally implement phase 1.
- [x] **#L4 — `"a" L"b"` (mixed narrow/wide concatenation) is rejected.** **✓ fixed (Phase 6).** The two structurally identical loops, each of which could see only its own token type, are replaced by one `parse_string_literal_run` covering every encoding: a run takes whichever prefix appears (6.4.5p5), and a run mixing two *different* prefixes is diagnosed (6.4.5p2). _(The audit's aside that `chars().count()` undersizes a narrow `char[N]` is **incorrect** and was left alone: the lexer stores one `char` per source byte, so the count is the byte count by construction.)_ Tests: `c11_mixed_narrow_wide_concatenation`, `c11_mixed_narrow_utf_concatenation`, `c11_conflicting_prefix_concatenation_is_rejected`, `c11_same_encoding_concatenation_still_works`. Original finding: `cc/parse/expression.rs:2800-2863`: the narrow and wide handlers each only concatenate with their own kind. C11/C17 6.4.5p5 makes the mixed form well-defined (it was UB in C99), yielding a wide string. **[probed]** — this is *diagnosed*, not silently mis-merged, so it is a clean missing-feature gap rather than a correctness hazard; demoted from the agent's Major to Minor accordingly. Fix: merge the two loops so either kind starting the run promotes the whole concatenation to wide.
- [x] **#X9 — `_Atomic` is not rejected on array or function types.** **✓ fixed (Phase 5)** in the specifier form `_Atomic(T)`, which is where the constraint actually bites. The qualifier form needs no check: in `_Atomic int a[3]` the bit lands on the *element* type, making it an array of atomic ints — legal, and pinned by a test. Tests: `diagnostics_atomic_on_array_is_rejected`, `diagnostics_atomic_qualified_forms_are_accepted`. Original finding: No constraint check in `cc/types.rs` or `cc/parse/parser.rs`. 6.7.2.4/6.7.3 forbid it. **[static]** Fix: check where `TypeModifiers::ATOMIC` meets an array/function `TypeKind`.
- [x] **#P13 — `#line` argument errors are silently swallowed.** **✓ fixed (Phase 3).** `handle_line` takes the directive's position and diagnoses a missing operand, a non-decimal operand, one outside [1, 2147483647] (6.10.4p3), a filename that is not a string literal, and trailing tokens. Tests: `preprocessor_line_directive_rejects_bad_arguments`, `preprocessor_line_directive_accepts_valid_forms`. Original finding: `cc/token/preprocess.rs:2540-2576` returns without diagnosing an unparsable or out-of-range (>2147483647, 6.10.4p3) line number. **[static]**
- [x] **#P14 — `&&`/`||` in `#if` do not short-circuit.** **✓ fixed (Phase 3).** The un-taken operand is still parsed — it has to be, to find the end of the expression — but under a `suppressed` flag that withholds its diagnostics. That became load-bearing in the same phase: #P6 added a real division-by-zero diagnostic, so without short-circuiting `#if defined(X) && 1/X` would now report an error on a branch the program never asked to evaluate. Tests: `preprocessor_logical_and_short_circuits`, `preprocessor_logical_or_short_circuits`, and `preprocessor_division_by_zero_is_diagnosed_when_reached` for the converse. Original finding: `cc/token/preprocess.rs:3373-3391` always evaluate both operands. Currently harmless (division by zero is separately guarded at 3503-3527) but a latent trap. **[static]**
- [x] **#P15 — `-D` cannot define a function-like macro.** **✓ fixed (Phase 4).** `Preprocessor::define_from_cmdline` rewrites the spec as the equivalent `#define` directive and runs it through the ordinary directive path, so `-D'F(x)=x+1'` gets the same parameter parsing, `#`/`##` handling and variadic support as a `#define` in source — rather than a second implementation that would drift. Both the C and the assembly `-D` loops now share it, and the duplicated `Macro::from_cmdline_define` was deleted. Tests: `preprocessor_dash_d_defines_function_like_macros`, `preprocessor_dash_d_object_forms_still_work`. Original finding: `cc/token/preprocess.rs:191-232` always builds an object-like macro, so `-D'FOO(x)=x+1'` makes `"FOO(x)"` the macro *name* — a silent no-op. Not POSIX-mandated (the spec says only "name"), but a universal expectation. **[static]**
- [x] **#P16 — GNU named-variadic macros drop all but the first extra argument.** **✓ fixed (Phase 4).** The parameter loop tracks whether an identifier sits immediately before the `...`; if so it pops that identifier back out of `params` and records it as `variadic_name`, which `tokens_to_macro_body` then treats exactly like `__VA_ARGS__`. The formerly write-only `_is_variadic` became the load-bearing `is_variadic`. Tests: `preprocessor_named_variadic_binds_all_trailing_arguments` (runs the program, so it observes all three arguments arriving), `preprocessor_va_args_form_still_works`. Original finding: `cc/token/preprocess.rs:1152-1187` treats the identifier before `...` as an ordinary positional parameter. GNU extension, not C17-mandated. **[static]**
- [x] **#P12 — `_GNU_SOURCE`/`_XOPEN_SOURCE=700` are predefined unconditionally on Linux.** **✓ documented (Phase 9)**, which is all the finding asks — it explicitly notes this is not a violation. `cc/os/linux.rs` now records why the divergence is deliberate. (`_XOPEN_SOURCE` is `800` since Phase 7; see #H10.) Original finding: `cc/os/linux.rs:26-31`, before user `-D`/`-U`. POSIX only *encourages* restricting visibility here (88196-88203), so this is not a violation. **[static]**
- [ ] **#U7 — Runtime diagnostics are hardcoded English.** **Partly fixed (Phase 8).** The `warning:`/`error:` labels on every diagnostic, the include-chain note, and all of the driver's own operational messages now go through `gettext()`, which does real catalog lookup (cross-cutting note 4). **The message bodies do not**, and deliberately: they are built with `format!` at ~50 call sites, so the literal is not the msgid and wrapping it achieves nothing. Closing this properly means restructuring each site to carry a translatable template with positional substitution — a mechanical change worth doing on its own, not as a rider. Original finding: `cc/diag.rs:299-403` and every `eprintln!` in `cc/main.rs`. `setlocale` *is* called (`cc/main.rs:870`) and help strings *are* wrapped, so only the diagnostic strings are missing. Per the revised cross-cutting note 4, `gettext()` now performs real catalog lookup, so wrapping these strings would take effect immediately — this is straightforward work, not blocked.
- [x] **#U8 — The binary was named `pcc`, not `c17`.** `cc/Cargo.toml:31-33`. **✓ fixed** — the binary is `c17`. The rename also covered the hidden internal flags (`--pcc-*` → `--c17-*`), the diagnostic prefix (`c17: …`), `__VERSION__`, the `Generated by c17 …` assembly header, the DWARF `DW_AT_producer` string, the `_C17_LIMITS_H`/`_C17_FLOAT_H` builtin-header guards, the `/tmp/c17_<pid>.*` temp names, and `lib/utils.tsv`. No `pcc` compatibility alias was kept. The maintainer scoped this to a cosmetic rename, so the caveat noted here originally still stands: #P1/#X2 did **not** land with it, and the `c17` binary misreports its language level as `201112L` until it does.
- [x] **#U9 — `-` is accepted as a pathname operand though STDIN says "Not used."** **✓ documented (Phase 2)** — kept as a deliberate GCC-compatible extension, as the finding recommends; `linkargs::scan` treats a bare `-` as an operand rather than an option, with a test pinning that. Original finding: `cc/main.rs:321-323`, `840`. A harmless GCC-compatible extension; conforming applications never pass a bare `-`. Document rather than remove.
- [x] **#U10 — `-c -o out` with multiple inputs overwrites `out` once per input, silently.** **✓ fixed (Phase 1)** — still unspecified per the spec, but now warned rather than silent. Test: `driver_warns_on_dash_c_dash_o_with_multiple_sources`. Original finding: `cc/main.rs:575`. The spec leaves this unspecified (88338-88343) so it is not a defect; a one-line warning would help. Not required for conformance.
- [x] **#L8 — `cc/doc/c99-checklist.md` overclaims and should not be used as compliance evidence.** **✓ fixed (Phase 9).** The checklist opens with an accuracy note naming the failure mode, and the individual entries this finding cites are corrected: `return;`/`return expression;` and the 1023-case-label row now say the constraint is diagnosed, the mixed narrow/wide concatenation row says when it was implemented, and the two rows that were simply *false* are un-ticked — typedef-of-VLA (now rejected) and `__STDC_IEC_559_COMPLEX__` (deliberately undefined). Original finding: Every item in #L1–#L6 has a corresponding `[x]` (e.g. lines 353, 431, 564, 580, 856-858). The checklist treats "parses without panicking" as "conforms". Fix: correct the entries and add rows for implicit-int and duplicate-case detection, which the checklist does not track at all.
- [x] **#X10 — `cc/doc/c11-checklist.md` *under*-reports anonymous struct/union inside `union`.** **✓ fixed (Phase 9)** — the finding's reading was right, it already worked. Boxes ticked after adding the regression test the finding asked for: `c11_anonymous_members_inside_a_union`, which checks member promotion, storage overlay and `sizeof`. Original finding: Lines 138-139/199-200 mark these unimplemented, but `cc/parse/parser.rs:2061-2147` uses one shared member-parsing path whose anonymous-member branch (2135-2147) is gated only on `is_struct_or_union && is_special(b';')`, with no `is_union` restriction — so it already works. **[static]** Fix: add a regression test, then tick the boxes.
- [x] **#H1 — `<stdint.h>` and `<uchar.h>` are not bundled; the host's are used.** **✓ fixed for `stdint.h` (Phase 7)** — `cc/include/stdint.h`, the full C17 7.20 surface built on the `__INTn_TYPE__`/`__INTn_MAX__` predefines. `uchar.h` is **not** bundled, per the maintainer's scoping. Bundling it immediately exposed a real pre-existing bug it had been masking: `__INT64_TYPE__` said `long long int` on LP64 where the host says `long`, so the two headers disagreed about `int64_t`. The 64-bit type and constant-suffix predefines now follow `target.long_width`. Tests: `c17_bundled_stdint_provides_the_mandated_surface`, `c17_bundled_stdint_agrees_with_system_headers`. Original finding: **[probed]** all of `float.h iso646.h limits.h stdalign.h stdarg.h stdbool.h stddef.h stdnoreturn.h complex.h stdatomic.h` are bundled in `cc/include/` and compile; `stdint.h`, `uchar.h`, and `threads.h` resolve to the system copies. C17 4p6 puts `<stdint.h>` in the *freestanding* set, which the implementation must supply itself. It works today because `cc/arch/mod.rs:112-231` predefines the full GCC-compatible `__INTn_TYPE__`/`__INTn_MAX__` surface glibc's `<stdint.h>` expects — a functional but not freestanding-capable design that hard-depends on a full libc sysroot even for trivial programs. `cc/tests/c99/stdlib_headers.rs:12` documents the delegation explicitly. Fix: bundle `stdint.h` (and `uchar.h` if #X5 is addressed).
- [x] **#H10 — `_POSIX_C_SOURCE` defaults to `200809L` (POSIX.1-2008), not `202405L` (POSIX.1-2024).** **✓ fixed (Phase 7)** — `_POSIX_C_SOURCE` is `202405L` and `_XOPEN_SOURCE` is `800`. Verified against a broad system-header set (`unistd.h`, `sys/stat.h`, `fcntl.h`, `time.h`, `signal.h`, `pthread.h`) plus the full `c99_stdlib_headers_mega` suite. Original finding: `cc/os/mod.rs:24`. **[probed]** Every delegated system header gates POSIX.1-2024 prototypes behind this macro, so a `c17`-branded, POSIX.1-2024-targeting compiler exposes a 2008-era system interface by default. (`_XOPEN_SOURCE=700` at `cc/os/linux.rs:30` is likewise the 2008-era value.) Fix: bump the defaults; `-D` already overrides them.
- [x] **#H9 — `xmmintrin.h`/`emmintrin.h` are registered builtin headers that unconditionally `#error`.** **✓ fixed (Phase 7)** — both dropped from `get_builtin_header`, so the ordinary search can find a real one. The files themselves stay for reference. Original finding: `cc/include/xmmintrin.h:13`, `cc/include/emmintrin.h:13`, wired at `cc/builtin_headers.rs:49-53,72-73`. **[probed]** Not ISO/POSIX-mandated, so no conformance impact on its own — but combined with #P9 (builtins win over `-I`), registering them means a user who *does* have a real SSE intrinsics header on their include path gets c17's hard failure instead. Fix: drop them from `get_builtin_header()` so the normal search can find a real one.
- [ ] **#H8 — `FLT_ROUNDS` is a static `1`.** **Accepted divergence (Phase 7); the finding's premise is wrong.** It states that "glibc's own `<float.h>` calls `__flt_rounds()`" — glibc ships no `<float.h>` at all (GCC does) and exports no `__flt_rounds`; an implementation attempt failed to link against it. GCC's own `<float.h>` defines `FLT_ROUNDS` as a literal `1`, exactly as we do. Tracking the live mode would mean pulling `<fenv.h>` machinery into `<float.h>`, which no mainstream implementation does; code that needs it calls `fegetround()`. The header comment now records this. Original finding: `cc/include/float.h:69`. C17 5.2.4.2.2p8 requires it to reflect the current rounding mode; glibc's own `<float.h>` calls `__flt_rounds()`. c17 has no `<fenv.h>` coordination, so a runtime `fesetround()` is never reflected. **[probed]** Low impact; note only.
- [x] **#H12 — `-ffreestanding` is accepted and silently ignored; `__STDC_HOSTED__` is always `1`.** **✓ fixed (Phase 7)** by diagnosing it, the finding's first option: there is no freestanding environment to enter, so accepting the flag was the misleading part. `-fhosted` is accepted, being what we already are. Test: `c17_ffreestanding_is_diagnosed`. Original finding: The generic `-f*` catch-all at `cc/main.rs:767` swallows it, and `__STDC_HOSTED__` is hardcoded `"1"` in two places (`cc/token/preprocess.rs:550`, `cc/os/mod.rs:22`) with no path that flips it. **[probed]** Consistent with #H1 (there is no real freestanding mode), but silently accepting a flag that does nothing is misleading. Fix: diagnose it as unsupported, or implement it.
- [x] **#H11 — `cc/rtlib.rs` is not the runtime-helper source of truth its doc comment claims.** **✓ fixed (Phase 9)** — the doc comment now says what the file actually holds (the `_Float16` pairs) and names the three places the load-bearing helper names really live, so it cannot be mistaken for an inventory. Original finding: `cc/rtlib.rs:9-14` says it "maps C type operations to their corresponding runtime library function names", but it holds only the 12 `_Float16` conversion pairs. The load-bearing names are hardcoded at their call sites: `__divti3`/`__udivti3`/`__modti3`/`__umodti3` at `cc/arch/mapping.rs:1326-1329`, the whole `__*tf*` quad family at `cc/arch/aarch64/mapping.rs:38-215`, and bare `memcpy`/`memset` at `cc/arch/x86_64/features.rs:1236,1270`. **[static]** Not a correctness bug — all are stable libgcc/compiler-rt export names — but documentation drift worth fixing before more targets land.

### Found during remediation (not in the original audit)

- [ ] **#C1 — `float _Complex` silently loses its imaginary part.** **[probed]** `float _Complex f = 2.0f + 3.0f*I;` yields `(2, 0)`; through `__builtin_complex(2.0f, 3.0f)` it yields `(2, 0)` as well, and assigning a `double _Complex` to a `float _Complex` yields `(0, 0)`. `double _Complex` is correct throughout, so this is specific to the single-precision lowering. Found while testing #X7's `CMPLXF`; **pre-existing** — the failing expression uses no macro this effort added. This is why `__STDC_IEC_559_COMPLEX__` is left undefined.
- [ ] **#C2 — `long double _Complex` emits an invalid instruction.** **[probed]** `long double _Complex l = 2.0L + 3.0L*I;` produces `movt %xmm0,-240(%rbp)`, which the assembler rejects outright (`Error: no such instruction`). Also pre-existing, also found via #X7. Together with #C1 this means only `double _Complex` is usable.
- [ ] **#C3 — `__GLIBC_MINOR__` is hardcoded to `17`.** `cc/os/linux.rs:24` predefines the glibc minor version as a "conservative baseline" regardless of the host, which on this machine reports 2.17 against an actual 2.39. It is self-correcting in practice — `features.h` redefines both macros to the true values — but any code testing `__GLIBC_PREREQ` *before* including a system header sees the wrong answer. Noticed in Phase 4, when the redefinition diagnostic fired on every compilation against glibc.

### Not confirmed

- [ ] **#L7 — K&R old-style parameter default-argument promotion — CLAIM NOT REPRODUCED.** An agent reported that `cc/parse/parser.rs:3405-3448` stores old-style parameter types verbatim without widening `float`→`double` / `char`/`short`→`int` per C99 6.9.1p8, predicting a miscompile. **[probed]** `int takes_float(a) float a; { return (int)(a*2); }` called as `takes_float(3.5)` returns **7** under both `c17` and `gcc`. The predicted failure does not occur on this path. Recorded for completeness; needs a narrower repro before being treated as a real finding, and should not be scheduled as work on the strength of the static reading alone.

## Detailed conformance matrix

### SYNOPSIS

- [x] Options interspersed with operands — clap parses flags anywhere; `cc/main.rs:78`.
- [x] Grouping option letters "need not be recognized" — spec-permissive, N/A.
- [x] `--` end-of-options — **[probed]** `c17 -c -- t.c` works.
- [x] At least one pathname operand required — `cc/main.rs:78`.
- [x] `-L`/`-l`/`-R` order significant — #U3 closed; recovered by `cc/linkargs.rs` and emitted in argument order.

### OPTIONS

| Option | Status | Evidence |
|---|---|---|
| `-B mode` | CONFORMS | `-Wl,-Bstatic`/`-Bdynamic`; unknown mode diagnosed |
| `-c` | CONFORMS | `cc/main.rs:134-135,573-594` |
| `-D name[=value]` | CONFORMS | `cc/main.rs:116-117`; **[probed]** default value is `1` |
| `-D`/`-U` precedence (`-U` wins regardless of order) | CONFORMS | `cc/token/preprocess.rs:3800-3818`; **[probed]** both orders leave the name undefined — exactly as 87878-87880 requires |
| `-D` limits (≥2048 bytes, ≥256 names) | CONFORMS | unbounded `Vec` |
| `-E` | CONFORMS | line markers emitted on stream transitions — #U5 closed |
| `-G` | CONFORMS | shares the `--shared` path (`producing_shared`) |
| `-g` | CONFORMS | `cc/main.rs:147-148,578-579,602-603` |
| `-I directory` | CONFORMS | `cc/main.rs:122-123`; order preserved, unbounded (≥10 floor met) |
| `-I` search algorithm (`""` vs `<>`) | CONFORMS | matches 87905-87910; builtins now searched after the user's paths — #P9 closed |
| `-L directory` | CONFORMS | ordering recovered by `cc/linkargs.rs` |
| `-l library` | CONFORMS | `.so`-vs-`.a` selection delegated to the host linker (acceptable), now steerable with `-B`; interspersion preserved |
| `-O optlevel` | CONFORMS | `cc/main.rs:184-186`; `cc/opt.rs:72-75` disables all passes at `0`; omitted ⇒ `0` (spec: default unspecified) |
| `-o outfile` | CONFORMS | names the link output, or the single `-c` object; `-c` with several sources warns (#U10) |
| `-R directory` | CONFORMS | emitted as `-Wl,-rpath,dir` in operand order |
| `-s` | CONFORMS | passed to the link |
| `-U name` | CONFORMS | `cc/main.rs:119-120` |

### OPERANDS

- [x] `file.c`, `file.i` — `cc/main.rs:838-840`; **[probed]** `.i` from `c17 -E` recompiles.
- [x] `file.o`, `file.a`, `file.so` (incl. versioned `libfoo.so.N`) recognized — `cc/main.rs:849-856`.
- [x] Unrecognized suffixes warned and skipped — `cc/main.rs:925-929`; spec calls this implementation-defined.
- [x] Operands actually combined into one link — #U1 closed; one `link_objects` call covers every operand in argument order.

### STDIN / INPUT FILES / OUTPUT FILES

- [x] STDIN not used (bare `-` is an opt-in extension, #U9).
- [x] `-c` without `-o` produces `$(basename pathname .c).o` — `cc/main.rs:539-548,575`; **[probed]**.
- [x] `a.out` default — `cc/main.rs:597`; **[probed]**.
- [x] Executable permission bits `S_IRWXU|S_IRWXG|S_IRWXO` minus umask — **[probed]** `a.out` is `0775` under `umask 0002`. Delegated to the host linker, which satisfies the mandate.
- [x] `.o`/`.a`/`.so` operands linked — #U1 closed; object operands join the link inputs in place.

### ENVIRONMENT VARIABLES

| Var | Status | Notes |
|---|---|---|
| `LANG`, `LC_ALL`, `LC_CTYPE` | CONFORMS | `setlocale(LcAll,"")` at `cc/main.rs:870` forwards to libc |
| `LC_MESSAGES` | PARTIAL | labels and driver messages translated; diagnostic bodies still English — #U7 |
| `NLSPATH` (XSI) | PARTIAL | `gettext()` honors it (cross-cutting note 4), but `c17`'s own diagnostics are unwrapped — #U7 |
| `TMPDIR` (XSI) | CONFORMS | one `tempfile::TempDir` per run |

### ASYNCHRONOUS EVENTS

- [x] "Default" — no handlers installed, which is the conforming baseline. Temp files are not cleaned up if the process is signalled (see #U6's fix, which would also address this).

### STDOUT / STDERR

- [x] `-E` output to stdout — `cc/main.rs:387-446`.
- [x] Diagnostics to stderr only — `cc/diag.rs:383-402`; **[probed]**.
- [x] Warnings do not force a non-zero exit — `cc/diag.rs:409-412`.
- [x] The optional `"%s:\n"` per-file header is not emitted — spec says "may", so N/A.
- [x] `-E` `# <line> "<file>"` markers — #U5 closed.

### EXTENDED DESCRIPTION

- [x] Implicit `-l c` after all operands — delegated: the final link invokes the host `cc` (`cc/main.rs:614,1007`) without `-nostdlib`, so libc is always linked last. CONFORMS by delegation.

> **Architectural note (not a finding).** `c17` implements none of Phase 8 itself: it shells out to `as` for assembly (`cc/main.rs:601`) and to `cc` for linking (`614`, `1007`). There is no crt object selection, no dynamic-linker path resolution, and no explicit `-lc` or `-lgcc` anywhere in the crate. This is a legitimate choice — it satisfies the implicit-`-l c` and executable-permission mandates transitively, since every conforming host `cc` does those things — but it is an undocumented hard runtime dependency on `cc` and `as` being on `$PATH`, and it means c17 inherits the host driver's crt and runtime-library decisions rather than making its own. Worth stating in `cc/README.md`.

- [x] `-l m`, `-l pthread`, `-l rt`, `-l xnet`, `-l l`, `-l y` forwarded verbatim — `cc/main.rs:629-631,1025-1027`. The spec permits these to not exist as regular files; resolution is the host linker's job.
- [x] External symbol significance ≥31 bytes, ≥4095 identifiers per TU — no artificial caps in `cc/symbol.rs`/`cc/strings.rs`.
- [x] "A library shall be searched when its name is encountered" — #U3 closed; a library named before the object that references it now correctly fails to resolve.
- [x] Programming environments (`getconf _POSIX_V8_*`, `*_CFLAGS`/`*_LDFLAGS`/`*_LIBS`) — **N/A to `cc/`**: this is a `getconf`/system-configuration obligation (88105-88179), not something the compiler binary implements. Flagged here only so it is not lost; it belongs to whichever crate owns `getconf`.

### EXIT STATUS / CONSEQUENCES OF ERRORS

- [x] `0` on success, `>0` on error — **[probed]** compile error exits 1; the only `exit(0)` calls are legitimate info queries (`--print-targets` etc.).
- [x] Link failure diagnosed to stderr with non-zero exit — `cc/main.rs:610-611,640-641,1037-1039`; **[probed]**.
- [x] Continue compiling remaining operands after an error — #U2 closed.

### ISO C language conformance summary

| Area | Status |
|---|---|
| Translation phase 1 (trigraphs) | CONFORMS behind `--trigraphs`, off by default (#P11) |
| Translation phase 2 (line splicing) | CONFORMS — handled in `nextchar`/`peekchar` (`cc/token/lexer.rs:322-417`), so transparent to identifiers, literals, and comments alike |
| Translation phase 3 (comments) | CONFORMS — #P10 closed |
| Directives | CONFORMS |
| Computed `#include`, `_Pragma`, digraphs, `#include_next` | CONFORMS — **[probed]** computed include and `_Pragma` both work |
| Include cycle/depth guard | CONFORMS — **[probed]** a self-including header exits 1, no hang (`max_include_depth = 200`) |
| Macro expansion (blue paint, rescanning, `##` placemarkers) | CONFORMS |
| `#if` constant expressions | CONFORMS — #P6, #P14 closed |
| Predefined macros | PARTIAL — #P1, #P7, #X8 |
| C89 core | CONFORMS (declarators, bitfields, storage classes, tentative definitions, promotions) |
| C99 | CONFORMS for everything audited (VLAs, designated initializers, compound literals, flexible array members, `restrict`, `inline`, `_Bool`, `long long`, `_Complex`, `__func__`, UCNs, hex floats) |
| C11 | PARTIAL — `_Static_assert`, `_Alignas`/`_Alignof`, `_Noreturn`, `_Thread_local`, anonymous struct/union, and the explicit atomic API all work. Gaps: #X1, #X3, #X6, #X7 |
| C17 | NOT CLAIMED — #P1/#X2. C17 is DR-only; DR 412 (`_Static_assert` as a struct member) is honored at `cc/parse/parser.rs:2116-2120`. DR 423 is moot until `_Generic` exists |
| Translation limits (C99 5.2.4.1) | CONFORMS — `Vec`/`BTreeMap`-backed throughout; `cc/tests/c99/translation_limits.rs` exercises 130-member structs, 20 params, 30 cases. Residual risk: the recursive-descent parser has no explicit depth guard, so very deep nesting depends on stack size |
| Diagnostics for constraint violations (C17 5.1.1.3) | CONFORMS for everything audited — the pre-existing set plus implicit int, duplicate/non-constant `case`, multiple `default`, `return` type, call arity, typedef redefinition, `_Atomic` on an array, and the four preprocessor constraints. Residual: a zero-parameter prototype's call arity (see #L6) |

### Extensions accepted

Not findings — POSIX permits extensions, and none of these change the meaning
of strictly-conforming source: `__attribute__`, statement expressions,
`typeof`, the `__builtin_*` family, `__c11_atomic_*`, GCC extended inline asm
(incl. `asm goto`), `__int128`, `_Float16`/`_Float32`/`_Float64`, nullability
qualifiers, `#include_next`, `#warning`, digraphs, and the C23 one-argument
`_Static_assert`. See `cc/doc/ATTR.md` and `cc/doc/BUILTIN.md`.

## Test coverage signal

Not covered:
- [x] More than one `.c`/`.i` operand in a single invocation — `cc/tests/driver/mod.rs` now drives the binary with a raw argv via the new `common::run_c17` helper; `driver_multiple_sources_do_not_share_temp_files` also pins that per-source scratch names do not collide.
- [x] A `.c` operand combined with a `.o`/`.a`/`.so` operand (spec EXAMPLE 1) — `driver_links_source_with_object_operand`, plus `driver_object_operand_may_come_first` for the reverse order.
- [x] `-L`/`-l` interleaved with pathname operands (spec EXAMPLE 3) — `driver_library_order_is_significant` and `driver_library_named_before_its_user_does_not_resolve`, plus `linkargs`' own `spec_example_3_interleaving_is_preserved` unit test.
- [x] `-B`, `-G`, `-R`, `-s` — `driver_accepts_mandated_options`, `driver_rejects_unknown_binding_mode`, `driver_dash_s_strips_symbols`.
- [x] `-E` output containing `# <line> "<file>"` markers — `preprocessor_emits_line_markers_for_includes` and two companions.
- [x] `TMPDIR` honored for temp files — `driver_honors_tmpdir_and_cleans_up`, which also asserts nothing is left behind.
- [x] A compile error on a non-final operand followed by a later successful one — `driver_continues_past_a_failing_operand`.
- [x] Negative-path diagnostics generally — **closed (Phase 5)**. `cc/tests/diagnostics/` now exists, built on `compile_expect_error`/`compile_expect_ok` in `cc/tests/common/mod.rs`. It compiles with `-S`, which passes both error checkpoints (after parsing and after linearization), and greps stderr for the expected text; `--dump-ast` would return before linearization and miss anything the linearizer diagnoses, and there is no `-fsyntax-only`. Every constraint is tested in **both** directions, so a check cannot pass by rejecting everything. Covers #L1, #L2, #L3, #L5, #L6, #X4, #X9 plus two pre-existing checks. (#P3–#P5 are preprocessor constraints and live in `cc/tests/preprocessor/conformance.rs`.)
- [x] Trigraphs (#P11) — `c17_trigraphs_are_off_by_default`, `c17_trigraphs_work_when_enabled` (all nine sequences), `c17_trigraphs_affect_string_literals_only_when_enabled`, plus the `test_replace_trigraphs` unit test. _(The bare `#` null directive, macro redefinition and arity diagnostics, large `#if` hex constants, and comment-as-whitespace stringification are covered by `cc/tests/preprocessor/conformance.rs`, which asserts on `-E` **text** rather than a compiled program's exit code.)_
- [ ] `_Atomic` via plain operators — a naive test would *pass* today (#X1 only manifests under concurrency or assembly inspection), so the test must assert on generated assembly containing `lock`.
- [ ] `_Generic` — still uncovered. _(`CMPLX` is covered by `c11_cmplx_constructs_exactly`; Unicode literal prefixes by `cc/tests/c11/literals.rs`; incompatible typedef redefinition by `diagnostics_incompatible_typedef_redefinition_is_rejected`.)_
- [x] Header-search precedence between builtin headers and `-I`/dir-of-file (#P9) — `preprocessor_local_header_wins_over_builtin`, `preprocessor_dash_i_wins_over_builtin`, `preprocessor_builtin_headers_still_resolve`, plus `preprocessor_missing_include_is_still_an_error` so the shadowing tests cannot pass vacuously.
- [x] aarch64 `long double` round-trip (#H4) — `c17_long_double_round_trip`. Passes on x86_64; aarch64 CI settles the question.

Actively pinning current behavior (must change alongside the fix):
- [ ] `cc/tests/c11/core.rs:301` asserts `__STDC_VERSION__ == 201112L` (#P1/#X2).
- [x] `test_macro_redefinition` documented silent macro override as intended (#P3) — rewritten in Phase 4. It still pins that the later definition wins and that the diagnostic is non-fatal, but no longer asserts the silence as correct.

---

# `cflow`

**Implementation:** `cc/cflow.rs`
**Tests:** `cc/tests/tools/cflow.rs` (147 lines, 1 mega-test) + `cc/tests/cflow/test.c`
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/cflow.md`

## TL;DR

**All 12 findings are closed.** The forward flowgraph matches the spec's
worked EXAMPLE exactly, including indentation, the
`name: int(), <file line>` definition form, the `<>` undefined-reference form,
the `%d %s:%s` back-reference form and declarator-accurate line numbers.
Object files and `.l`/`.y` operands are supported, `-i x` reproduces the
spec's EXAMPLE output, `-D`/`-U` order is significant as this utility's spec
(unlike `c17`'s) requires, `-r` orders through `strcoll`, and every error path
returns non-zero.

*As first written* this section read: "the utility exits 0 no matter what
fails, `-i x` is entirely inert, the back-reference line uses an invented
`name  {N}` format, definition line numbers point at the type-specifier line,
and object files and `.l`/`.y` inputs are unsupported." Every clause is now
false; it is quoted here so the finding list below stays readable against what
it was written about.

## Priority issues

### Critical

- [x] **#F1 — Exit status is always 0.** `cc/cflow.rs:626-692`: `main()` unconditionally returns `ExitCode::SUCCESS`; per-file errors (649-658) and swallowed parse errors (488-491) are only `eprintln!`'d. **[probed]** a file with a compile error prints a diagnostic and exits 0. Fix: accumulate an error flag and return `ExitCode::FAILURE`. **✓ fixed** — routed through `plib::diag`; `main` exits via `has_errors()` plus the front-end's `has_error()`.
- [x] **#F2 — `-i x` has no effect; the spec's own EXAMPLE cannot be reproduced.** `cc/cflow.rs:87-88,98,634`: `CallGraph::_include_data` is written and never read; `extract_calls_from_expr`/`extract_calls_from_stmt` (157-351) only collect `ExprKind::Call` targets, so plain object references are never graph nodes. **[probed]** running the spec's EXAMPLE verbatim, `cflow -i x file.c` omits the mandated line `4 i: int, <file.c 1>` entirely. Fix: collect referenced file-scope object identifiers and splice them in when `include_data` is set. **✓ fixed** — file-scope objects collected as graph nodes; the spec EXAMPLE is now reproduced exactly and pinned by a test.

### Major

- [x] **#F3 — The back-reference line does not match the documented output format.** `cc/cflow.rs:549-552` prints `"{ref}{indent}{name}  {{{prev}}}"`. STDOUT mandates `"%d %s:%s\n"` and *"subsequent references to that name contain only the reference number of the line where the definition can be found."* **[probed]** a diamond call graph yields `5         shared  {3}` — no colon, and `{3}` braces inconsistent with the tool's own `<>` conventions. Recursion produces the same shape. The shipped mega-test cannot catch this: its only repeated call is de-duplicated at extraction time (line 168). Fix: emit the colon form. **✓ fixed** — repeated references print `name: <refnum>`.
- [x] **#F4 — `-D`/`-I`/`-U` relative order is not preserved.** `cc/cflow.rs:49-56` + `cc/token/preprocess.rs:3795-3818`. `cflow` explicitly overrides XBD 12.2 to make this order significant (cflow.md 88920-88921) — the *opposite* of `c17`'s rule — but it shares `c17`'s "all `-D`s then all `-U`s" code path. **[probed]** via the shared path: `-DFOO=1 -UFOO -DFOO=2` leaves `FOO` undefined; per cflow's spec it should be `2`. Fix: parse into one ordered list and replay in order. **✓ fixed** — `cc/ppargs.rs` rescans argv and replays `-D`/`-U` in order; `c17`'s own rule is unchanged.
- [x] **#F5 — Object-file input is unsupported.** `cc/cflow.rs:647-666` recognizes only `c|h|l|y|i` and stubs `s`; everything else, including `.o`, hits "unknown file type". DESCRIPTION and INPUT FILES require object files, and STDOUT even specifies their distinct definition format ("filename and location counter ... for example, `text`"). **[static]** Fix: read ELF/Mach-O symbol tables, or document the gap. **✓ fixed** — object files read via the `object` crate; call edges recovered from relocations; definitions use the `<file counter>` form. *Known limitation:* edges exist only where the object retains a relocation for the call. ELF emits one even for a same-object call (the symbol may be interposed), but the Mach-O arm64 assembler resolves an intra-section `bl` itself, so calls between two functions in one Mach-O object are not recovered. Calls to symbols defined elsewhere always carry a relocation and are recovered on every format. Closing that gap would require disassembly.
- [x] **#F6 — `.l`/`.y` operands are parsed as plain C.** `cc/cflow.rs:647-648,484-492` routes them through the identical C pipeline. OPERANDS requires they be "processed as appropriate" for lex/yacc. Real `.l`/`.y` source is not valid C, so the parse fails, the error is swallowed (488-491), and the file contributes nothing — silently. **[static]** Fix: pre-run `lex`/`yacc`, or refuse them explicitly like `.s`. **✓ fixed** — `.l`/`.y` operands are run through `lex`/`yacc` and the generated C is analyzed; diagnostics name the original operand.

### Minor

- [x] **#F7 — Non-positive `-d` is not ignored.** `cc/cflow.rs:42-43,536-541`. The spec requires *"Attempts to set the cut-off depth to a non-positive integer shall be ignored."* **[probed]** `-d 0` truncates output to the root line only, instead of behaving as if `-d` were absent; `-d -1` is a clap usage error. Fix: accept a signed value and fall back to unbounded for anything `<= 0`. **✓ fixed** — `-d` is signed and any value <= 0 is ignored.
- [x] **#F12 — Definition line numbers point at the type-specifier line, not the declarator line.** `cc/cflow.rs:557-560`. **[probed]** for the spec's EXAMPLE source, c17 reports `main: int(), <file.c 5>` and `f: int(), <file.c 12>` where the spec's expected output is `<file.c 6>` and `<file.c 13>` — the lines holding `main(void)` and `f()`, not the preceding bare `int`. Off-by-one whenever the return type is on its own line (a common style, and the style used by the spec's own example). Fix: attribute the definition to the declarator's position. **✓ fixed** — declarator line resolved by scanning forward from the declaration start.
- [x] **#F8 — `-r` ordering ignores `LC_COLLATE`.** `cc/cflow.rs:135,147,592` use byte-order `.sort()`. ENVIRONMENT VARIABLES names `LC_COLLATE` specifically for `-r` ordering. **[static]** Fix: sort via `plib::locale::strcoll`. **✓ fixed** — `-r` ordering via `plib::locale::strcoll`.
- [x] **#F9 — The `-i` option-argument is not validated.** `cc/cflow.rs:46-47,634-635` silently accept anything other than `x`/`_`. **[static]** **✓ fixed** — `-i` rejects anything but `x`/`_`.
- [x] **#F10 — `.i` operands are preprocessed a second time.** `cc/cflow.rs:647-648,464-478`. Inherited from `c17`'s OPERANDS: the processing already done by `-E` *"shall not be repeated."* Usually idempotent. **[static]** **✓ fixed** — `.i` operands bypass the preprocessor.
- [x] **#F11 — `NLSPATH`/`LC_MESSAGES` are no-ops.** See cross-cutting note 4. **✓ fixed** — runtime diagnostics now go through `gettext()`; the `gettext` stub itself remains a workspace-wide condition.

## Detailed conformance matrix

### SYNOPSIS / OPTIONS

| Option | Spec requirement | Status |
|---|---|---|
| `-d num` | cutoff depth; default very large; non-positive ignored | PARTIAL — #F7 |
| `-i incl` | `x` = include data/static symbols; `_` = include underscore names | PARTIAL — `_` works (`cc/cflow.rs:103-108`); `x` inert (#F2); values unvalidated (#F9) |
| `-r` | invert caller:callee, sort lexicographically | PARTIAL — implemented (589-620); collation (#F8); reverse-mode indentation hardcoded to 5 spaces (614) vs 4/level elsewhere |
| `-D name[=def]` | as `c17` | PARTIAL — value defaults to 1 correctly; ordering (#F4) |
| `-I directory` | as `c17` | CONFORMS |
| `-U name` | as `c17` | PARTIAL — ordering (#F4) |
| `--`, interspersed operands | XBD 12.2 | CONFORMS (clap) |

### OPERANDS / INPUT FILES

- [x] `file.c` → C source. CONFORMS.
- [x] `.h` also accepted — harmless extension.
- [x] `file.i` → **not** re-preprocessed — #F10 closed; `cc/cflow.rs:934-955` sets `already_preprocessed` and sends a `.i` operand straight to the parser.
- [x] `file.y`, `file.l` → run through `yacc`/`lex`, generated C analyzed — #F6 closed; `cc/cflow.rs:692-739,1208-1225`. Diagnostics name the operand, not the temp file.
- [x] `file.s` → explicitly refused (`cc/cflow.rs:1226-1232`). The spec permits "more limited" processing, so refusal is defensible; it now also exits non-zero, closing this row's #F1 half.
- [x] Object files → supported — #F5 closed; `cc/cflow.rs:768-895` reads symbols from ELF/Mach-O and call edges from relocations.

### STDIN / STDERR / OUTPUT FILES

- [x] STDIN not used. CONFORMS.
- [x] stderr used only for diagnostics. CONFORMS.
- [x] OUTPUT FILES "None". CONFORMS.

### ENVIRONMENT VARIABLES

- [x] `LANG`, `LC_ALL` — via `setlocale` (`cc/cflow.rs:627`). CONFORMS.
- [x] `LC_COLLATE` — #F8 closed; `-r` orders via `plib::locale::strcoll` (`cc/cflow.rs:196,1127`).
- [x] `LC_MESSAGES`, `NLSPATH` — #F11 closed; diagnostics resolve through `gettext()`, which now loads real `.mo` catalogs honoring `NLSPATH` templates and `LC_ALL`/`LC_MESSAGES`/`LANG` precedence (`gettext-rs/src/catalog.rs:123-178`). Residual: `io::Error` text is still emitted untranslated, and front-end diagnostics from `cc/diag.rs` remain English — #U7.

### STDOUT

- [x] First-definition line `"%d %s:%s\n"` with `type(), <file line>` — **[probed]** matches the spec EXAMPLE's shape exactly.
- [x] Indentation ≥1 column per level — 4 spaces/level (`cc/cflow.rs:543`), matches the EXAMPLE.
- [x] Undefined reference `<>` — **[probed]** `3         h: <>` matches the EXAMPLE. (Note: an identifier that is never *declared* is a hard compile error and is dropped rather than shown as `<>`; the spec's example declares `int h();`, so this is correct behavior for the specified case.)
- [x] Line-number attribution — #F12 closed; `declarator_line` (`cc/cflow.rs:565-585`) reports the declarator's line.
- [x] Back-reference form — #F3 closed; a repeat prints `name: <refnum>` in the same `%d %s:%s` shape (`cc/cflow.rs:1085-1088`), with no invented braces.
- [x] Data-symbol definition form (`i: int, <file.c 1>`) — #F2 closed; reproduced exactly under `-i x`.
- [x] Object-file definition form (`<file.o text>`) — #F5 closed; `cc/cflow.rs:887`, `location_counter` at 751-760.

### EXIT STATUS / CONSEQUENCES OF ERRORS

- [x] `>0` on error — CONFORMS; #F1 closed by `exit_code()` (`cc/cflow.rs:1276-1282`), which folds in both this utility's and the front end's error counters.

## Test coverage signal

Covered by `cc/tests/tools/posix.rs` since the audit, except where noted:
- [x] Back-reference/diamond call graphs (#F3) — `cc/tests/tools/posix.rs:525`, on a purpose-built diamond fixture.
- [x] `-i x` (#F2), `-i` validation (#F9) — `cc/tests/tools/posix.rs:468,510,579`.
- [x] `-D`/`-U` interleaving (#F4) — `cc/tests/tools/posix.rs:628`, five orderings.
- [x] A `.y` operand (#F6) — `cflow_processes_yacc_input`, which also checks that diagnostics name the operand rather than the generated file. Skips on a host with no yacc/bison.
- [x] Exit status on any failure (#F1) — `cc/tests/tools/posix.rs:72` (`tools_exit_status_mega`).
- [x] `-d 0` / negative depth (#F7) — `cc/tests/tools/posix.rs:555`.
- [x] Exact line-number attribution (#F12) — `cc/tests/tools/posix.rs:498-505` now asserts the spec EXAMPLE's full output, including `<file.c 6>` / `<file.c 13>`.
- [x] `LC_COLLATE` ordering under `-r` (#F8) — `tools_collation_actually_depends_on_the_locale` compares two locales for both `cflow -r` and `ctags -x`, so it fails if collation is not consulted. The older single-locale tests are kept as format checks; they simply cannot settle this on their own.

---

# `ctags`

**Implementation:** `cc/ctags.rs`
**Tests:** `cc/tests/tools/ctags.rs` (147 lines, 1 mega-test) + `cc/tests/ctags/test.c`
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/ctags.md`

> **Two premises in the audit brief were wrong and are corrected here.**
> (1) POSIX.1-2024 `ctags` has exactly three options — `-a`, `-f`, `-x`
> (ctags.md 91224-91228). `-u`, `-v`, and `-w` are **not** in the standard;
> Austin Group Defect 1330 removed the obsolescent interfaces. Their absence
> from the implementation is conforming, not a gap.
> (2) Fortran support is **not** mandated. `.f` appears only in RATIONALE
> (91356) as something historical implementations understood. The
> implementation's explicit "FORTRAN files not supported" message is a
> legitimate implementation-defined choice.

## TL;DR

The tags-file format is right where it counts: three tab-separated fields, the
`/^...$/` anchored pattern, correct `\/` and `\\` escaping in the correct
order, the `M`-prefix rule for `main`, and correct omission of the two
alternate formats the spec forbids. Two defects undercut it: any tag name that
appears twice is silently discarded down to one entry (real data loss across
files), and the utility exits 0 on every input error while printing a
diagnostic. The `-x` listing also diverges from the mandated
`"%s %d %s %s"` format by using padded columns and trimming the source line's
leading indentation.

## Priority issues

### Critical

- [x] **#T1 — Exit status stays 0 on every input error.** `cc/ctags.rs:296-313,342-349`. Unreadable files, swallowed parse errors (144-148), and unsupported extensions (306-312) all print to stderr without setting an error flag; only a tags-file write failure returns non-zero. **[probed]** `ctags nosuch.c` → `No such file or directory` then exit 0; `ctags z.pas` → `unknown file type` then exit 0. Fix: track `had_error` and return 1. **✓ fixed** — same mechanism as #F1.
- [x] **#T2 — Tags are silently lost when a name repeats.** `cc/ctags.rs:285,299,318,334`: `all_tags: BTreeMap<String, TagEntry>` is keyed by name alone, so `insert` overwrites. **[probed]** two files each defining `init` produce a tags file containing only `init  f2.c` — the `f1.c` definition vanishes with no diagnostic. The spec's "implementation-defined" latitude for duplicates covers *how* they are presented, not silently discarding valid distinct tags. Fix: key by `(name, file, line)` or store `Vec<TagEntry>` per name. **✓ fixed** — map keyed by `(name, file, line)`; still name-major, so tags-file order is unchanged.

### Major

- [x] **#T3 — `-x` output diverges from the mandated format.** `cc/ctags.rs:79-87` uses `format!("{:<16} {:>6} {:<20} {}", ...)` and `.trim()`s the line text. STDOUT (91264-91265) mandates `"%s %d %s %s"` where `<text>` is *"the text of line `<line-number>` of file `<filename>`"*. **[probed]** output is column-padded, and a function written as `    int indented_fn(...)` loses its leading indentation. Both break naive field-splitting and misreport the line's text. Fix: emit single-space-separated fields and do not trim. **✓ fixed** — single-space fields, source line reproduced verbatim.
- [x] **#T4 — `-x` sort order ignores `LC_COLLATE`.** `cc/ctags.rs:285,316-320`. The spec has an asymmetry worth preserving: the *tags file* must be sorted in the **POSIX locale's** collating sequence (so byte order is correct there, and the current `BTreeMap` happens to satisfy it), while **`-x` output** must honor `LC_COLLATE`. One map serves both. **[static]** Fix: sort `-x` separately via `plib::locale::strcoll`. **✓ fixed** — `-x` sorts via `plib::locale::strcoll`; the tags file deliberately keeps byte order (POSIX locale).

### Minor

- [x] **#T6 — Multi-line `typedef`s are missed.** `cc/ctags.rs:211-248`: `is_typedef_line` requires the `typedef` keyword and the type name on the same physical line, so the ubiquitous `typedef struct { ... } Foo;` is never tagged. **[static]** Fix: track brace depth, or use the symbol table. **✓ fixed** — `find_typedef_line` now walks the whole declaration to its closing `;` and reports the declarator's line.
- [x] **#T7 — The two SYNOPSIS forms are not mutually exclusive.** `cc/ctags.rs:34-51,316-346`. Per XBD 12.2 guideline 8, separate synopsis lines signal exclusivity; `-x` silently ignores `-a`/`-f`. **[static]** Fix: add a clap conflict group. **✓ fixed** — `-x` now `conflicts_with` `-a`/`-f`.
- [x] **#T8 — The tags file is written non-atomically.** `cc/ctags.rs:329` (`File::create`) truncates in place, so an interrupted write leaves a corrupt tags file for concurrent `vi`/`ex` readers. Not spec-mandated, but `plib::io::write_atomic` already exists and was adopted crate-wide in `dev/` for exactly this. **[static]** **✓ fixed** — fresh tags files go through `plib::io::write_atomic`; append is unchanged (cannot be atomic).
- [x] **#T9 — Input is UTF-8-only.** `cc/ctags.rs:108` (`fs::read_to_string`) hard-errors on non-UTF-8 source regardless of `LC_CTYPE`; combined with #T1 that becomes an unreported failure. **[static]** **✓ fixed** — decoded with `from_utf8_lossy`.
- [x] **#T5 — Runtime diagnostics bypass `gettext`.** See cross-cutting note 4. **✓ fixed** — as #F11.

## Detailed conformance matrix

### SYNOPSIS / OPTIONS

| Item | Status | Notes |
|---|---|---|
| `ctags [-a] [-f tagsfile] pathname...` | CONFORMS | `cc/ctags.rs:38-50` |
| `ctags -x pathname...` | CONFORMS (structurally) | `cc/ctags.rs:46-47` |
| Mutual exclusivity of the two forms | MISSING | #T7 |
| `-a` append | CONFORMS | `OpenOptions::append`, `cc/ctags.rs:323-327` |
| `-f tagsfile` (default `tags`) | CONFORMS | `cc/ctags.rs:42` |
| `-x` | PARTIAL | right fields, wrong format — #T3 |
| `-u`, `-v`, `-w` | N/A — correctly absent | Removed from the standard by Defect 1330 |
| `--`, option grouping | CONFORMS (clap) | |

### OPERANDS / INPUT FILES / STDIN

- [x] `.c` and `.h` treated as C source — `cc/ctags.rs:296`.
- [x] Other suffixes are implementation-defined; `.f` and unknown extensions are explicitly refused with a message (306-312) — a valid choice, though see #T1 for the exit code.
- [x] STDIN not used — `files` is required.

### ENVIRONMENT VARIABLES

- [x] `LANG`, `LC_ALL` — `setlocale` at `cc/ctags.rs:278`.
- [x] `LC_COLLATE` for the **tags file** (POSIX-locale order) — satisfied by byte-order `BTreeMap`.
- [x] `LC_COLLATE` for **`-x`** — #T4 closed; `-x` sorts via `plib::locale::strcoll` (`cc/ctags.rs:378-390`), while the tags file correctly stays in byte order.
- [x] `LC_CTYPE` — **✓ fixed (Phase 9).** `ctags` reads its source one `char` per byte instead of through `String::from_utf8_lossy`, and writes the tags file as bytes. The lossy decode replaced every invalid byte with U+FFFD *inside the emitted search pattern*, so the pattern no longer matched the line it pointed at and the editor landed nowhere. Byte-exact reproduction is what `LC_CTYPE` would otherwise have to select. Test: `ctags_pattern_preserves_source_bytes`.
- [x] `LC_MESSAGES`, `NLSPATH` — #T5 closed; diagnostics go through `gettext()` (`cc/ctags.rs:332,363-374`) and the vendored `gettext-rs` now loads real `.mo` catalogs via `NLSPATH`/`TEXTDOMAINDIR` (`gettext-rs/src/catalog.rs:123-176`).

### OUTPUT FILES — the tags file format

| Rule | Status |
|---|---|
| `"%s\t%s\t/%s/\n", identifier, filename, pattern` | CONFORMS — `cc/ctags.rs:75` |
| Optional `^`/`$` anchors | CONFORMS — always emitted, which also correctly disambiguates literal leading `^`/trailing `$` in source text per RATIONALE 91352-91355 |
| Escape `/` and `\` | CONFORMS — `cc/ctags.rs:74`, backslash replaced first, avoiding double-escaping |
| `?...?` alternate form | CONFORMS by omission — the spec *forbids* producing it (91289) |
| Line-number alternate form | CONFORMS by omission — same rule |
| Sorted by tagname in the POSIX collating sequence | CONFORMS |
| No silent loss of distinct tags | **DIVERGES** — #T2 |

### EXTENDED DESCRIPTION — objects tagged

| Object | Requirement | Status |
|---|---|---|
| Function definitions | shall attempt | CONFORMS — `cc/ctags.rs:153-175`, via the AST so line info is accurate |
| Type definitions | shall attempt | PARTIAL — #T6 |
| Macros with arguments | shall attempt | CONFORMS — `cc/ctags.rs:250-271` |
| `main` → `M` + file stem, path and suffix stripped | shall | CONFORMS — `cc/ctags.rs:159-167` |
| `#if`/`#ifdef` produce no output | shall | CONFORMS (with the spec-acknowledged caveat at APPLICATION USAGE that a `#define` inside `#if 0` is still scanned, since `extract_macro_tags` runs on raw text) |
| Prototypes, structs/unions/enums, globals, object-like macros, `#line` | may | Not implemented — all optional |

### EXIT STATUS

- [x] `0` on success — `cc/ctags.rs:348`.
- [x] `>0` on error — #T1 closed; `exit_code()` at `cc/ctags.rs:444,449-455`.

## Test coverage signal

Covered by `cc/tests/tools/posix.rs` since the audit, except where noted:
- [x] Duplicate tag names across files (#T2) — `cc/tests/tools/posix.rs:120`.
- [x] Exit status on unreadable / unsupported / parse-failing input (#T1) — `cc/tests/tools/posix.rs:72`. ctags' *own* parse-error branch (`cc/ctags.rs:162-165`) is still reached only indirectly.
- [x] Exact `-x` format, incl. padding and preserved indentation (#T3) — `cc/tests/tools/posix.rs:144` asserts the whole line, not `.contains()`.
- [x] `LC_COLLATE` for `-x` (#T4) — `tools_collation_actually_depends_on_the_locale`.
- [x] Default `-f tags` filename — `ctags_default_output_file_is_tags`, run in a scratch cwd so the default lands somewhere observable.
- [x] Escaping of `/` and `\` inside a pattern — `ctags_escapes_slash_in_patterns`.
- [x] Multi-line `typedef struct {...} Name;` (#T6) — `cc/tests/tools/posix.rs:181`.
- [x] `-x` combined with `-a`/`-f` (#T7) — `cc/tests/tools/posix.rs:214`.
- [x] Non-UTF-8 input (#T9) — `cc/tests/tools/posix.rs:234`. Asserts the tag survives, not byte-fidelity of the pattern (see the `LC_CTYPE` row above).
- [x] Sort order across an `-a` append boundary — **✓ fixed (Phase 9).** This was the live behavioral question the row raised, not just a missing test: `-a` appended a separately-sorted block, leaving the file a run of sorted runs, against ctags.md 91292 ("the file shall be sorted by identifier") and defeating the binary search vi/ex does over it. `-a` now merges with the existing file, sorts, de-duplicates and rewrites atomically. Test: `ctags_append_keeps_the_whole_file_sorted`, which also covers a first `-a` with no existing file and a repeated append.

---

# `cxref`

**Implementation:** `cc/cxref.rs`
**Tests:** `cc/tests/tools/cxref.rs` + the shared `cc/tests/tools/posix.rs` conformance suite + `cc/tests/cxref/test.c`
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/cxref.md`

## TL;DR

**All 11 findings are closed.** The listing has the right shape *and* the
right data: `#define` symbols are cross-referenced, defining line numbers are
real, containing-function scoping is correct, the per-file name line is
emitted so `-c` genuinely differs from the default, `-w` bounds every line,
`-D`/`-U` order is significant, the symbol list orders through `strcoll`, any
C source pathname is accepted, and every error path returns non-zero.

*As first written* this section read: "the data is substantially wrong:
`#define` symbols are never cross-referenced at all, the defining line number
is `0` for any uninitialized declaration, uninitialized locals vanish
entirely, the containing-function column reports the enclosing block scope
instead of global scope, and the mandated per-file name line is never emitted
— which also makes `-c` a no-op. As with its siblings, every error path
returns 0." Every clause is now false; it is quoted so the finding list below
stays readable against what it was written about.

## Priority issues

### Critical

- [x] **#R1 — `#define` symbols are never cross-referenced.** `cc/cxref.rs:348-443`. Preprocessing (378-390) fully expands macros before parsing, and nothing captures macro definitions beforehand, so they cannot appear in the listing. **[probed]** a file containing `#define MAXV 10` used later produces no `MAXV` row at all. Fix: capture macro name+line at `#define` time and feed them into `CrossRef`. **✓ fixed** — `#define` names and their uses recovered from the raw source before preprocessing.
- [x] **#R2 — Defining line numbers are `0` for uninitialized declarations; uninitialized locals vanish.** `cc/cxref.rs:432` (`.unwrap_or(0)` for file-scope declarations lacking an initializer) and `269-273` (block-scope declarations without an initializer are skipped outright). The root cause is that `InitDeclarator`/`Declaration` in `cc/parse/ast.rs:949-966` carry no position, so the line is only recoverable from the initializer expression. **[probed]** for `int g; int fn(void); int fn(void){ int loc; ... }`, both `g` and `fn` report `*0`, and `loc` never appears. Fix: add a position field to the declarator and thread it through independent of the initializer. **✓ fixed** — `InitDeclarator` now carries `pos`; also fixed a dropped C99 6.7.8 VLA diagnostic in the front end.
- [x] **#R3 — Exit status is always 0.** `cc/cxref.rs:529-595`: parse errors (398-404), unreadable files (573-575), and rejected operands (583-586) are `eprintln!`-only; only an `-o` create failure returns 1. **[probed]** a file with a compile error prints a diagnostic and exits 0. Fix: track an error flag across the loop. **✓ fixed** — same mechanism as #F1.

### Major

- [x] **#R4 — The mandated per-file name line is missing, which also makes `-c` a no-op.** `cc/cxref.rs:450-523` never emits a standalone filename line; the filename is only a per-row column. STDOUT (91628-91629): *"If the −c option is not specified, each portion of the listing shall start with the name of the input file on a separate line."* **[probed]** `cxref p.c q.c` and `cxref -c p.c q.c` produce **byte-identical** output — the tool always behaves as if `-c` were given. Fix: when `!combined`, print the filename on its own line before each file's sorted block. **✓ fixed** — per-file name line emitted when `-c` is absent, so `-c` is now meaningfully different.
- [x] **#R5 — A function's own definition is mis-scoped under itself.** `cc/cxref.rs:411-412` calls `set_function(&name)` *before* `add_definition(&name, ...)`. The spec asks for the name of the function in which a symbol appears *"if it is not a function name itself"*. **[probed]** `fn` appears both as a bogus global row `*0` and as a row scoped to `fn` itself. Fix: add the definition before setting the scope. **✓ fixed** — the function's own definition is recorded at file scope.
- [x] **#R6 — `-D`/`-I`/`-U` relative order is not preserved.** `cc/cxref.rs:53-60` + `cc/token/preprocess.rs:3801-3818`. Like `cflow`, `cxref` explicitly makes this order significant (cxref.md 91587-91588), the opposite of `c17`'s rule, but shares `c17`'s code path. **[probed]** `-DFOO=1 -UFOO -DFOO=2` leaves `FOO` undefined; it should be `2`. Fix: one ordered list, replayed in order. **✓ fixed** — as #F4.
- [x] **#R7 — The symbol listing ignores `LC_COLLATE`.** `cc/cxref.rs:105,451` iterate a `BTreeMap<String, SymbolInfo>` in byte order. **[static]** Fix: sort via `plib::locale::strcoll`. **✓ fixed** — listing sorted via `plib::locale::strcoll`.

### Minor

- [x] **#R8 — The operand extension gate rejects valid C source.** `cc/cxref.rs:564-586` accepts only `.c`/`.h`; OPERANDS imposes no naming rule. **[static]** **✓ fixed** — suffix gate removed; any pathname is offered to the parser.
- [x] **#R9 — Struct/union member names are not tracked.** `cc/cxref.rs:189-191` recurses into the base expression and discards the field identifier. Soft gap — the spec is generic about "symbols". **[static]** **✓ fixed** — member names recorded as references.
- [x] **#R11 — `-w` does not bound the row prefix.** `cc/cxref.rs:480-486` builds the name/file/function columns unconditionally; only line-number continuation wrapping respects `width` (492-513), so a long filename alone can exceed the requested width. **[static]** **✓ fixed** — `-w` now bounds the row prefix as well as the line-number run.
- [x] **#R10 — `LC_MESSAGES`/`NLSPATH` are no-ops.** See cross-cutting note 4. **✓ fixed** — as #F11.

## Detailed conformance matrix

### SYNOPSIS / OPTIONS

| Option | Status | Notes |
|---|---|---|
| `-c` combined listing | **DIVERGES** | Accumulates correctly (`cc/cxref.rs:590-592`) but is indistinguishable from the default — #R4 |
| `-s` silent (no filenames) | PARTIAL | Blanks the filename column (455-459); interacts with the missing header (#R4) |
| `-o file` | CONFORMS | `cc/cxref.rs:540-552`, diagnoses and exits 1 on create failure |
| `-w num` (default 80) | PARTIAL | Default correct (50-51); prefix not bounded — #R11 |
| `-D name[=def]` | PARTIAL | Mechanics correct (value defaults to 1); ordering — #R6 |
| `-I dir` | CONFORMS | Order among `-I`s preserved |
| `-U name` | PARTIAL | Ordering — #R6 |
| `-t`, `-l` | N/A | Not in this spec's OPTIONS list; correctly absent |
| `--`, `[-cs]` grouping | CONFORMS (clap) | |

### OPERANDS / STDIN / INPUT FILES

- [x] `file` — any C source pathname, with no suffix restriction; #R8 closed by removing the `.c`/`.h` gate (`cc/cxref.rs:659-661`).
- [x] Multiple operands, per-file vs combined accumulation — `cc/cxref.rs:558-587` (but see #R4).
- [x] STDIN not used.

### ENVIRONMENT VARIABLES

- [x] `LANG`, `LC_ALL` — `setlocale` at `cc/cxref.rs:530`.
- [x] `LC_COLLATE` — #R7 closed; the listing is re-sorted through `plib::locale::strcoll` (`cc/cxref.rs:533`).
- [x] `LC_CTYPE` — passed to libc.
- [x] `LC_MESSAGES`, `NLSPATH` — #R10 closed. cxref's own diagnostics route through `gettext()`, which reads real `.mo` catalogs honoring `NLSPATH`. Front-end diagnostics now carry translated `error:`/`warning:` labels too (Phase 8); their bodies remain English under the narrowed #U7.

### STDOUT

Spec (91626-91635): format is otherwise unspecified, but the listing *shall*
include a per-file name line (absent `-c`), a sorted symbol list with location
pathname, containing function name, and line-number references, with an
optional `*` marking the declaring reference.

| Requirement | Status |
|---|---|
| Per-file name line | **MISSING** — #R4 |
| Sorted symbol list | CONFORMS (byte order; #R7 for locale) |
| Location pathname per entry | CONFORMS — `cc/cxref.rs:454-459` |
| Containing function, blank for function names themselves | **DIVERGES** — #R5 |
| `*` declaring-reference flag | CONFORMS mechanically (471-475); the attached line number is often wrong (#R2) and macros are absent entirely (#R1) |

### STDERR / OUTPUT FILES / EXTENDED DESCRIPTION

- [x] stderr used only for diagnostics.
- [x] `-o file` fully replaces stdout — `cc/cxref.rs:540-552`.
- [x] EXTENDED DESCRIPTION "None". N/A.

### EXIT STATUS

- [x] `>0` on error — CONFORMS; #R3 closed by `exit_code()` (`cc/cxref.rs:703-709`), which folds `plib::diag` and front-end error counts into the status.

## Test coverage signal

Covered by `cc/tests/tools/posix.rs` since the audit, except where noted:
- [x] `-c` versus default output shape (#R4) — `cc/tests/tools/posix.rs:331` (`cxref_per_file_header_distinguishes_combined`).
- [x] Exact line numbers attached to `*` markers (#R2) and correct scoping (#R5) — `cc/tests/tools/posix.rs:298` and `:365`.
- [x] `#define` symbols in the listing (#R1) — `cc/tests/tools/posix.rs:269`.
- [x] `-w` wrapping/truncation (#R11) — `cc/tests/tools/posix.rs:428`, at widths 80/40/20.
- [x] `-D`/`-U`, including order-sensitivity (#R6) — `cc/tests/tools/posix.rs:628`, five orderings. `-I` search-path handling is still untested for both `cxref` and `cflow`.
- [x] Multiple file operands — `cc/tests/tools/posix.rs:331`, two operands in both plain and `-c` mode.
- [x] Exit status on any error path (#R3) — `cc/tests/tools/posix.rs:72` (`tools_exit_status_mega`).
- [x] Non-`.c`/`.h` operands, unreadable files, parse errors — `cc/tests/tools/posix.rs:409` and `:72`.
- [x] `LC_COLLATE` ordering (#R7) — covered by the two-locale `tools_collation_actually_depends_on_the_locale`; the single-locale `cxref_sort_respects_lc_collate` is kept as a format check.

---

# Suggested PR groupings

Ordered roughly by value-per-unit-risk. Each is independently landable.

- **PR A — "Exit status and error propagation"**: #F1, #T1, #R3, #U2.
  Three of four utilities never report failure, and `c17` aborts instead of
  continuing. Small, mechanical, high user-visible value, and it makes every
  later PR's tests able to assert failure.
  **✓ LANDED** (8f42de37) — note the scope split: `c17`'s #U2 was excluded from this round.

- **PR B — "`c17`: one link, all operands"**: #U1, #U3.
  The single largest correctness defect in the crate. Restructure the driver
  into compile-all-then-link-once, and preserve raw-argv ordering for
  `-L`/`-l`/`-R` while you are in there — the two changes touch the same code
  and are painful to do separately.

- **PR C — "Preprocessor correctness"**: #P2, #P6, #P9, #P10.
  Four independent, small, well-isolated bugs, two of which (null-directive
  source deletion, builtin headers shadowing user headers) silently corrupt
  translation. Each needs one regression test per `cc/CLAUDE.md`.

- **PR D — "C17 language level"**: #P1/#X2, #X8, #P7, #H10, #P11 (docs only).
  Add `-std=`, set `__STDC_VERSION__` to `201710L`, define the
  `__STDC_IEC_559__` family, conditionally define `__STDC_NO_THREADS__`, bump
  `_POSIX_C_SOURCE`/`_XOPEN_SOURCE` to their POSIX.1-2024 values, and fix the
  trigraph claims in `README.md`/`c99-checklist.md`. This was written as the
  prerequisite for the `pcc`→`c17` rename; **the maintainer landed the rename
  first anyway** (#U8), as a deliberately cosmetic change, so a binary named
  `c17` currently reports itself as C11 and this PR is now the debt that pays
  that off. Note that `cc/tests/c11/core.rs:301` currently asserts
  `__STDC_VERSION__ == 201112L`, so the test must move with the macro.

- **PR E — "Constraint diagnostics"**: #L1, #L2, #L3, #L6, #P3, #P4, #P5, #X4, #X9.
  All the same shape: detect a C17 constraint violation and call `diag::error`.
  Best landed alongside a new `cc/tests/diagnostics/` suite asserting
  `error_count() > 0`, which the crate lacks entirely today.

- **PR F — "Atomics correctness"**: #X1, #X6.
  Route `_Atomic` lvalues through the existing atomic opcodes in the
  linearizer, and complete `<stdatomic.h>`. The test must assert on generated
  assembly (`lock` prefix present), because a behavioral test passes today.

- **PR G — "`cxref` data correctness"**: #R1, #R2, #R4, #R5.
  Threading a position through `InitDeclarator` (#R2) is the prerequisite for
  most of the rest; the per-file header (#R4) is a few lines once that lands.
  **✓ LANDED** (a9eee879).

- **PR H — "`ctags` data correctness and format"**: #T2, #T3, #T8.
  Re-key the tag map, fix the `-x` format to the mandated `%s %d %s %s`, and
  adopt `plib::io::write_atomic`.
  **✓ LANDED** (f7c58704).

- **PR I — "`cflow` output format"**: #F2, #F3, #F12.
  Make `-i x` real, fix the back-reference line, and attribute definitions to
  the declarator line. Together these make the spec's worked EXAMPLE
  reproducible byte-for-byte, which is a good acceptance test.
  **✓ LANDED** (ccc84be5) — the spec's worked EXAMPLE is now reproduced exactly and pinned by a test.

- **PR J — "Missing `c17` options"**: #U4, #U6.
  `-B`, `-G`, `-R`, `-s`, plus `TMPDIR` and safe temp-file creation. The
  underlying plumbing largely exists (`--shared`, `-fPIC`, the `-Wl,`
  passthrough), so much of this is surfacing it under the mandated short
  options. Larger and more design-heavy than the rest; deliberately last.

- **PR L — "Header and runtime hygiene"**: #H1, #H9, #H8, #H12, #H11, #X7.
  Bundle `stdint.h`, unregister the always-`#error` intrinsics headers, add
  `CMPLX`, and either implement or reject `-ffreestanding`. Mostly small and
  independent; good first-contribution material.

- **PR K — "Locale collation"**: #T4, #F8, #R7.
  Adopt `plib::locale::strcoll` in the three sort paths. Note the `ctags`
  asymmetry: the tags file must stay in POSIX-locale order; only `-x` follows
  `LC_COLLATE`.
  **✓ LANDED** across the three per-utility commits.

- **PR M — "`cflow` object-file and lex/yacc input"**: #F5, #F6.
  Originally deferred here as "substantial new subsystems". **✓ LANDED**
  (c9336743) — the maintainer opted in. Object symbols are read with the
  `object` crate and call edges recovered from relocations; `.l`/`.y` operands
  are run through `lex`/`yacc` first.

- **Deferred / not scheduled**: #L7 (K&R promotion) is unconfirmed — it did not reproduce — and
  #H4 (aarch64 `long double`) is unverified because this audit ran on x86_64;
  both need a repro before being scheduled. The `getconf`
  programming-environment obligations (c17.md 88105-88179) belong to whichever
  crate owns `getconf`, not here.
