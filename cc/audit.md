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
> **Remediation pass (2026-08-10): every box in this file is now closed.** The
> `c17` findings #P1/#X2, #X1, #X3, #C3, #C5, #U7 and the aarch64 pair #H4/#H13
> are fixed; #H8 and #L7 are closed explicitly as an accepted divergence and a
> non-reproduced claim rather than left as ambiguous unchecked boxes. Each phase
> was gated on the full workspace suite, zero clippy warnings, and the CPython
> -O2 acceptance build.
>
> **The CPython gate was already red before that pass started**, on three
> pre-existing defects that no test caught and that no document recorded. It had
> evidently not been run since the later audit phases landed:
>
> 1. **A stray `;` at file scope was a hard error**, reported as "type specifier
>    missing; implicit 'int'" — a message describing something else entirely.
>    Any function-like macro expanding to nothing leaves one behind;
>    CPython's `_Py_DECLARE_STR()` is exactly that, and it killed
>    `Objects/listobject.c`. Came in with #L1's implicit-int check.
> 2. **Every `"=m"` inline-asm output was miscompiled at `-O` and above.** DCE's
>    use-collector read `asm_data.inputs` only, but a memory *output* also reads
>    its pseudo — the pseudo is the address written *through*. DCE deleted the
>    address computation, so the store went through a garbage register.
>    `_Py_get_387controlword` compiled to `fnstcw (%rax)` with RAX at 0, a null
>    write that segfaulted the bootstrap interpreter. `-O0` was always correct,
>    which is why nothing caught it.
> 3. **Declaration specifiers leaked into type compatibility.** `static int
>    f(void)`'s return type carried `STATIC`, so a call to it was not compatible
>    with `int`; a typedef's TypeId kept its `TYPEDEF` bit. Both made
>    `__builtin_types_compatible_p` answer 0 where gcc answers 1, and both would
>    have made `_Generic` mis-dispatch on typedefs — most of what `_Generic` is
>    for.
>
> The same pass found three more in the atomics implementation itself, all
> recorded with their fixes at #X1 and in the commit log:
>
> 4. **The atomic emitters clobbered RAX/RCX/R8/R9 (and X0/X1/X2/X8) without
>    declaring it.** Six live ints bracketing one `__c11_atomic_fetch_add`
>    summed to 22 instead of 31. This corrupted ordinary code near any existing
>    atomic builtin, at every optimization level, with no `_Atomic` operator
>    involved.
> 5. **8- and 16-bit atomics read and wrote adjacent bytes** on x86_64
>    (`insn.size.max(32)`), so a `lock xaddl` on a byte field carried into its
>    neighbour.
> 6. **`emit_mov_to_reg` silently substituted zero** for `Loc::Xmm`, `Loc::FImm`
>    and `Loc::IncomingArg`, making every `_Atomic float`/`double` operation
>    produce 0 and faulting on an atomic operand passed as the seventh argument.
>
> Two further defects are recorded but **not** fixed, and are the honest
> residue of this pass:
>
> - **`ExprKind::FloatLit` is an `f64`**, so any floating literal outside
>   double's range is already lost at parse time: `LDBL_MAX` evaluates to
>   `inf` on x86_64 today, where `long double` is x87 80-bit and holds it
>   comfortably. Widening it touches the AST, the constant folder and both
>   backends' immediate paths.
> - **`_Atomic` is not accepted after `*` in a declarator** — `int *_Atomic p;`
>   fails to parse, while `int *const p;` and `int *volatile p;` are fine.
>
> **`_Float128` was scoped out once, and is now implemented.** The original
> reasoning still holds as history: it is not a `<tgmath.h>` prerequisite —
> glibc decides `__HAVE_FLOAT128` from a bare `__GNUC_PREREQ(4,3)` test and
> never asks whether the compiler has the type — and a first attempt emitted
> `movt`, an instruction that does not exist, because the x86-64 floating-point
> paths derive their instruction form from a *width*, and this compiler sizes
> both x87 extended and binary128 at 128 bits.
>
> What changed is that two of its prerequisites arrived on their own account.
> `FloatVal` now carries a 128-bit significand, so an exact binary128 constant
> already existed; and the whole soft-float lowering was already written for
> aarch64, where `long double` *is* binary128 — it was merely keyed on the
> target rather than on the type. Re-keying it on the type handed x86-64 every
> `__*tf*` call at once, which left only 16-byte data movement to write.
> `movt` is gone because the format is now resolved from the type at each site
> that knows one. See #C7.

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
> #C1-#C3. **#C1 and #C2 were subsequently fixed** (see their entries); doing
> so uncovered #C4.
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

> **Constraint-diagnostic and declarator series (2026-08-15).** A re-probe of the
> **built binary** against the `c17` spec slice — rather than against this file, which
> claimed every box closed — found five real gaps. Three were taken:
> `getconf POSIX_V8_THREADS_*` (Table 3-6), the declarator defects #C44/#C44a, and the
> 21 undiagnosed constraint violations now closed by #C45-#C49. Two were deliberately
> left: #C50 (`-l xnet`/`-l y`) and #C55 (trigraph default).
>
> Three rows in the `c17` matrix below were **overstated and are corrected**: constraint
> diagnostics ("for everything audited" was carrying the claim — the audited set was
> small), translation limits (eleven minimums verified, a twelfth was failing at level 2),
> and the `-l` standard-libraries row (wrong about whose job resolution is).
>
> Two silent miscompiles are closed: `int x; double x;` emitting `.comm x,4,4`, and
> `int *p; p = 1.5;` compiling to a `cvttsd2si`. A 35-case matrix — 21 violations and 14
> accept-side controls — now agrees with `gcc -std=c17` on every row.
>
> Each phase was gated on the full workspace suite, zero clippy warnings, and the CPython
> -O2 acceptance build (40,817 tests). The gate script itself had to be fixed first: it
> discarded `make`'s exit status through a pipe and read `$?` of the wrong statement, so
> it reported success on a broken build. It caught three genuine false positives after
> that, one of them a pre-existing type-comparison defect (#C49) that nothing had
> previously leaned on hard enough to expose.

> **Live-bug series (2026-08-15, second pass).** A re-probe of the built binary after the
> constraint work found four live bugs and two missing features whose absence produced wrong
> code. All are closed: #C42 and #C43 (`__int128` and `long double` argument passing on
> x86-64), #C58 (universal character names and the four literal encodings), #C59 (attributes
> dropped in silence), plus `__builtin_{add,sub,mul}_overflow` and the symbol attributes
> `weak`/`section`/`visibility`.
>
> Three bugs were found *by* that work rather than before it, each by a test asking a question
> nothing had asked: #C57, where an argument following a stacked `__int128` was mislaid on
> **both** targets by opposite ABI rules; the `\x`-escape half of #C58; and a `char` array
> member initialized from a string literal writing one byte too many. #C38 finally reproduced
> too, under a sharper case, and is now fixed — and writing *its* regression test turned up
> #C61, three separate ways a parameter could spend an argument register without the x86-64
> prologue charging it.
>
> The aarch64 sweep baseline is **14 failures**, not the 13 recorded earlier; it was measured
> at 14 before any of this work and is 14 after.
>
> Two measurements changed a decision that had been made on paper. `mode` could not be a hard
> error, because glibc declares `register_t` with it. And an unrecognised-attribute warning
> was 356 per translation unit until `__const__` -- GCC's const-function attribute, spelled
> like the qualifier keyword -- was recognised as both; it is two now, each naming a real gap.

> **Signedness series (2026-08-17).** A conformance re-probe of the built binary against
> `gcc -std=c17` found two wrong-answer miscompiles that 570 tests did not catch and no
> document recorded, and both turned out to be one defect: **signedness was asked of a
> modifier bit**, and that bit is silent for exactly the two kinds whose signedness does not
> live there — `_Bool`, which is always unsigned, and plain `char`, which follows the target.
> #C87 (a `_Bool` bit-field reading back as −1, so `f == 1` was false) and #C84 (plain `char`
> signed on aarch64, where the backend emitted a correct `ldrb` and the front end sign-extended
> it back) are closed by making `TypeTable::is_unsigned` answer for both.
>
> Fixing it required #C86 first, and that is the finding of the series: the parser never
> applied the integer promotions, so `unsigned char` and `unsigned short` arithmetic was done
> **unsigned** — `(a-b)/2` gave 2147483647 where gcc gives 0 — on x86-64, today, unrelated to
> either bug. `_Bool` and plain `char` were correct *only because* the predicate lied about
> them; the two defects cancelled, and correcting one without the other would have regressed
> the compiler. One rule implemented twice, in the parser and the linearizer, disagreed until
> something leaned on it.
>
> #C52 was taken in the same series and was also worse than recorded: the size expression was
> never evaluated at all, and the bogus 0 was still an integer constant expression, so
> `int z[sizeof(int[n])];` silently became a zero-length array.
>
> Each of the four landed as its own commit, each gated on the full workspace suite, zero
> clippy warnings, and its own CPython -O2 run (`run=40,817 / SUCCESS`). The aarch64 half was
> verified by *executing* c17's output under `qemu-aarch64-static` against
> `aarch64-linux-gnu-gcc`, not by reading assembly. The gate script had to be rebuilt first
> and was proved able to fail — it reported `FAILURE` on its first real run, correctly, on
> `test_gdb`, the one exclusion the maintainer has approved.
>
> Seven findings are left open rather than folded in: #C88 (a variably modified declaration at
> **file scope** is accepted and sized 0 — pre-existing, and the reason #C52 did not inherit a
> rejection it was expected to), #C89, #C90, #C91, #C92, and from the same probe pass: `goto`
> and `switch` into the scope of a variably-modified type are undiagnosed (6.8.6.1p1); a
> trailing comma in a parameter list silently becomes an extra `int` parameter, so
> `void g(int, ); g(1);` is *rejected* — worse than #C53 records; a universal character name
> naming a basic character is accepted (6.4.3p2); a legal call through a 2-D VLA parameter
> draws a false-positive "incompatible pointer type" warning; and an `enum` bit-field is
> rejected where gcc accepts one.

> **Conformance series (2026-08-18).** Eight findings closed across seven commits, each
> gated on the full workspace suite, zero clippy warnings, and its own CPython -O2 run
> (`run=40,817 / SUCCESS`); the ABI and type-system ones on the aarch64 sweep as well.
>
> Two were ABI or wrong-code and were taken first. **#C94**: a zero-width bit-field must
> raise the aggregate's alignment on AAPCS64 and must not on x86-64, and c17 gave the
> x86-64 answer on both -- so a struct it laid out disagreed with every gcc-compiled object
> on aarch64. Fixing it exposed the union case, wrong on *both* targets in the opposite
> direction. **#C88**: `int n; int bad[n];` at file scope compiled, sized **zero**, because
> the first file-scope declarator has its own array-dimension loop that the three existing
> checks never reach; the same arm silently accepted `int a[-1];`.
>
> Four were constraint violations 5.1.1.3 requires a diagnostic for and that c17 accepted in
> silence: **#C95** a jump into a variably modified identifier's scope (which leaves the
> object's size never computed), **#C53** a trailing comma in a parameter list -- which was
> not merely tolerated but became an implicit-`int` parameter, so the *correct* call was the
> one rejected -- **#C96** a universal character name naming a character 6.4.3p2 forbids,
> and **#C90** `sizeof` of an incomplete type, which turned out to be far more than the one
> shape recorded.
>
> Two were wrong answers from asking an integer question about something that is not an
> integer: **#C91** pointer comparison emitted a signed condition, **#C92** an enumeration
> above `INT_MAX` had a correct underlying type but no signedness of its own, so an object
> of it read back negative while the constant was right. **#C97** was the mirror image --
> a *false* diagnostic, on a legal call through a 2-D VLA parameter.
>
> Two of these fixes broke something the others then caught, which is the argument for the
> per-commit gates: #C90's completeness check rejected `sizeof(typeof(a))`, valid code that
> `typeof` makes indistinguishable from an incomplete array, and now carries an explicit
> exemption recorded at #C89; and #C94's fix would have failed the aarch64 runner on a test
> whose expectations were taken from gcc on x86-64.

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
- [x] **#X1 — `_Atomic` objects accessed via ordinary operators compile to non-atomic code.** **✓ fixed.** All four access paths now route through `cc/ir/linearize_atomic.rs`: rvalue reads (branched at the head of `linearize_expr`, the single funnel for every read), plain and compound assignment, and prefix/postfix `++`/`--`. Operators with a native form map to the existing atomic opcodes; `*= /= %= <<= >>=` and all floating-point forms get a CAS retry loop built in the IR rather than per-backend, because an aarch64 LL/SC loop must not contain a call and FP `/=` lowers to a libgcc one. Reads matter even on x86, where the instruction is unchanged: on aarch64 this is `ldar` rather than `ldr`. An `_Atomic` type too large to be lock-free is now a diagnostic naming the type and size — gcc emits `__atomic_*` calls that fail to link without `-latomic`, and c17 links through the host `cc` with no such library. Tested behaviourally, in the IR against a non-atomic control, and on generated assembly for both arches with plain-`int` controls. _(Original finding below.)_
- [x] **#P1/#X2 — `__STDC_VERSION__` is `201112L`; the compiler cannot claim C17.** **✓ fixed.** A `CStd`/`LangOpts` pair in `cc/target.rs` threads the dialect through `Preprocessor::new` and `PreprocessConfig`; the default is `gnu17`, which POSIX.2024's `c17` requires (87830 binds the language to ISO/IEC 9899:2018). `-std=` is parsed rather than discarded, and an unknown value is an error. The test-only copy of the argument rewriter, which had its own `-std=` arm and would have drifted again, is folded into the real one.

  **Subsequently narrowed, deliberately.** That fix briefly gave c17 eight language modes (`CStd` × `gnu`), driving `__STDC_VERSION__`, `__STRICT_ANSI__`, `__STDC_UTF_16/32__`, `__STDC_NO_THREADS__` and the non-reserved predefines. That is machinery this compiler does not want, and the original audit said as much — *"no `-std=` multi-version switching will be added"* (see Status). `CStd` and `LangOpts` are gone. There is **one** language: C17 plus selected GNU extensions. Every predefine above is unconditional, `__STRICT_ANSI__` is never defined, and `unix`/`linux` always are. `-std=` survives only as a shim, because CPython's `configure.ac:2414` adds `-std=c11` unconditionally under `$GCC = yes` without ever probing it — refusing the flag breaks that build and every one shaped like it. A request for an older revision is now *reported* rather than silently dropped, which is what the finding was about in the first place; `-w` and `-Wno-c17-dialect` silence it (282 such warnings in a full CPython build).
- [x] **#L1 — Implicit-int is silently accepted for declarations and function definitions.** **✓ fixed (Phase 5).** `parse_type_specifier` records `saw_explicit_type`, which the two declaration entry points (`parse_external_decl`, `parse_declaration_and_bind_impl`) turn into a diagnostic at their own position. The flag rather than a check inside the specifier parser, because it has seven callers and some — an abstract parameter declarator, a K&R identifier list — legitimately reach it with no specifier. The predicate is subtler than the finding states: `signed`/`unsigned` name a type while setting only a modifier, so `base_kind.is_none()` alone would reject `unsigned x;`. Tests: `diagnostics_implicit_int_is_rejected`, `diagnostics_explicit_types_are_accepted` (eight legal spellings). Original finding: `cc/parse/parser.rs:1954` (`base_kind.unwrap_or(TypeKind::Int)`), reached unconditionally from `parse_external_decl` at `cc/parse/parser.rs:3031`. C99 removed implicit-int; 6.7.2p2 makes "at least one type specifier shall be given" a constraint requiring a diagnostic. **[probed]** `f(void){return 1;}` compiles clean with zero diagnostics. Fix: diagnose when `base_kind.is_none() && typedef_base.is_none()` before defaulting.

### Major

- [x] **#U3 — `-L`/`-l`/`-R` ordering relative to each other and to pathname operands is architecturally lost.** **✓ fixed (Phase 2).** New `cc/linkargs.rs` rescans the normalized argument vector into one ordered `Vec<LinkArg>`; `build_link_line` then substitutes each source operand's compiled object at its own position, so a library is searched where its name is encountered. If the rescan ever disagrees with what clap collected — the drift risk in its `VALUE_OPTIONS` table — it falls back to the old unordered shape rather than emitting a scrambled link line. Tests: `driver_library_order_is_significant` (two archives defining the same symbol; swapping `-lQ`/`-lP` changes the program's result) and `driver_library_named_before_its_user_does_not_resolve`. Original finding: `cc/main.rs:78` (`files: Vec<String>`), `199-203`, `624-636`, `1016-1031`: clap collects each flag into its own `Vec`, then the link line emits *all* `-L` then *all* `-l`, always after all objects. The spec deviates from XBD 12.2 specifically to say this order is significant (87866-87867), and EXAMPLE 3 depends on it. **[static]** — the source structure makes the interleaving unrecoverable after parsing. Fix: pre-scan raw argv for `-L`/`-l`/`-R`, preserving a single ordered stream mirrored onto the link command.
- [x] **#U4 — `-B mode`, `-G`, `-R directory`, and `-s` are unimplemented.** **✓ fixed (Phase 2).** All four added. `-G` shares the existing `--shared` path via `producing_shared()`; `-R` becomes `-Wl,-rpath,dir` in place; `-s` passes `-s` to the link; `-B dynamic|static` becomes `-Wl,-Bdynamic`/`-Wl,-Bstatic` ahead of the libraries it governs, and static binding is restored to dynamic before the host driver's implicit libc/libgcc_s, which are not shipped as archives. An unknown `-B` mode is diagnosed. Tests: `driver_accepts_mandated_options`, `driver_rejects_unknown_binding_mode`, `driver_dash_s_strips_symbols`. Original finding: 4 of the 14 mandated options. **[probed]** each is rejected by clap as `unexpected argument`. `--shared`/`-fPIC` exist as GCC-compat long flags but are not the POSIX short options, and there is no runpath emission (`-R`) or strip (`-s`) path at all. Fix: add all four; wire `-G` to the existing `--shared`/PIC path, `-R` to `-Wl,-rpath`, `-s` to a strip step, `-B dynamic` to library-selection preference.
- [x] **#U5/#P8 — `-E` emits none of the mandated `# <line> "<file>"` markers.** **✓ fixed (Phase 8).** The `-E` emitter opens with a marker naming the primary source and emits another on every stream transition, flagged `1` on entering a file and `2` on returning — GCC's convention, so existing consumers work. `include_file` strips the included stream's begin/end tokens, so transitions are detected from `pos.stream` rather than those markers. **Physical line numbers only**: `#line` sets state on the preprocessor and is never recorded in the stream registry, so `effective_position` cannot see it — the same pre-existing gap that keeps parser diagnostics on physical lines. Tests: `preprocessor_emits_line_markers_for_includes`, `preprocessor_emits_a_marker_without_includes`, `preprocessor_output_with_markers_still_compiles` (the `.i` must stay a valid operand). Original finding: `cc/main.rs:356-361`, `401-408` explicitly skip tokens whose text starts with `<STREAM`. STDOUT (88032-88038) says the `-E` output *shall* contain at least one such line for each file processed via `#include`; RATIONALE (88370-88374) states the purpose is makefile dependency generation. **[probed]** `c17 -E t.c | grep -c '^# [0-9]'` → `0`. Fix: emit `# {line} "{path}"` on stream transitions instead of discarding the markers.
- [x] **#U6 — `TMPDIR` is ignored; temp files use predictable names in a world-writable directory.** **✓ fixed (Phase 2).** One `tempfile::TempDir` per run holds every intermediate. `tempfile` places it under `TMPDIR` when set and creates it `O_EXCL` with a random name, which closes the symlink/pre-creation hazard; dropping it removes the tree, so the five best-effort `remove_file` calls that every `?` and `process::exit` used to bypass are gone. The one `process::exit` that outlives it drops it explicitly first. Test: `driver_honors_tmpdir_and_cleans_up`. Original finding: `cc/main.rs:567,600,941,947` hardcode `format!("/tmp/c17_{}.s", process::id())` etc. **[probed]** grep for `TMPDIR` across `cc/*.rs` returns zero matches; a run leaves `/tmp/c17_<pid>.o`. ENVIRONMENT VARIABLES (88020-88022) requires `TMPDIR` to override the temp directory. Separately, a predictable name in `/tmp` is a symlink/pre-creation hazard. Fix: honor `TMPDIR`, and create temp files with `O_EXCL` + randomized names (or use the `tempfile` crate already in dev-dependencies).
- [x] **#P6 — `#if` arithmetic is signed-`i64` only; large unsigned constants silently become 0.** **✓ fixed (Phase 3).** A new `PpValue { v: i128, unsigned: bool }` threads through all sixteen evaluator methods. Carrying both domains in an `i128` makes a `u64` representable exactly, so once `promote()` has applied the usual arithmetic conversions a plain signed comparison of the carrier *is* the correct unsigned comparison; results wrap back to 64 bits after each operation. `parse_number` parses into `u64` and marks a constant unsigned on a `u`/`U` suffix or when it exceeds `INTMAX_MAX` (C17 6.4.4.1p5). Tests: `preprocessor_if_handles_full_width_unsigned_constants`, `preprocessor_if_uint64_max_is_not_zero`, `preprocessor_if_signed_comparison_is_signed`, `preprocessor_if_unsigned_operand_promotes_the_comparison`. Original finding: `cc/token/preprocess.rs:3732-3744` (`i64::from_str_radix(...).unwrap_or(0)`). C17 6.10.1p4 requires `intmax_t`/`uintmax_t` arithmetic. **[probed]** `#if 0xFFFFFFFFFFFFFFFF` takes the *false* branch. Any `SIZE_MAX`/`UINTMAX_MAX`-style feature test silently misfires. Fix: parse into `u64`/`i128` and thread signedness through `ExprEvaluator`.
- [x] **#P9 — Builtin headers shadow the user's own headers for quoted includes.** **✓ fixed (Phase 3).** `find_include_file` now consults the bundled headers *after* the absolute-path check, the quote-form dir-of-file search and the `-I` list, so they stand in for the compiler's own include directory rather than pre-empting the whole search. `#include_next` still skips them. Tests: `preprocessor_local_header_wins_over_builtin`, `preprocessor_dash_i_wins_over_builtin`, and `preprocessor_builtin_headers_still_resolve` for the other direction. Original finding: `cc/token/preprocess.rs:1694-1762`: `get_builtin_header` is consulted at 1701-1709, *before* the quote-form same-directory lookup at 1721-1726. The `""` form must search the including file's directory first (87905-87910). **[probed]** a local `stddef.h` next to the source is ignored in favor of the builtin. Any project with its own `limits.h`/`float.h`/`stdbool.h`/`complex.h`/`iso646.h`/`stddef.h` is silently miscompiled. Fix: for quote-form includes, search dir-of-file and `-I` dirs before builtins.
- [x] **#X3 — `_Generic` is unimplemented.** **✓ fixed.** Resolved at parse time: the controlling expression's type selects an association and that association's expression is returned verbatim, with no new `ExprKind`. This follows `__builtin_types_compatible_p`, which already folds during parsing, and avoids four hazards — `linearize_expr`'s match has no catch-all, but both `eval_const_expr`s do, which would have silently made `_Generic` non-constant in `case` labels and `_Static_assert`; and `cflow`/`cxref`'s visitors would have skipped the selected subtree. Selection follows 6.5.1.1p2 (array→pointer, function→pointer, qualifiers dropped) via a new `lvalue_converted_type`; matching and the duplicate-association constraint use a new qualifier-sensitive `types_compatible_qualified`, because 6.7.3p10 makes `int` and `const int` *incompatible* — both may appear, and the `const int` arm can never be selected. Reported through `__has_feature(c_generic_selections)`. Known narrow gap: `types_compatible` compares a pointee by TypeId identity rather than recursively, so a *static* function's designator does not match an `int (*)(void)` association.
- [x] **#L2 — Duplicate `switch` case labels are not diagnosed.** **✓ fixed (Phase 5).** `collect_cases_from_stmt` rejects a repeated case value, a second `default`, and — newly noticed here — a case label that is not a constant expression, which could never match and was being dropped without a word. Tests: `diagnostics_duplicate_switch_labels_are_rejected`, `diagnostics_non_constant_case_label_is_rejected`, `diagnostics_distinct_switch_labels_are_accepted` (including Duff's device, where labels nest inside a loop). Original finding: `cc/ir/linearize_stmt.rs:1189-1220`, map built at `1142-1146`, no uniqueness check. C99 6.8.4.2p3 is a constraint. **[probed]** `switch(x){case 1: ...; case 1: ...}` compiles clean; which block wins is arbitrary. Fix: detect duplicates in `collect_switch_cases` and diagnose.
- [x] **#L3 — `return` is never checked against the function's return type.** **✓ fixed (Phase 5).** Both halves of 6.8.6.4p1 are checked in `linearize_stmt` against `current_func.return_type`. `Stmt` carries no position, so `return expr;` reports at the expression and a bare `return;` falls back to `current_pos`. Tests: `diagnostics_return_type_mismatch_is_rejected`, `diagnostics_matching_returns_are_accepted`. Original finding: `cc/ir/linearize_stmt.rs:70-113`. **[probed]** both `void f(void){ return 1; }` and `int f(void){ return; }` compile clean. C99 6.8.6.4p1 makes both constraint violations. Fix: add the two symmetric checks.
- [x] **#L6 — Call argument count is never validated against a visible prototype.** **✓ fixed (Phase 5)** at parse time in `check_call_arity`, not in the linearizer: the same `TypeId` is available at both, but the parser still has the call's position and each argument's, while by linearization `current_pos` points at whichever sub-expression was lowered last. Resolves through a function pointer, skips `params == None` (an unprototyped declaration, where no check is permitted, or an undeclared callee that already diagnosed). **One case is deliberately not checked**: `int f(void)` and `int f()` both intern as an empty parameter list, and telling them apart needs a no-prototype marker that the ABI, inliner and linearizer all read — too much reach for one diagnostic — so a zero-parameter prototype is skipped and `int f(void); f(1);` goes undiagnosed. Every prototype with at least one parameter is checked. Tests: `diagnostics_call_arity_mismatch_is_rejected`, `diagnostics_correct_calls_are_accepted` (six legal shapes). Original finding: `cc/parse/expression.rs:1442-1512` builds the `Call` node with no arity check; `cc/ir/linearize.rs:2325` bounds-checks `arg_idx < params.len()` (written knowing they can diverge) but never flags it. C99 6.5.2.2p2 constraint. **[probed]** both `g(1)` against `int g(int,int)` and `g(1,2,3)` against `int g(int)` compile clean. Fix: compare `args.len()` to `params.len()` (accounting for variadics) and diagnose.
- [x] **#L5 — `typedef` of a VLA type silently loses the size expression.** **✓ fixed (Phase 5)** by rejecting it, as the finding's second option suggests. The miscompile was worse than 'indistinguishable from `int a[]`': `linearize_local_decl` checks `STATIC` and `vla_sizes.is_empty()` but never `TYPEDEF`, so a block-scope `typedef int A[n];` was lowered as a *runtime VLA allocation*. File scope already rejected it. Tests: `diagnostics_typedef_of_vla_is_rejected`, `diagnostics_plain_vla_is_accepted`. Original finding: `cc/parse/parser.rs:1599-1611`: array declarators store non-constant dimensions as `array_size: None` plus a side-channel `vla_sizes: Vec<Expr>`, which every ordinary declarator path forwards but the `is_typedef` branch never inspects. The typedef becomes indistinguishable from an incomplete array `int a[]`. **[probed]** `typedef int arr_t[n]; arr_t x; x[0]=1;` compiles clean with no diagnostic. Fix: thread `vla_sizes` through the typedef symbol, or reject typedef'd VLAs with a diagnostic.
- [x] **#X4 — Incompatible `typedef` redefinition is silently accepted, keeping the first type.** **✓ fixed (Phase 5).** `check_typedef_redefinition` runs at all six `Symbol::typedef` binding sites and compares against the existing binding with `TypeTable::types_compatible`, which already existed (it backed `__builtin_types_compatible_p` and nothing else). Redefinition to a *compatible* type stays legal per C11 6.7p3, which two headers declaring the same alias rely on. Tests: `diagnostics_incompatible_typedef_redefinition_is_rejected`, `diagnostics_compatible_typedef_redefinition_is_accepted`. Original finding: `cc/parse/parser.rs:3651-3657` (pattern repeated at ~3156-3170, 3355, 3510, 3730): on `declare()` returning `Err`, the new declaration is discarded and the *existing* symbol reused, with no type-compatibility check. C11/C17 6.7p3 legalizes redefinition only for *compatible* types. **[probed]** `typedef int foo; typedef char foo; foo x;` compiles clean — strictly worse than C89, where any redefinition was flagged. Fix: compare against the existing type and diagnose a mismatch.
- [x] **#X5 — Unicode literal prefixes `u8""`, `u""`, `U""`, `u''`, `U''` are not lexed.** **✓ fixed (Phase 6), all five.** A `LiteralEncoding` replaces the lexer's `wide: bool`; `u8"..."` needs no new machinery because C11 6.4.5p6 gives it type `char[]` and the source is already UTF-8. `u`/`U` add `Utf16String`/`Utf32String`/`Utf16Char`/`Utf32Char` token types, `Utf16StringLit(Vec<u16>)`/`Utf32StringLit(Vec<u32>)` AST nodes, matching `Initializer` variants and module tables, and 2-byte/4-byte rodata emitters on both backends. The AST carries *code units*, not text, because the lexer keeps one `char` per source byte — so the parser decodes the UTF-8 first, and a code point outside the BMP becomes a surrogate pair for `char16_t`. `char16_t`/`char32_t` map to `unsigned short`/`unsigned int`; they are not declared as names because `<uchar.h>` is deliberately not bundled (see #H1). Tests: `cc/tests/c11/literals.rs` — sizes, values, non-ASCII decoding, static initializers — plus lexer unit tests including that `u8'x'` is *not* a character prefix. Original finding: `cc/token/lexer.rs:634-638` special-cases only `name == "L"`. **[probed]** `u8"x"` → `error: undeclared identifier 'u8'` followed by a confusing parse error, rather than a clear diagnostic. `<uchar.h>` is not bundled (the system one is picked up). Fix: recognize `u8`/`u`/`U` alongside `L`; either implement `char16_t`/`char32_t` semantics or reject with an explicit diagnostic.
- [x] **#X8 — No `__STDC_NO_*` macro is ever defined, including for genuinely absent features.** **✓ fixed (Phase 7).** `__STDC_NO_THREADS__` is defined when the host has no `<threads.h>`, probed per compilation rather than assumed — glibc gained it in 2.28 and macOS still lacks it, so the answer varies by host even for one target. `__STDC_UTF_16__`/`__STDC_UTF_32__`/`__STDC_IEC_559__`/`__STDC_ISO_10646__` are also defined now (see #P7). The finding is right that atomics, complex and VLA are supported and so must *not* be flagged. Original finding: **[probed]** `__STDC_NO_ATOMICS__`, `__STDC_NO_THREADS__`, `__STDC_NO_COMPLEX__`, `__STDC_NO_VLA__`, `__STDC_UTF_16__`, `__STDC_UTF_32__`, `__STDC_IEC_559__`, `__STDC_ISO_10646__` are all undefined. For atomics/complex/VLA that is correct (they are supported). But C17 requires `__STDC_NO_THREADS__` to be defined when `<threads.h>` is unavailable, and c17 bundles no `threads.h` — it relies entirely on the host libc, so on a host without one, portable code cannot feature-test and falls off a cliff. Fix: probe host `<threads.h>` per-target and define `__STDC_NO_THREADS__` when absent.
- [x] **#P7 — `__STDC_IEC_559__`, `__STDC_IEC_559_COMPLEX__`, `__STDC_ISO_10646__` are documented as implemented but do not exist.** **✓ fixed (Phase 7).** `__STDC_IEC_559__`, `__STDC_ISO_10646__`, `__STDC_UTF_16__` and `__STDC_UTF_32__` are now defined. **`__STDC_IEC_559_COMPLEX__` is deliberately left undefined**: it asserts Annex G conformance, and at the time #C1/#C2 below showed complex support did not meet that bar. **Re-verified after those were fixed** — the original reason is dead, and the macro stays undefined for a measured one: c17's complex arithmetic is byte-identical to gcc's at all three precisions across the Annex G cases, but G.5.1p4 is not met (an infinite operand against a NaN gives NaN, not an infinity). gcc fails that rule too and defines the macro anyway; c17 does not make a claim its arithmetic does not support. `c17_complex_infinity_rules_match_gcc` asserts the failing case in the failing direction, so implementing G.5.1 breaks the test and forces this decision to be retaken. Tests: `c17_conditional_feature_macros`, `c17_does_not_claim_annex_g_complex`. Original finding: `cc/doc/c99-checklist.md:793-795` marks all three `[x]`; a project-wide grep finds zero hits outside that doc. **[probed]** all three undefined. Since c17's floats are native IEEE-754, `__STDC_IEC_559__` *should* legitimately be defined; its absence makes conforming numeric code take a needlessly conservative path. Fix: define them, or correct the checklist.
- [x] **#P3 — Incompatible macro redefinition is not diagnosed.** **✓ fixed (Phase 4).** `handle_define` compares against any existing definition via `macro_redefinition_conflict`, which checks kind, parameter count and spelling, variadic form, and the replacement list. Reported as a **warning**: 6.10.3p2 requires only a diagnostic, and rejecting outright would break a great deal of code that redefines a macro benignly. Two exemptions keep it honest rather than noisy — whitespace *before* the first replacement token is not a separation within the list, and a macro the implementation predefined was not defined by a `#define` directive, so the constraint does not reach it. Without either, every compilation against glibc warned: `features.h` redefines `__GLIBC__` (same value, different leading space) and `__GLIBC_MINOR__` (we hardcode 17, the host says 39). Tests: `preprocessor_incompatible_redefinition_is_diagnosed`, `preprocessor_identical_redefinition_is_silent`, `preprocessor_redefining_a_predefine_is_silent`, `preprocessor_including_a_system_header_is_warning_free`, plus unit tests `test_macro_redefinition_conflict_detection` and `test_replacement_lists_ignore_leading_whitespace`. The pinning test noted below was rewritten. Original finding: `cc/token/preprocess.rs:1104-1207` overwrites via `HashMap::insert` unconditionally; the existing test `test_macro_redefinition` (4439-4446) documents the silent-override behavior as intended. C17 6.10.3p2 is a constraint requiring a diagnostic unless replacement lists are identical. **[static]** Fix: compare old vs. new on redefinition.
- [x] **#P4 — Function-like macro argument-count mismatch is not diagnosed.** **✓ fixed (Phase 4).** `check_macro_arity` runs at the top of `expand_function_macro`, which both expansion paths funnel through. It recovers the distinction `collect_macro_args` cannot see — `F()` supplies *one empty argument*, not zero, unless the macro takes no parameters — and allows an empty variadic tail, which is a GNU extension C23 adopted. Tests: `preprocessor_macro_arity_mismatch_is_diagnosed`, `preprocessor_legal_macro_arities_are_accepted`. Original finding: `cc/token/preprocess.rs:2701-2763`, `2765-2924` use `args.get(idx).cloned().unwrap_or_default()` with no count check. C17 6.10.3p4 constraint. **[static]** Fix: validate arity at the `expand_function_macro` call sites.
- [x] **#P5 — `#`/`##` placement constraints are unchecked.** **✓ fixed (Phase 4).** `tokens_to_macro_body` rejects `##` at either end of a replacement list (6.10.3.3p1) and, in a function-like macro, a `#` not followed by a parameter (6.10.3.2p1). In an object-like macro `#` stays an ordinary token, which is correct and is pinned by a test. Tests: `preprocessor_paste_at_either_end_is_diagnosed`, `preprocessor_stringify_without_a_parameter_is_diagnosed`, `preprocessor_legal_hash_forms_are_accepted`. Original finding: `cc/token/preprocess.rs:1210-1330` never rejects `##` at the start/end of a replacement list (6.10.3.3p1) or a `#` not followed by a parameter in a function-like macro (6.10.3.2p1); both become literal tokens. **[static]** Fix: add both checks in `tokens_to_macro_body`.
- [x] **#X6 — `<stdatomic.h>` is missing most mandated typedefs and `atomic_is_lock_free`.** **✓ fixed (Phase 7).** All of C11 7.17.6.1 added, spelled against the compiler's own `__*_TYPE__` predefines so that including `<stdatomic.h>` alone suffices. `atomic_is_lock_free` follows from the object's size: both targets are lock-free up to pointer width and no wider. Test: `c11_stdatomic_provides_the_mandated_typedefs`. Original finding: `cc/include/stdatomic.h:25-37` defines only the fixed-width base types. Absent: `atomic_intptr_t`, `atomic_uintptr_t`, `atomic_size_t`, `atomic_ptrdiff_t`, `atomic_wchar_t`, `atomic_char16_t`, `atomic_char32_t`, all `atomic_{u,}int_{least,fast}N_t` (C11 7.17.6.1), and `atomic_is_lock_free()` (7.17.5.1). **[static]** `atomic_size_t` in particular is common in lock-free code. Fix: add the typedefs and the lock-free query.
- [x] **#X7 — `<complex.h>` is missing the `CMPLX`/`CMPLXF`/`CMPLXL` macros.** **✓ fixed (Phase 7)**, all three, via the existing `__builtin_complex`. Test `c11_cmplx_constructs_exactly` proves the point of the macro: `CMPLX(1.0, INFINITY)` keeps the real part at 1.0, while `1.0 + INFINITY*I` makes it NaN. **Only the `double` form is verified**, because writing the test surfaced two pre-existing codegen defects recorded as #C1/#C2 below — `CMPLXF` and `CMPLXL` are correct macros sitting on broken lowering. Original finding: `cc/include/complex.h` defines `I` via `__builtin_complex` but not the C11 7.3.9.5-7 constructors, which exist precisely so `x + y*I` (which can corrupt `NaN`/`Inf` operands) has an exact alternative. **[static]** `cc/doc/c99-checklist.md:645` claims them done; zero hits for `CMPLX` anywhere under `cc/tests`. Fix: define all three in terms of the existing `__builtin_complex`, and correct the checklist.
- [x] **#H4 — aarch64 `long double` load/store collapses to 64-bit.** **✓ fixed, and worse than recorded.** `FpSize` gains `Quad`, distinct from x87 `Extended`; `from_bits` now takes a target, because 128 bits is ambiguous without one — which also fixes `va_arg(ap, long double)` there. Three further blockers had to go, and the store was the worst: `emit_store` had **no floating-point dispatch at all** on aarch64, so a 128-bit store fell through to `emit_struct_store`, whose operand match ends in `_ => return`, and **was silently dropped** — `g = y` on a global emitted no instruction. Also `emit_ret` decided FP-ness by location rather than type (an rtlib result lands on the stack, so it returned in x0), and FP spill slots were hardcoded to 8 bytes. A binary128 constant is assembled from two halves via a new `InsGpToVecD`. `long double _Complex`, which used to panic the compiler, is a two-element HVA in q0/q1 — `HfaBase` gains `Float128` and keys on width so Darwin stays on doubles; the old ABI test asserted `Indirect`, and gcc disagrees, emitting no x8 indirect-result pointer. Output now matches `aarch64-linux-gnu-gcc` instruction for instruction and assembles clean. **CI now runs an `ubuntu-24.04-arm` job**, which is the runner this item was deferred twice for wanting.

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
- [x] **#U7 — Runtime diagnostics are hardcoded English.** **✓ fixed.** `gettext-rs` gains `gettext_args` — the runtime formatter its own `gettext!` doc comment described as missing — using **positional** `{0}`/`{1}` placeholders so a translation can reorder substitutions, which the `.replacen("{}", v, 1)` chains open-coded in `m4` and `man` cannot express. `cc/diag.rs` grows `error_args`/`warning_args`/`error_plural`. All 26 `format!`-built sites are converted and the 15 plain literals are wrapped; nothing in `cc/token`, `cc/parse` or `cc/ir` builds a diagnostic with `format!` any more. The two messages that bake the English singular/plural into the sentence use `error_plural`, with both forms as msgids. Deliberately untranslated: `#error`/`#warning` bodies (the user's own source) and `io::Error` text (the OS's).
- [x] **#U8 — The binary was named `pcc`, not `c17`.** `cc/Cargo.toml:31-33`. **✓ fixed** — the binary is `c17`. The rename also covered the hidden internal flags (`--pcc-*` → `--c17-*`), the diagnostic prefix (`c17: …`), `__VERSION__`, the `Generated by c17 …` assembly header, the DWARF `DW_AT_producer` string, the `_C17_LIMITS_H`/`_C17_FLOAT_H` builtin-header guards, the `/tmp/c17_<pid>.*` temp names, and `lib/utils.tsv`. No `pcc` compatibility alias was kept. The maintainer scoped this to a cosmetic rename, so the caveat noted here originally still stands: #P1/#X2 did **not** land with it, and the `c17` binary misreports its language level as `201112L` until it does.
- [x] **#U9 — `-` is accepted as a pathname operand though STDIN says "Not used."** **✓ documented (Phase 2)** — kept as a deliberate GCC-compatible extension, as the finding recommends; `linkargs::scan` treats a bare `-` as an operand rather than an option, with a test pinning that. Original finding: `cc/main.rs:321-323`, `840`. A harmless GCC-compatible extension; conforming applications never pass a bare `-`. Document rather than remove.
- [x] **#U10 — `-c -o out` with multiple inputs overwrites `out` once per input, silently.** **✓ fixed (Phase 1)** — still unspecified per the spec, but now warned rather than silent. Test: `driver_warns_on_dash_c_dash_o_with_multiple_sources`. Original finding: `cc/main.rs:575`. The spec leaves this unspecified (88338-88343) so it is not a defect; a one-line warning would help. Not required for conformance.
- [x] **#L8 — `cc/doc/c99-checklist.md` overclaims and should not be used as compliance evidence.** **✓ fixed (Phase 9), then the file was deleted outright.** A checklist whose accuracy has to be audited is a liability; its four remaining open rows were resolved or rehomed first — the typedef-of-VLA row is a deliberate rejection, `__STDC_IEC_559_COMPLEX__` is settled under #P7, and the two external-test-suite rows were never conformance claims and now sit in `doc/TODO.md`. Conformance is tracked in this file. What Phase 9 had done before that: the checklist opened with an accuracy note naming the failure mode, and the individual entries this finding cites are corrected: `return;`/`return expression;` and the 1023-case-label row now say the constraint is diagnosed, the mixed narrow/wide concatenation row says when it was implemented, and the two rows that were simply *false* are un-ticked — typedef-of-VLA (now rejected) and `__STDC_IEC_559_COMPLEX__` (deliberately undefined). Original finding: Every item in #L1–#L6 has a corresponding `[x]` (e.g. lines 353, 431, 564, 580, 856-858). The checklist treats "parses without panicking" as "conforms". Fix: correct the entries and add rows for implicit-int and duplicate-case detection, which the checklist does not track at all.
- [x] **#X10 — `cc/doc/c11-checklist.md` *under*-reports anonymous struct/union inside `union`.** **✓ fixed (Phase 9); that file is now deleted too, for the reasons under #L8** — the finding's reading was right, it already worked. Boxes ticked after adding the regression test the finding asked for: `c11_anonymous_members_inside_a_union`, which checks member promotion, storage overlay and `sizeof`. Original finding: Lines 138-139/199-200 mark these unimplemented, but `cc/parse/parser.rs:2061-2147` uses one shared member-parsing path whose anonymous-member branch (2135-2147) is gated only on `is_struct_or_union && is_special(b';')`, with no `is_union` restriction — so it already works. **[static]** Fix: add a regression test, then tick the boxes.
- [x] **#H1 — `<stdint.h>` and `<uchar.h>` are not bundled; the host's are used.** **✓ fixed for `stdint.h` (Phase 7)** — `cc/include/stdint.h`, the full C17 7.20 surface built on the `__INTn_TYPE__`/`__INTn_MAX__` predefines. `uchar.h` is **not** bundled, per the maintainer's scoping. Bundling it immediately exposed a real pre-existing bug it had been masking: `__INT64_TYPE__` said `long long int` on LP64 where the host says `long`, so the two headers disagreed about `int64_t`. The 64-bit type and constant-suffix predefines now follow `target.long_width`. Tests: `c17_bundled_stdint_provides_the_mandated_surface`, `c17_bundled_stdint_agrees_with_system_headers`. Original finding: **[probed]** all of `float.h iso646.h limits.h stdalign.h stdarg.h stdbool.h stddef.h stdnoreturn.h complex.h stdatomic.h` are bundled in `cc/include/` and compile; `stdint.h`, `uchar.h`, and `threads.h` resolve to the system copies. C17 4p6 puts `<stdint.h>` in the *freestanding* set, which the implementation must supply itself. It works today because `cc/arch/mod.rs:112-231` predefines the full GCC-compatible `__INTn_TYPE__`/`__INTn_MAX__` surface glibc's `<stdint.h>` expects — a functional but not freestanding-capable design that hard-depends on a full libc sysroot even for trivial programs. `cc/tests/c99/stdlib_headers.rs:12` documents the delegation explicitly. Fix: bundle `stdint.h` (and `uchar.h` if #X5 is addressed).
- [x] **#H10 — `_POSIX_C_SOURCE` defaults to `200809L` (POSIX.1-2008), not `202405L` (POSIX.1-2024).** **✓ fixed (Phase 7)** — `_POSIX_C_SOURCE` is `202405L` and `_XOPEN_SOURCE` is `800`. Verified against a broad system-header set (`unistd.h`, `sys/stat.h`, `fcntl.h`, `time.h`, `signal.h`, `pthread.h`) plus the full `c99_stdlib_headers_mega` suite. Original finding: `cc/os/mod.rs:24`. **[probed]** Every delegated system header gates POSIX.1-2024 prototypes behind this macro, so a `c17`-branded, POSIX.1-2024-targeting compiler exposes a 2008-era system interface by default. (`_XOPEN_SOURCE=700` at `cc/os/linux.rs:30` is likewise the 2008-era value.) Fix: bump the defaults; `-D` already overrides them.
- [x] **#H9 — `xmmintrin.h`/`emmintrin.h` are registered builtin headers that unconditionally `#error`.** **✓ fixed (Phase 7)** — both dropped from `get_builtin_header`, so the ordinary search can find a real one. The files themselves stay for reference. Original finding: `cc/include/xmmintrin.h:13`, `cc/include/emmintrin.h:13`, wired at `cc/builtin_headers.rs:49-53,72-73`. **[probed]** Not ISO/POSIX-mandated, so no conformance impact on its own — but combined with #P9 (builtins win over `-I`), registering them means a user who *does* have a real SSE intrinsics header on their include path gets c17's hard failure instead. Fix: drop them from `get_builtin_header()` so the normal search can find a real one.
- [x] **#H8 — `FLT_ROUNDS` is a static `1`.** **Closed as an accepted divergence; the finding's premise is wrong.** It states that "glibc's own `<float.h>` calls `__flt_rounds()`" — glibc ships no `<float.h>` at all (GCC does) and exports no `__flt_rounds`; an implementation attempt failed to link against it. GCC's own `<float.h>` defines `FLT_ROUNDS` as a literal `1`, exactly as we do. Tracking the live mode would mean pulling `<fenv.h>` machinery into `<float.h>`, which no mainstream implementation does; code that needs it calls `fegetround()`. The header comment now records this. Original finding: `cc/include/float.h:69`. C17 5.2.4.2.2p8 requires it to reflect the current rounding mode; glibc's own `<float.h>` calls `__flt_rounds()`. c17 has no `<fenv.h>` coordination, so a runtime `fesetround()` is never reflected. **[probed]** Low impact; note only.
- [x] **#H12 — `-ffreestanding` is accepted and silently ignored; `__STDC_HOSTED__` is always `1`.** **✓ fixed (Phase 7)** by diagnosing it, the finding's first option: there is no freestanding environment to enter, so accepting the flag was the misleading part. `-fhosted` is accepted, being what we already are. Test: `c17_ffreestanding_is_diagnosed`. Original finding: The generic `-f*` catch-all at `cc/main.rs:767` swallows it, and `__STDC_HOSTED__` is hardcoded `"1"` in two places (`cc/token/preprocess.rs:550`, `cc/os/mod.rs:22`) with no path that flips it. **[probed]** Consistent with #H1 (there is no real freestanding mode), but silently accepting a flag that does nothing is misleading. Fix: diagnose it as unsupported, or implement it.
- [x] **#H11 — `cc/rtlib.rs` is not the runtime-helper source of truth its doc comment claims.** **✓ fixed (Phase 9)** — the doc comment now says what the file actually holds (the `_Float16` pairs) and names the three places the load-bearing helper names really live, so it cannot be mistaken for an inventory. Original finding: `cc/rtlib.rs:9-14` says it "maps C type operations to their corresponding runtime library function names", but it holds only the 12 `_Float16` conversion pairs. The load-bearing names are hardcoded at their call sites: `__divti3`/`__udivti3`/`__modti3`/`__umodti3` at `cc/arch/mapping.rs:1326-1329`, the whole `__*tf*` quad family at `cc/arch/aarch64/mapping.rs:38-215`, and bare `memcpy`/`memset` at `cc/arch/x86_64/features.rs:1236,1270`. **[static]** Not a correctness bug — all are stable libgcc/compiler-rt export names — but documentation drift worth fixing before more targets land.

### Found during remediation (not in the original audit)

- [x] **#C1 — `float _Complex` silently loses its imaginary part.** **✓ fixed.** Not a float-specific bug but a whole-family ABI defect: `classify_param`/`classify_return` returned two SSE eightbytes for *every* complex type. §3.2.3 says otherwise — `float _Complex` is 8 bytes and so is **one** eightbyte with both halves packed into a single XMM, `double _Complex` is two, and `long double _Complex` is COMPLEX_X87 (see #C2). Passing a float complex in two registers left the imaginary half where the callee never looked, so `cimagf` read 0 while `crealf` happened to be right. Fixed in the classifier plus every site that counted FP argument registers — the caller's setup, the callee's prologue, and the register allocator's own argument assignment, which was the one that made a `float` argument *after* a `float _Complex` land in the wrong register. Two further defects surfaced while testing and are fixed here too: a complex *parameter* was spilled by the allocator into an unrelated 8-byte slot, losing half a `double _Complex` and suppressing the prologue's correct handling; and passing an rvalue complex — `f(g())` — took the address of a value rather than a value's address, faulting in the callee. Tests: `cc/tests/c99/complex_abi.rs`.
- [x] **#C2 — `long double _Complex` emits an invalid instruction.** **✓ fixed.** `movt` was `mov` plus `FpSize::Extended`'s x87 size suffix `t`, built because the type was routed down the SSE path — but an x87 value has no XMM form at all, so the mnemonic did not exist. §3.2.3 classifies it COMPLEX_X87: passed in **memory**, returned in **st(0)/st(1)**. Both are implemented. The return convention is not optional: complex multiply and divide lower to libgcc's `__mulxc3`/`__divxc3`, which follow it, so returning indirectly would have silently disagreed with the very library calls the arithmetic depends on. Construction, argument passing, return, addition and multiplication are all verified. Fixing it also required correcting a value-vs-address confusion in the x87 load and store paths, which assumed a stack slot always *is* the storage when for a temp it holds a pointer to it — that is what wrote a returned value over its own buffer pointer and past the end of the frame. Tests: `c99_complex_long_double_argument_passing`, `c99_complex_arithmetic_compiles_and_works`.
- [x] **#C4 — Converting a complex value between base precisions is broken.** ✓ fixed. Three sites read the source with the *target's* base type and stride: `emit_assign`, the declaration initializer in `linearize_stmt`, and `emit_complex_binary` — the last is why the textbook `x + y*I` was wrong at every precision except `double`. Each now converts through the source's own base type first (`complex_operand_at_precision`). Tests: `c99_complex_mixed_precision`. Original finding: **[probed]** `double _Complex d = ...; float _Complex f = d;` and the same with `long double` both **segfault**; `float _Complex f = 2.0f + 3.0f*I;` yields `(0, 2)`. The assignment path in `emit_assign` reads the source with the *target's* base type and stride, so converting up loads 16 bytes from an 8-byte real part and then reads a full element past the end of the source object. That path is fixed here; the *initializer* path is separate and still wrong, which is what the cases above exercise. This is why `2.0f + 3.0f*I` misbehaves at all: `I` is `__builtin_complex(0.0, 1.0)`, a **`double`** complex, so every mixed-precision use of `I` is a conversion. Same-precision arithmetic is correct — `2.0 + 3.0*I` gives `(2, 3)` — and building each literal at its own precision works for all three base types. Pre-existing and independent of #C1/#C2, both of which are ABI-side; this one is in the front end's complex initializer lowering.
- [x] **#H13 — AArch64 does not pass a complex argument that runs out of FP registers.** **✓ fixed.** Both sides were wrong, differently. The caller pushed the argument pseudo onto its stack list twice and moved each with `emit_fp_move` — but a complex pseudo holds the value's *address*, so it wrote the pointer's bit pattern into both halves; it also charged 8 bytes per element where the callee rounds the pair to 8 total. The callee's prologue skipped the copy into the local entirely. And neither side saturated NSRN, which AAPCS64 §6.4.2 sets to 8 once anything is stacked — the rule that made the x86_64 fix untransplantable and kept this open. `c99_complex_argument_spilled_to_the_stack` loses its `#[cfg(target_arch = "x86_64")]`, so macOS CI executes it on aarch64.

- [x] **#C5 — A cast to a narrower type is dropped when the value is passed to a variadic function.** **✓ fixed, with a different diagnosis than recorded.** The cast is not being walked past. The implicit argument-conversion block is guarded by `arg_idx < params.len()`, and a variadic argument sits at or past that bound by definition, so **no integer promotion was applied to variadic arguments at all** — the in-code comment claiming integers were "already handled above" was simply false. The cast contributes nothing either, because `emit_convert` returns early for a same-size integer conversion. Fixed by promoting `_Bool`/`char`/`short` to `int` for variadic arguments, reusing the existing `integer_promote`. aarch64 now emits the `uxtb` it was missing.

- [x] **#C3 — `__GLIBC_MINOR__` is hardcoded to `17`.** **✓ fixed.** Read from the `features.h` on the include path — the header set the translation unit will actually use, and the one whose mid-file redefinition it must agree with — rather than from the running libc or a guess. The OS macro tables widen from `&'static str` to `String` to carry it.

- [x] **#C6 — A `float` constant converted to `long double` is read at the wrong width, yielding 0.** **✓ fixed.** `long double x = 1.5f;` produced `0`. Converting float or double to x87 long double loads the source from memory, and the load width came from the expression's C *type*; but a constant is materialised into `double_constants`, emitted as `.quad`, so it is 8 bytes whatever its type. A float-typed one was then read back with `flds`, taking the low half of the double — zero for any value whose significant bits sit in the top half. Pool constants now load as doubles, which is lossless since the f64 holds the f32 value exactly. Runtime conversions were always correct, which is why nothing caught it. Found while verifying Annex G, and the reason that verification was worth doing rather than asserting: `<math.h>` spells `INFINITY` as `__builtin_inff()` — a **float** — so *every* `long double` infinity in a program was silently `0`, which is how `CMPLX(INFINITY,0)/CMPLX(2,0)` came out finite. Test: `c17_complex_infinity_rules_match_gcc`.

- [x] **#C7 — `isnan()` on a `long double` returns 65535 rather than 1.** **Closed.** The diagnosis in the original entry was right about the mechanism and wrong about the price. glibc's `<math.h>` uses `__builtin_isnan` only at `__GNUC_PREREQ (4,4)`; below it, the `sizeof` ternary calls `__isnanl`, which returns raw class bits. Both conform — C99 7.12.3.4 asks only for "a nonzero value" — but `isnan(x) == 1` is what code writes. Claiming 4.4 required `__float128`, because `bits/floatn.h` turns on `__HAVE_FLOAT128` one threshold *below* it, at 4.3 on x86-64. That type now exists (see the note above), the claimed version is **6.5.0**, and `isnan(x)` answers 1 at float, double and long double, matching gcc. Two further things were needed and are done: `__builtin_isinf_sign`, which `isinf` switches to at 4.4; and folding a conditional whose condition is a constant — glibc's `isinf` is `__builtin_types_compatible_p(...) ? __isinff128(x) : __builtin_isinf_sign(x)`, and emitting the untaken arm left an undefined reference to `__isinff128` in every object that used `isinf`. Test `builtins_isnan_answers_one_at_every_width`. The ceiling is measured: 6.5 is clean and 7.0 is not, because `__HAVE_FLOATN_NOT_TYPEDEF` makes `_FloatN` native types and `stdlib.h` then declares `strtof32x` in terms of a `_Float32x` this compiler does not have.

- [x] **#C8 — A long hex float significand wrapped to a wrong value.** **✓ fixed.** `0x1.0000000000000002p+0` evaluated to **3.0**. The significand was read into a `u64` and divided by `1u64 << (4 * digits)`; sixteen fraction digits make that a shift of 64, which wraps to a shift of 0 in a release build, so the fraction was added whole rather than scaled — and `u64::from_str_radix` overflowed on the same input. Wrong by a factor of three, silently, in a C99 feature the matrix below lists as conforming. Now decomposed exactly (integer significand plus binary exponent, sticky bit beyond 128 bits) and converted once. Verified differentially against gcc across 16-digit fractions, 31-digit significands, both ends of the exponent range, subnormals, and the leading/trailing-dot forms. Tests: `c99_hex_float_long_significand`, `test_hex_float_long_significand_is_not_mangled`.

- [x] **#C9 — `_Atomic` was not accepted as a pointer qualifier at file scope.** **✓ fixed.** `int *_Atomic p;` parsed inside a function and failed at file scope, and `int *_Atomic;` was *accepted* there — with no qualifier arm to consume it, `_Atomic` fell through to the name position and declared a variable called `_Atomic`. gcc rejects that. C17 6.7.6.1 puts a type-qualifier list after `*` and 6.7.3 makes `_Atomic` one. The loop existed in three copies (`parse_declarator`, `parse_function_def`, `parse_external_decl`) and only the first had been updated; all three now share one helper. `_Atomic` was also missing from the array declarator's qualifier list (6.7.6.2), so `void f(int a[_Atomic 4]);` was rejected outright.

- [x] **#C10 — A keyword can be used as a declarator name, and one non-keyword could not.** **Fixed.** Filed for `sizeof` / `_Alignof` / `_Generic`, and **[probed]** it was wider than that: every *statement* keyword was accepted too, and c17 emitted a real symbol for it. `int if;` produced `.globl if` and an object in `.data` that no later C translation unit could name; `struct S { int if; };` and `p->if` compiled cleanly. The declarator-name position rejected only `TYPE_KEYWORD`-tagged identifiers, which covered neither the deliberately untagged `sizeof` family nor `STMT_KW`.

  The opposite error was present at the same time, which is why they had to be fixed together: `int typeof;` was **rejected**, and gcc accepts it — `typeof` is a GNU extension, not a C17 keyword. It was being consumed as a type specifier which then demanded `(`. It is now taken as a specifier only when a `(` actually follows, since `typeof` has no other form.

  The reserved set is a new `RESERVED_NAME` tag rather than a derived predicate, because the boundary is empirical — exactly what gcc rejects in C17 mode. `alignof`, `typeof_unqual` and `_BitInt` are C23 spellings and stay usable; `__typeof__` and `__typeof` stay reserved, since a leading double underscore is reserved to the implementation in every scope (C17 7.1.3). `_Imaginary` gained a table entry with no type behind it: C99 6.4.1 reserves the keyword even where Annex G's imaginary types are absent. The check is a separate `expect_declarator_name`, not a change to `expect_identifier`, whose eighteen callers include labels, struct tags and member references — all of which live in their own namespaces and may legitimately use these words.

  Tests: `diagnostics_keywords_are_rejected_as_declarator_names`, `..._in_every_declarator_position`, `diagnostics_non_keywords_remain_usable_as_names`, `diagnostics_labels_and_tags_are_unaffected`.

- [x] **#C19 — Three complex-valued expressions operated on the value's *address* instead of the value.** **Fixed.** **[probed]** Found by the Phase 4 sweep, which ran `_Complex` arithmetic differentially against gcc for the first time.

  A complex value lives in memory and travels by address; three paths did not know that and treated the address as the number.

  - **`-z` crashed.** The scalar unary path applied integer negation to the address, and the small negative number that produced was passed on as though it were a complex object, so `creal(-z)` dereferenced it. A segfault on valid code.
  - **`z += 1.0` crashed**, for the same reason: only `AssignOp::Assign` had a complex arm, so every compound assignment computed on the address and stored the result over the object.
  - **`(double) z` produced a number in the range of a pointer bit pattern** — 4.6e18 where gcc gives 3.0 — because the cast path reinterpreted the address rather than loading the real part. C17 6.3.1.7p2.

  `emit_complex_negate`, `emit_complex_to_real` and a compound-assignment arm that reuses `emit_complex_binary` fix the three. Test: `c11_complex_unary_minus_and_real_cast`, covering negation at all three precisions (the part stride differs in each), negation nested in larger expressions, casts to `double`/`float`/`int`, and all four compound operators with both real and complex right-hand sides.

  The same sweep found `_Complex` **correct** for multiply, divide, add, conjugate, `cabs`, mixed real/complex operands, equality, array elements, function arguments and returns, and the Annex G infinity rules — so these three were the gap, not the feature.

- [x] **#C23 — `_Float16` was not a floating type to the ABI classifier.** **Fixed.** **[probed]** The classifier kept its own `is_float`, and `_Float16` was missing from it while the type table's included it. An eightbyte holding one therefore reached the default arm, which answers MEMORY, so `struct { _Float16 h; }` was returned through a hidden pointer nothing ever wrote and read back as **zero** with no diagnostic; gcc returns it in `xmm0` (`pinsrw $0, …, %xmm0`). A scalar `_Float16` fell to the size-based default and was classified INTEGER, so the argument accounting charged it a general register while the backend used an SSE one — harmless only because the two sides keep separate indices. An aggregate carried in one SSE register also needs its move width from the ABI class rather than from its own type, which answers `double` whatever the struct holds: two bytes too wide for a `_Float16` and half what a binary128 needs. Test: `codegen_float16_struct_returns_in_a_register` and `float16_is_classified_as_a_floating_type`. Found while fixing #C21.

- [x] **#C24 — An aggregate holding only a `__float128` travelled in two XMM registers.** **Fixed.** **[probed]** System V classifies binary128 SSE + SSEUP, and SSEUP never travels on its own: the pair is one register carrying all sixteen bytes, which is what a scalar `__float128` has always answered. `classify_aggregate` counts eightbytes while every consumer counts registers, so `struct { __float128 v; }` went into `xmm0` *and* `xmm1` and a gcc-compiled peer read only its low half. gcc emits a bare `ret` for `__float128 f(struct Q e) { return e.v; }`. Adding a `RegClass::SseUp` variant was the alternative and was rejected: `RegClass::merge` and the mixed-return dispatch both end in wildcard arms that would swallow it silently, so the compiler could not enumerate the sites, and the register *count* would still have been wrong everywhere until each was found by hand. The *sole content* restriction is load-bearing — `union { __float128 v; double d[2]; }` merges SSEUP with SSE and really is two registers, which gcc confirms. Test: `codegen_lone_binary128_struct_uses_one_xmm` and `a_lone_binary128_aggregate_travels_in_one_register`. Found while fixing #C21.

- [x] **#C30 — A one-element HFA was returned in a general register on aarch64, and a binary128 one killed the compiler.** **Fixed.** **[probed]** AAPCS64 returns a struct holding a single `float`, `double` or `long double` exactly as it returns the bare scalar: in V0. c17 sent all three out through a general register while the *caller*, which does consult the ABI, read the floating one — so the two sides disagreed inside a single program. gcc emits `fmov s0, 3.5e+0` for `struct F { float v; } mkf(void)`; c17 emitted `ldr w0`. Worse, the return path recognised an HFA but ignored its element **count**, so a one-element quad HFA took the pair path and tried to move sixteen bytes out of one X register: on aarch64 Linux, where `long double` is binary128, `struct { long double v; }` returned by value **crashed the compiler** with `a binary128 value does not fit one X register`. Test: `codegen_aarch64_one_element_hfa_returns_in_v0`, a cross-target assembly probe, so an x86-64 host proves it. Found by the tests added with #C21; the defects predate that series.

- [x] **#C31 — A spilled binary128 argument lost its top half on aarch64.** **Fixed.** **[probed]** An FP argument register that has to survive a call is stored to the frame in the prologue. Both the slot and the store were a fixed eight bytes, so on aarch64 Linux — where `long double` is binary128 — half of such an argument was dropped and the slot overlapped whatever came after it. Which parameter suffered depended on which path stored it: in `int eql(const long double *v, long double re, long double im)` the first went through another path and survived whole, while the second was stored with `str d1` instead of `str q1`. Comparing two binary128 values is itself a libgcc call, so a function that merely compares its arguments is enough to reach this. Test: `codegen_aarch64_spilled_binary128_argument_is_whole`. Found by the tests added with #C20; the defect predates that series.

- [x] **#C33 — A union was classified as an HFA of all its members at once, and the caller wrote past it.** **Fixed.** **[probed]** A union's members overlap, so it holds as many floating elements as its *largest* member does — AAPCS64 5.9.5 takes the maximum. The HFA walk summed them, so `union { double v; double d; }`, eight bytes, was classified a **two**-element HFA: the callee read sixteen bytes out of an eight-byte object and the caller wrote sixteen back into an eight-byte slot, over whatever followed it on the frame. gcc emits `fmov d0, 3.25e+0; ret` for it; c17 emitted `ldr d0, [x0]; ldr d1, [x0, #8]`, and the caller `str d0, [x29, #192]; str d1, [x29, #200]`.

  Only Apple arm64 showed it, and the reason is the type mapping: there `long double` **is** `double`, so `union { long double v; double d; }` is exactly that shape. On aarch64 Linux the same union is sixteen bytes with two different bases — binary128 and double — so it is not an HFA at all and never faulted. The damage was to the caller's frame, so it killed the process rather than returning a wrong value. Tests: `a_union_is_an_hfa_of_its_largest_member` and `codegen_aarch64_union_hfa_counts_overlapping_members_once`, both of which also pin that a *struct* of two doubles really is two elements. Found by the tests added with #C21.

- [x] **#C32 — aarch64 did not recognise HFAs with array members, or half precision.** **Fixed.** **[probed]** The member walk recursed into a nested struct but not into an array member, and `try_classify_hfa`'s array arm was reachable only for a top-level array type, which C never forms — so `struct { float v[1]; }` was rejected as holding a non-floating field. `_Float16` was not among the accepted base types either, though AAPCS64 admits half precision. Both went back in a general register where gcc uses V0. Wider than filed: every array-member shape was affected, `struct { float v[1]; }` and `struct { double v[2]; }` as much as the `long double` one. `HfaBase` gained a `Float16` variant — the right shape here precisely because every match on it is exhaustive, so the compiler named all six sites; `__float128` joined the base types, and half-precision complex now classifies like the other widths. Test: `codegen_aarch64_hfa_accepts_arrays_and_half_precision`.

- [x] **#C25 — A `long double _Complex` parameter advanced the argument cursor by half its size.** **Fixed.** **[probed]** `void probe(long double _Complex v, long double re, long double im)` read `re=2.5 im=1.5` under c17 and `re=1.5 im=2.5` under gcc. `types.kind()` answers the *base* kind for a complex type — COMPLEX is a modifier bit, not a kind — so `long double _Complex` satisfied the allocator's `is_longdouble` test and took the sixteen-byte branch. It is COMPLEX_X87 and occupies thirty-two, so every stack parameter after one was read sixteen bytes low, landing on its imaginary half, and each later parameter was shifted another slot along. The branch written for it sits twenty lines below with the right stride and had never been reachable; both sibling sites, in `call.rs` and `codegen.rs`, already excluded complex. Filed as not reproducible at `-O2`: it is, at every level — the original probe's callee was being inlined, which removes the ABI from the question rather than the defect from the compiler. Test: `c99_complex_long_double_then_stack_scalars`.

- [x] **#C26 — A struct rvalue used as a designator yielded its value, not its address.** **Fixed.** **[probed]** `mka().v[0]` crashed for every element type. `linearize_lvalue` fell through to evaluating the expression for anything that is not a designator, and for a struct-returning call that is the result local — the value. A consumer that loads or stores folds the member offset into the addressing mode and reads the right bytes, which is why `mk().scalar_member` always worked; a consumer that does arithmetic dereferenced the value's own bits. Wider than filed: `&mka().v[1]`, letting an array member decay to a pointer, and `mkb().inner.x` at a **non-zero** offset were equally broken — offset zero worked by accident. The materialize-and-take-address logic already existed for complex operands, with a comment describing this exact hazard; it is now the shared `rvalue_addr`, which also retired an open-coded copy in the call-argument path and two duplicated "is this a designator" lists. Test: `c99_struct_rvalue_designators`.

- [x] **#C27 — A nine-to-sixteen-byte integer or mixed struct was passed as a pointer.** **Fixed.** **[probed]** System V classifies each eightbyte on its own: `struct { long a, b; }` is two general registers, `struct { double a; int b; }` an SSE register and a general one. c17 passed a pointer in a single general register. Caller and callee agreed inside a translation unit, so no program c17 compiled by itself could see it — it read garbage only against a peer that followed the ABI, which is why the regression test is an assembly probe rather than a program. The classifier was already right; one line replaced the struct type with a pointer type and made the pipeline consistently wrong. The caller's two-integer arm already existed and had never been reachable; mixed classes had no arm at all. Three accounting rules came with the register form, each wrong in a way only that form exposes: an argument that does not fit goes to memory *whole* (3.2.3 step 5) and its outgoing size was counted with `max` rather than a sum; the callee had no incoming location for that case; and a struct before an ellipsis spends every register it occupies, so counting it as one put `va_start`'s save-area index a slot out. Gated to x86-64 — `linearize.rs` is shared and aarch64 genuinely passes these as a pointer. Tests: `codegen_medium_struct_passes_in_registers`, `codegen_medium_struct_uses_two_registers`.

- [x] **#C28 — A small all-SSE struct was passed in a general register.** **Fixed.** **[probed]** The return side asks the ABI class; the argument side asked the kind and the size and treated everything of eight bytes or fewer as a general-register value, so `struct { float v; }`, `struct { float a, b; }` and the `_Float16` pair went out in RDI where gcc uses XMM0. Filed against `_Float16`; `float` was equally affected and is considerably more common. The floor that excluded them was added when the sixteen-byte cases landed, to hold behaviour steady. At this size the argument pseudo holds the *value* rather than an address, which is the difference from the sixteen-byte shapes — widening the address-based path instead made every small *integer* struct dereference its own value, which is how that distinction was found. Test: `codegen_small_sse_struct_uses_an_xmm`.

- [x] **#C29 — `__real__` and `__imag__` were not accepted.** **Fixed.** **[probed]** Both spellings of each, as rvalues and as lvalues. Almost no new machinery: a complex value already travels by address and its halves already sit at offset 0 and one base type above, so `emit_complex_to_real` was `__real__` with a conversion on the end. The lvalue form is that address plus the offset, which makes `__real__ z = v`, `__imag__ z *= 2` and `&__imag__ z` fall out of the ordinary scalar store — the target type is the *base* type, so nothing on the assignment path treats it as complex. gcc accepts both on a real operand, where `__real__ x` is `x` and `__imag__ x` is a zero of its type, including for integers; that is handled rather than rejected. Test: `c99_real_and_imag_operators`.

- [x] **#C22 — Constant folding of floating arithmetic went through `f64`.** **Fixed.** **[probed]** `eval_const_float_expr` folded a binary operator by narrowing both operands with `to_f64()`, which cost a `long double` eleven significand bits and a `__float128` sixty. `static __float128 c = 1.0q/3.0q;` emitted `3ffd5555555555555000000000000000` where gcc emits `...5555555555555555`, and `static __float128 s = 1.0q + 1e-30q;` collapsed to exactly `1.0`. The sharp edge was that the same expression as a **local** initializer is computed at run time through libgcc and was correct, so the static and automatic forms of one initializer disagreed.

  `FloatVal` now does exact add, subtract, multiply and divide over a 256-bit intermediate and rounds **once**, to nearest with ties to even, at the format of the expression's own type — which is asked for by type rather than by width, since `long double` is x87's 80-bit format on x86-64, binary128 on aarch64 Linux and plain `double` on Apple arm64, and the two 128-bit ones are indistinguishable by size. Operands are rounded to that format first, so a literal wider than the type it was written for contributes only the bits that type has. Integer and character constants convert exactly instead of through `f64`, and a cast rounds to its target for every float format rather than only `float` and `double`.

  Verified differentially against gcc on the emitted image: 2,675 folded initializers on x86-64 (`__float128`, `long double`, `double`, `float`) and 900 `long double` ones against `aarch64-linux-gnu-gcc`, where the format is binary128 — all byte-identical, concentrated on the cliffs at each format's subnormal floor and overflow ceiling. One defect was found that way and fixed: on far underflow, where half an ulp is wider than the intermediate, forming it shifted the one off the top and left zero, which every value compares greater than, so `0x9482dbp-94f * 0xb85f3dp-118f` came out as the smallest subnormal instead of none at all. Unit tests additionally check all four operations against the hardware over 20,000 random `double` and 20,000 random `float` pairs.

  Tests: `c99_constant_folding_is_exact_in_the_expressions_own_format`, and `arithmetic_rounds_once_at_the_target_precision`, `ties_at_the_last_significand_bit_go_to_even`, `underflow_rounds_rather_than_flushing`, `overflow_saturates_at_each_formats_ceiling`, `special_values_follow_ieee`, `rounding_to_a_format_agrees_with_emission`, `double_results_agree_with_hardware`, `float_results_agree_with_hardware`, `integers_convert_exactly` in `cc/float.rs`. Not a conformance question — C17 6.6p5 lets an implementation fold with less range and precision than the target format — but a surprising one. Found by code review of the `__float128` series.

- [x] **#C20 — A `_Complex` member of an automatic aggregate was silently dropped.** **Fixed.** **[probed]** `struct S { double _Complex z; }; struct S s = { 1.0 + 2.0*I };` left `s.z` as garbage under c17 and `1 + 2i` under gcc, with no diagnostic.

  The filed diagnosis pointed at `linearize_init`. That path — objects with static storage duration — was never the broken one: `ast_init_to_ir` tests the *object* type first and already routes a complex member to `complex_initializer`. The broken path is the **automatic** one, `linearize_struct_field_init` in `cc/ir/linearize_stmt.rs`, which assumes a scalar member holds its own value. A complex value lives in memory and travels by *address*, so the store wrote the address: every complex member of a local struct, union or array read back as a stack address reinterpreted as a double, about `6.9e-310`. A **real** initializer for one — `struct S b = { 3.0 };` — crashed outright, which the original report did not capture.

  Twelve shapes were affected, all through the same three call sites: a struct or union member, a nested one, a designated one, an array of complex, an array of structs holding one, a function-scope compound literal, and a braced complex scalar (`double _Complex z = {1.0};`, which takes the initializer-list path before the complex arm of a plain declaration ever runs). The two-half store that a plain complex local already used is now the helper `store_complex_at`, called from all three.

  Test: `c99_complex_members_of_automatic_aggregates_are_initialized` (all twelve shapes plus the `float` and `long double` base precisions, at -O0 and -O2, with the static and assignment forms as controls) and the unit test `test_complex_struct_member_init_stores_both_halves`. Found by the Phase 4 sweep.

- [x] **#C21 — An x87 scratch slot aliased the first local, and a struct holding a `long double` had the wrong ABI.** **Fixed.** **[probed]** Filed as two `long double`-returning calls in one variadic argument list outliving a temporary, with a separate note that returning a struct containing a `long double` yielded zero. Both diagnoses were wrong, and the second was wrong in an interesting way.

  **The corruption.** `fild` and `fld` have no register form, so an immediate or a general register has to be staged through memory on its way into the FPU. That staging address was `-(callee_saved_offset + 8)(%rbp)` — an address nothing had reserved. Slot offsets start at zero, so it landed squarely on the first local; for a `long double` first local it overwrote bytes 8 and 9, the sign and exponent, turning `1.0L` into `2^-16382`, which prints as `0.0`. It is not about two calls, or about a variadic list, or about a temporary: **one** int-to-`long double` conversion is enough and no call is needed at all. The region is now reserved by the allocator and reached through a single accessor. Test: `codegen_x87_scratch_does_not_clobber_a_live_local`, pinned to `-O0` because at `-O` the folder turns the conversions into constants and the staging path is never taken — which is why the existing `long double` codegen tests, whose values come from `.rodata` rather than from an integer, never saw it.

  **The struct.** The value really was zero, but the fix is not a hidden pointer. System V classifies the two eightbytes of `struct R { long double v; }` as X87 and X87UP, and X87UP is preceded by X87, so the merge-to-MEMORY rule does not fire: gcc emits `fld1; ret`. c17 decided by raw size, and 128 bits is not *greater* than 128, so it took the two-register path — the callee returned RAX:RDX, the caller assumed sret had already happened and stored nothing. As an **argument** the class genuinely is MEMORY: gcc leaves sixteen bytes on the stack, where c17 passed a pointer in RDI. Caller and callee agreed inside one translation unit, which is why running a program could never catch that half; it disagreed with every gcc-compiled peer.

  `sole_scalar_content` answers what an aggregate is made of, seeing through one-member structs and unions and one-element arrays. The return arm asks it; the parameter arm does not. The hidden-pointer test is now the union of the old size rule and the classifier rather than a replacement, which is what keeps aarch64 bit-identical: an HFA over sixteen bytes classifies `Hfa`, never `Indirect`, and nothing implements a three-register HFA return. Such a `Ret` carries an address rather than a value, exactly like a complex one, so the inliner flag — renamed `ret_is_address` for what it means — had to cover it too; missing that would have been a miscompile visible only at `-O`.

  All five shapes (`struct`, nested, one-element array, `union`, and `union { long double v; double d; }`, which merges X87 with SSE and really is MEMORY) now agree with gcc in both directions across a real object boundary. Tests: `codegen_long_double_aggregate_returns_in_st0`, `codegen_memory_class_struct_arrives_by_value`, and classifier unit tests in `cc/abi/sysv_amd64.rs`. Found by the Phase 4 sweep.

- [x] **#C17 — A bitfield narrower than `int` did not sign-extend on read.** **Fixed.** **[probed]** `struct C { signed char c:3; }; c.c = -4;` reads back as `4` under c17 and `-4` under gcc — a silent wrong value in ordinary code. `int a:4` is correct, so the defect is specific to a bitfield whose declared type is narrower than `int`; the read appears to extend from the declared type's width rather than the field's. The extension shifted by `storage_unit_bits - width`, but the extraction runs in a register of the *promoted* width, so for a one-byte unit it left the field's top bit at bit 7 — not the sign bit — and the arithmetic shift back saw a positive value. `int a:4` was correct only because its storage unit is 32 bits, which happens to equal the operation width. Test: `c99_signed_bitfields_sign_extend_at_every_declared_width`, covering every signed declared type at its extremes, `-1`, increment across the boundary, unsigned controls, and use in an expression. Found by the Phase F sweep.

- [x] **#C18 — Struct layout is wrong when bitfields mix with other members.** **Fixed.** **[probed]** `struct D { char x; unsigned a:1; char y; }` was 4 bytes under gcc and **12** under c17; `struct C { int a:4; unsigned b:4; signed char c:3; }` was 4 under gcc and 8 under c17. Both are ABI-visible: a struct passed between a c17 object and a gcc one was laid out differently, so this was worse than a size mismatch.

  `compute_struct_layout` tracked a byte `offset` plus one open storage unit, and opened a new unit whenever `current_storage_unit_size != storage_size` — so two bitfields of different declared types never shared a unit. After a plain member it reset the unit and re-aligned the *byte* offset to the bitfield's declared type, which is what pushed `struct D` to 12.

  The System V ABI does not work in units-and-bytes: it allocates from a running **bit** offset measured from the start of the struct. A bitfield takes the next free bits; its declared type contributes the struct's alignment and the size of the window the field may not straddle, but never an allocation of its own. `compute_struct_layout` is now that model, and the whole `/tmp/bf.sh` matrix agrees with gcc: same-type packing, cross-unit, zero-width, `long long`, every narrow type, both orders of plain-member/bitfield, and nine cross-type pairings the original finding did not name (`unsigned a:12; unsigned char b:4;` and `unsigned short a:9; unsigned b:9;` were 8 bytes against gcc's 4).

  Two consequences had to be fixed with it, neither visible until the layout moved:

  - **Static initialization.** A bitfield's access window is `sizeof(T)` wide and aligned, so it now routinely spans bytes belonging to other members — `unsigned a:1` after a `char` sits at bit 8 of a window based at byte 0. The initializer packed one whole window per byte offset, which both blanked those neighbours and, where two fields sharing a byte had *different* window sizes (`unsigned a:12, b:12` takes a two-byte window and a four-byte one), emitted only the first field's window and dropped the rest. Bitfields are now merged into a byte map covering exactly the bytes their own bits reach, and the duplicate-initializer rule compares byte spans rather than byte offsets.
  - **Union members.** `compute_union_layout` never recorded a bitfield *as* one, so the accessors read and wrote the full declared type: `union { int a:4; unsigned b; }` with `b` set to 15 read `a` back as 15 where gcc gives -1.

  Test: `c99_bitfields_allocate_at_the_next_free_bit` — sizes, alignments and offsets for seven shapes, value round trips that check each field's neighbours survive the read-modify-write of a store, the union case, and static images verified both field by field and byte by byte.

  One divergence is left and is recorded in `cc/doc/TODO.md`: bitfields ignore `__attribute__((packed))`, since packing them to the bit lets a field straddle its window and the accessors cannot address an unaligned span. Found by the Phase F sweep.

- [x] **#C15 — Conflicting type specifiers were accepted, and named whichever type came last.** **Fixed** for the type-specifier half; see below for what remains. Filed as "a declaration with no declarator is accepted where gcc errors" and marked Minor on the grounds that "no symbol is emitted in any of these, so nothing is miscompiled". That was the smaller half of it, and the assessment was wrong.

  **[probed]** The same defect reaches declarations that *do* have a declarator, at every scope. `parse_type_specifier_inner` accumulated specifiers by overwriting `base_kind`, with no check against C17 6.7.2p2's list of admissible combinations, so an impossible one silently named whichever type came last: **`float double x;` was a `double`, `void int y;` an object of type void with a size of 4, and `long long long z;` a `long long`** — all accepted with no diagnostic, in file scope, block scope and struct members alike. That is a wrong program, not a missing diagnostic.

  A `SpecifierTally` now records the data-type specifiers in source order and counts `short`/`long`/`signed`/`unsigned` separately (the valid combinations pair those with a data type rather than naming one), and checks the list once it is complete. All fifteen combinations gcc rejects are now rejected, and all thirteen it admits — including the ones that look like duplicates, `long unsigned int`, `signed long long`, `long double _Complex` — still compile.

  Fixing it exposed a collision the old behaviour had been hiding: c17 treats `_Float32`/`_Float64`/`_Float16`/`__int128_t`/`__uint128_t` as keywords, but glibc's `<bits/floatn-common.h>` writes `typedef float _Float32;` whenever the compiler does not claim native support. That is two data types under the new check. Those five alias spellings now yield to the declarator position once a data type has been given, which is the rule `typeof` already needed for the same reason — and it is why the typedef used to "work": the declaration parsed as *two specifiers and no declarator*, which is exactly the empty declaration this finding was originally about.

  Tests: `diagnostics_conflicting_type_specifiers_are_rejected`.

  **Still open, Minor — the empty-declaration diagnostics proper.** `int register;` and `int inline;` are accepted where gcc errors ("'register' in file-scope empty declaration"), as is `int __asm__;`. In the other direction `static;`, `extern;` and `const;` are *rejected*, where gcc warns and continues — conforming either way, since C17 6.7p2 makes them a constraint violation and both a warning and an error are diagnostics, but c17's message is wrong: it says "type specifier missing; implicit 'int' was removed in C99" when the real complaint is that the declaration declares nothing.

- [x] **#C16 — `alignof`, `offsetof` and `setjmp` cannot be *used* as ordinary identifiers.** **Fixed.** **[probed]** all three declared fine (`int offsetof;` was accepted) but failed in expression position: `offsetof = 1;` gave "expected '('" and `alignof = 1;` was a parse error. gcc accepts all three in C17 mode, since none is a C17 keyword — `offsetof` is a macro, `alignof` a C23 spelling, `setjmp` and `longjmp` ordinary library functions. The expression parser recognised the names ahead of ordinary identifier lookup, in both `parse_primary_expr` and the `_Alignof` arm of `parse_unary_expr`.

  A new `builtin_is_shadowed` consults the ordinary namespace first. `setjmp` and `longjmp` are the exception, and only against a *function* declaration: `<setjmp.h>` declares exactly those and they need code generation an ordinary call cannot produce, so only a non-function declaration displaces them. The reserved spellings (`__builtin_*`, `_Alignof`, `__alignof__`) are never displaced — C17 7.1.3 reserves them to the implementation in every scope. Tests: `diagnostics_shadowable_builtins_yield_to_a_declaration`, covering a local, a parameter, a file-scope object taken by address, a function definition of the same name, `<setjmp.h>` still working, and the builtin meaning still applying when nothing has claimed the name. Distinct from #C10, which is about the declarator position only.

- [x] **#C11 — Constant folding in global initializers was full of holes.** **Fixed.** Filed as "a `_Complex` object with static storage duration cannot be initialized", which was true but was the smallest part of it. **[probed]** `double a = 1.0 + 2.0;` was *rejected*: the `+`/`-` arm is the pointer-arithmetic arm, and when neither operand is a pointer it fell back to an integer-only evaluator, while `*` and `/` reached the catch-all and folded correctly. Worse, `double a = -(1.0 + 2.0);` was *accepted and silently became 0.0* — the negation arm returned "no initializer" with no diagnostic, which lands the object in `.bss`. Both arms now use the same integer-then-float ladder the catch-all already had. `_Complex` gained real constant folding (`__builtin_complex`, `CMPLX`, `1.0 + 2.0*I`, and complex `+ - * /`), emitted as the two-field `Initializer::Struct` the layout already describes. A string literal can now carry an offset (`const char *p = "hello" + 1;`), which needed interning the literal — something the `&self` address evaluator could not do. Both smaller faults are fixed too: the diagnostic no longer prints a raw Rust `Debug` dump of the AST, and it reports the expression's own position instead of `:0`. Tests: `c99_global_initializer_folds_floating_arithmetic`, `..._folds_pointer_arithmetic`, `..._accepts_complex`.

  One claim in the original entry was wrong and is worth recording: `double _Complex z = {1.0, 2.0};` was thought to drop the imaginary part. It should. A complex type is a *scalar* type (C11 6.2.5p21), so that is a braced scalar initializer with an excess element; gcc warns and keeps only the first, and c17 now matches.

- [ ] **#C34 — aarch64 tells the caller's frame from the callee's by the sign of an offset.** **Open, Minor — the hazard is contained, the `Loc` split is not done.** x86-64 has a `Loc::IncomingArg(i32)` distinct from `Loc::Stack(i32)`; aarch64 has only the latter, and a *positive* offset means an incoming stack argument while a *negative* one means a local or spill slot. Two address spaces shared one integer type, so nothing stopped one being assigned to the other, and it had already caused one regression. Three things have since been done about it. (1) The producers are typed: `LocalSlot` and `IncomingOff` apply their own sign convention inside their constructors, and `SpilledArg.to_stack_offset` — the field the regression actually corrupted, and which a `Loc` variant would never have covered — holds a `LocalSlot`. (2) Twenty-two emitters that computed a frame address themselves now go through `stack_mem`/`stack_mem_plus`; eight of them were using `frame_size + off`, which is wrong by the register save area in a variadic function, wrong for an over-aligned frame, and short by sixteen bytes for an incoming argument. (3) Only four sign tests remain outside the accessors. What is left is the `Loc::IncomingArg` variant itself. It is deferred rather than forgotten: of the 66 `Loc::Stack` sites on this target, nearly all are `if let` or `match` with a `_` arm, so adding a variant produces almost no compile errors and each missed site would emit *nothing* — worse than today's wrong address. Doing it safely means giving the accessors a typed parameter first, so the split is checked at every call site rather than at the few that happen to match exhaustively. Found while fixing #C33.

- [x] **#C35 — An over-aligned frame's base register was allocatable.** **Fixed.** **[probed]** A local whose alignment exceeds the sixteen bytes the stack guarantees cannot sit at a fixed displacement from the frame pointer, so aarch64 rounds the pointer up into X19 and addresses locals from there — while leaving X19 in `Reg::allocatable()`. The colorer prefers callee-saved registers for values live across a call, and X19 is one, so the prologue's base was overwritten by an ordinary integer and every later local access dereferenced it. Three lines of C with an `_Alignas(32)` array segfault at `-O0`. Withholding it was impossible before: the decision came from `max_local_align`, discovered *during* allocation, long after the pool was seeded. `FrameBase` now decides it up front from the function's declared locals — the only place over-alignment can originate — and carries the register and the alignment as one value. Removing it from `free_regs` was also not enough: the chordal colorer builds its preference orders straight from `Reg::allocatable()`, so all four consumers now go through a per-function `allocatable_regs`. Test: `codegen_over_aligned_frame_base_reserved`. Found while probing #C34's addressing.

- [x] **#C36 — x86-64 mis-aligned every over-aligned local when the callee-saved count was odd.** **Fixed.** **[probed]** x86-64 realigns `%rsp` rather than claiming a register, so it has no #C35 hazard, but it had the same defect by another route: a local resolves to `%rsp + (alloc_size - slot)`, the `andq` guarantees `%rsp` is a multiple of the alignment and the allocator guarantees the slot is, but `alloc_size` is a 16-byte-rounded total *less eight bytes per pushed register*. With five callee-saved pushes, `_Alignas(64)` yielded a 64-byte object at an address ≡ 8. Rounding `alloc_size` to the alignment fixes it. Same test as #C35, which fails on both targets without its fix.

- [x] **#C37 — aarch64 passed and returned an HFA in two registers whatever its element count.** **Fixed.** **[probed]** AAPCS64 §5.4.2 gives a homogeneous floating-point aggregate one V register per element, for one through four. Both sides of the call, and both sides of the return, asked the ABI for `Hfa { count: 2, .. }` specifically; everything else fell through to the general-register path. `struct { float a, b, c, d; }` went out whole in one X register while the callee read V0-V3, and it consumed an integer slot, so the *next* integer argument moved too. Eight of twelve parameter shapes disagreed with gcc. The return side had three layers to reconcile: the linearizer claimed every aggregate over sixteen bytes for the hidden pointer, on the recorded grounds that "nothing implements a three-register HFA return"; the emitter handled one element and two and capped the path at sixteen bytes, excluding four `double`s at thirty-two; and the caller's landing local was allocated only at sixteen bytes or fewer, so a twenty-four-byte result had nowhere to go. Verified by splitting the objects — gcc's caller against c17's callee and the reverse — over all shapes, plus arguments overflowing V0-V7. Tests: `codegen_hfa_param_element_counts`, `codegen_hfa_return_element_counts`. Found while probing #C34's addressing.

- [x] **#C38a — A stacked aarch64 HFA was written as if it were a `_Complex`.** **Fixed.** **[probed]** An HFA that does not fit in the remaining V registers is laid on the stack by the caller. Every multi-element argument took the `_Complex` path to do it: `complex_fp_info` for the element size and a fixed two-element loop. For a three- or four-element HFA that wrote the wrong number of elements at the wrong stride, and `slot_bytes` reserved the wrong number of bytes, so the argument after it landed inside it. `StackArg`'s `complex_pair: bool` became a `StackKind` carrying `Hfa { base, count }`, and the element load is now shared with the register path rather than spelled out at each site. Test: `codegen_stacked_hfa_element_counts`, which fails on the parent commit. Reported by review on the #C37 change.

- [x] **#C38 — An inlined function with a register-sized aggregate parameter crashes.** **Fixed.** **[probed]** The shape recorded here previously (`F4, F4, D2`) does *not* reproduce; it returns the right value on both targets at every optimization level. What reproduces is an aggregate of **floats** rather than doubles:

  ```c
  typedef struct { float a, b; } F2;
  typedef struct { double a, b, c, d; } D4;
  static double f(D4 p, D4 q, F2 r) { return p.a + q.a + r.a + r.b; }   /* SIGSEGV */
  ```

  aarch64 at `-O2` only — `-O0` is fine, `noinline` is fine, x86-64 is fine, and `aarch64-linux-gnu-gcc` is fine. That combination is what made it look like an ABI defect for so long; it is an inliner defect, and the ABI is what makes the two shapes differ.

  **An aggregate that fits in one register travels *as* its value.** A larger one travels by address. `implicit_param_copies` — the copies that stand in for the backend prologue when a callee is inlined — always loaded *through* the argument pseudo, so `struct { float a, b; }` became a wild pointer spelled by two floats and the inlined body segfaulted on its first read. The `D4`s in the same call are thirty-two bytes, travel by address, and were always right. The copy now stores the value directly when the aggregate is one, two, four, or eight bytes wide. Test: `codegen_inlined_register_sized_aggregate_param`, which covers three failing shapes and three that always worked.

  This is the same value-versus-address distinction as the struct-rvalue-as-call-argument defect and the two-SSE `LEA`/`MOV` one. Three separate sites have now had it; the representation does not carry which of the two a pseudo holds, so each place that consumes an aggregate pseudo has to rederive it from the size.

- [x] **#C61 — An argument register spent by a parameter the prologue never stores, on x86-64.** **Fixed.** **[probed]** Found while verifying #C38's regression test, and unrelated to it: `g(F2 a, D2 f)` returned 41 rather than 19, and `g(MIX unused, D2 f)` failed at `-O` only. Which register an argument arrives in is a property of the *signature* — every parameter ahead of it spends its registers whether or not the body ever reads it — and `store_args_to_stack` charged for a parameter only along the paths where it also had a store to emit. Three ways to reach the same wrong answer, all of them making a later floating-point argument read a register one too low, sometimes one the earlier argument was still sitting in:

  - The counting-only path for a **spilled** parameter asked the aggregate's *size* (`> 64 bits`) rather than its class, so an eight-byte all-float struct — one whole XMM — was tallied against the general registers.
  - A parameter whose pseudo did not survive `-O` was **never visited at all**: the walk iterates surviving pseudos, and an unread parameter has none.
  - `store_reg_pair_param_to_local` returned early when the local was gone, **before** advancing the counters — the `MIX` case above.

  The tally is now one helper applied to every parameter on every path, and the register-pair store spends its registers before it looks for somewhere to put them. Test: `codegen_sse_aggregate_advances_fp_arg_count`, eight signatures across the three routes. Values checked against `gcc -std=c17`; only `-O0` was correct beforehand for two of the three.

- [x] **#C39 — `va_arg` of an aggregate read from the wrong place, on both targets.** **Fixed.** **[probed]** Three defects with one shape: the compiler knew how an aggregate *arrives* and read it from somewhere else.

  On **aarch64**, an HFA arrives in the SIMD registers, so `va_arg` must read it from *their* save area, one element per 16-byte slot. Aggregates took the general-register path unconditionally — "as they always have", as the code said. That agreed with a caller which also sent them in general registers, and with nothing else; correcting the caller in #C37 made the two halves of a single c17-compiled program disagree. AAPCS64 has three aggregate shapes and only one was handled: an HFA (SIMD area), a composite of at most sixteen bytes (consecutive general slots), and one larger than sixteen bytes, which stage B.4 passes as a *pointer* to a caller-made copy. `VaAggKind` names all three. The save area and the stack space elements differently — 16 bytes apart versus packed at their own stride — so the copy selects a stride using the comparison already made for the address, rather than branching.

  On **x86-64**, every aggregate fell to `emit_va_arg_int`, which pulls the type's whole width from the general save area as one value. For anything the classifier put in SSE registers that is unrelated data: `struct { float a, b, c, d; }` arrives in `xmm0`/`xmm1` and came back as `-1.1e38`. It never worked. An aggregate in registers is *not* contiguous in the save area — its eightbytes come from the general and SSE areas independently, and those advance by 8 and 16 — so each is now fetched from the area its own class names and packed at the destination.

  Underneath both: **an aggregate wider than a register had nowhere to live.** The linearizer gave `va_arg` an ordinary pseudo, so the backend was handed a register holding whatever happened to be there and treated it as the destination's address. It now gets a local, the arrangement a call returning an aggregate in registers already uses. That is also what #C40 was — with several such results live at once, one function's worth of luck ran out. Tests: `codegen_variadic_hfa_argument`, `codegen_variadic_aggregate_results_coexist`. Reported by review on #C37.

- [x] **#C40 — Four variadic aggregate calls in one function segfaulted on aarch64.** **Fixed** as part of #C39 — same root cause. Any three shapes ran, a fourth did not, and each was correct in isolation, because the result of an aggregate `va_arg` was a register pseudo with no storage behind it: whether it survived depended on register pressure, which is why it looked cumulative rather than shape-specific.

- [x] **#C41 — aarch64 passed a composite of at most sixteen bytes by pointer, not in registers.** **Fixed.** **[probed]** AAPCS64 §5.4.2 stage C.10 puts a composite of sixteen bytes or fewer in consecutive X registers. c17's aarch64 caller handed over its *address* and its callee read one, so c17 agreed with itself and nothing in the suite noticed — while every call across a c17/gcc boundary was wrong in both directions: `struct { long a, b; }` by value gave garbage with a c17 caller and **segfaulted** with a gcc caller, which read the aggregate's first eight bytes as a pointer and dereferenced them. Four and eight byte composites were already right; the broken range was exactly the two-eightbyte one. The linearizer's `reg_pair` test was gated `arch == X86_64` and said so in a comment; it now applies to both. Four things had to follow it: the caller loads the pair out of the aggregate's address, the callee's prologue writes the pair into the parameter's local (and copies from the incoming slot when the registers ran out, the same gap #H13 was for HFAs), the allocator reserves *two* registers and advances by two — ordered after the `__int128` arm, which classifies identically but wants its own sixteen-byte slot — and `va_start` counts two, or the first variadic argument of a function with such a named parameter came back as that parameter's upper half. A composite that overflows X0-X7 is copied to the stack by value rather than pushed as a pointer. Surfaced by #C39, which made `va_arg` follow the ABI and so disagree with c17's own caller. Tests: `codegen_composite_in_two_registers`, and the sixteen-byte integer shape restored to `codegen_variadic_aggregate_results_coexist`.

- [x] **#C42a — aarch64 placed a stacked argument without honouring its alignment.** **Fixed.** **[probed]** AAPCS64 §6.4.2 stage C rounds the next stacked-argument address up to `max(8, alignof(type))` *before* placing the argument; c17 advanced by the rounded *size* only. A sixteen-byte-aligned argument therefore landed eight bytes low whenever an odd number of eight-byte slots came before it. The caller and the callee shared the error, so c17 agreed with itself and only a c17/gcc boundary showed it: a stacked `__int128` came back as 0 with a c17 caller and as the *following* argument's value with a gcc caller. `IncomingOff::take` and the caller's slot placement now both align the start, and the outgoing area is measured by walking the arguments rather than summing their sizes, since the padding between them is part of it. `AbiArg` carries the parameter's type for this: `ArgClass` has a size but not an alignment, and the two do not follow one another — `struct { long a, b; }` is sixteen bytes and eight-byte aligned, `__int128` is sixteen of each. Test: `codegen_stacked_argument_alignment`. Reported by review on #C37.

- [x] **#C42 — An `__int128` constant argument crashes the x86-64 backend.** **✓ fixed (2026-08-15).** Two defects in one. The allocator's immediate arm claimed the constant and skipped the sixteen-byte-slot test, so `f((__int128)424242)` aborted in `int128_hi_mem_loc`; `return (__int128)424242;` was the same panic at a second site that no test reached. Giving it a slot then exposed that `SetVal` emitted only into registers, so the slot was allocated and never written and the constant read back as **zero** -- a silent wrong answer in place of a crash. The slot is asked of the constant's own defining `SetVal` rather than of `int128_pseudos`, which also holds the narrow constants that merely *feed* a 128-bit instruction.

- [x] **#C43 — a stacked sixteen-byte-aligned argument was a wrong answer on both sides.** **✓ fixed (2026-08-15).** Recorded here as an alignment defect; measurement showed the value is simply lost. With seven `long`s ahead of it, a split-object matrix against gcc gave `c17`-caller/gcc-callee **0** and gcc-caller/`c17`-callee **7** -- the preceding argument -- for both `__int128` and `long double`. `__float128` and an eight-byte-aligned sixteen-byte struct were correct at the same position, which is what made the shape findable.

  Two independent causes. The *callee* advanced a running offset per argument and never rounded up to the type's alignment, and kept a **second** counter in the prologue that only the `__int128` arm advanced, so the two disagreed as soon as anything else was stacked. The *caller* pushed arguments in reverse with a single pad at the top of the area, and a push cannot express a gap *between* two arguments -- which is what an alignment boundary is. The outgoing area is now walked in parameter order and reserved once, the shape aarch64 already used.

  Reserving it once also required fixing a hazard the old code already had: with an over-aligned frame locals are addressed from `%rsp`, so moving `%rsp` invalidates every local address read afterwards.

- [x] **#C44 — A declarator could wear only one pair of parentheses.** **✓ fixed (2026-08-15).** C17 6.7.6 builds `direct-declarator` recursively through `( declarator )` and 5.2.4.1 asks for 63 levels; exactly one parsed, so `int ((q));`, `int ((*p));` and `void ((g))(void);` were rejected as "expected identifier". Two copies of the predicate deciding whether a `(` opened a grouped declarator had drifted apart, and the inline one claimed *any* identifier, so the function-type parameter `int (fn)(int)` was read as a declarator named `fn`. Consolidated onto one predicate. Found by probing the 5.2.4.1 minimums.

- [x] **#C44a — Type-names had a second, weaker declarator parser.** **✓ fixed (2026-08-15).** `try_parse_type_name` hand-rolled abstract-declarator parsing and knew exactly one shape, `(*)(params)`, so `sizeof(int (*)[3])`, `sizeof(int (**)(void))`, `sizeof(int (*[4])(void))` and the cast `(int(*)[3])0` were parse errors — types that could be declared as objects but not named. Five more copies of the idea sat in the struct, union and enum arms. All now route through `parse_declarator`. That exposed its abstract-declarator test being a whitelist of four following tokens, which a `_Generic` association's `:` is not on; whether an identifier may be absent is now a parameter (`DeclaratorName`) rather than a guess from the next token.

- [x] **#C44b — A string literal in braces did not initialize its array.** **✓ fixed (2026-08-15).** C17 6.7.9p14 allows the string initializing a character array to be enclosed in braces, so `char b[] = {"hi"}` is `char[3]`. It was read as an ordinary initializer list: size 1, contents never copied. `(char[]){"hi"}` was an empty string, and an explicitly sized `char c[6] = {"hi"}` had the right size with the wrong contents. Both the size deduction and the store path had the look-through one level down, for `char names[3][4] = {"Sun"}`, and neither had it at the outermost level. A silent wrong value, not a missing diagnostic.

- [x] **#C45 — `int f(void)` and `int f()` were the same type.** **✓ fixed (2026-08-15).** C17 6.7.6.3p14 makes an empty identifier list supply no information about the parameters while `(void)` says there are none; both interned as an empty parameter vector. That cost a diagnostic in one direction and produced a wrong one in the other: a call to `int f(void)` carrying arguments went unchecked (6.5.2.2p2), while a call to a K&R definition *was* checked though 6.5.2.2p1 forbids it — `int f(a,b) int a,b; {...}` called as `f(1)` was rejected where gcc accepts it. `params: None` now carries the distinction; the type key had to change first, since it flattened `None` and `Some(vec![])` to one key and would have discarded the marker at birth. Closes the #L6 residual.

- [x] **#C46 — Assignment and the increment operators took any operand.** **✓ fixed (2026-08-15).** 6.5.16p2 and 6.5.3.1p1 require a modifiable lvalue and 6.5.3.2p1 an addressable object; nothing checked. `a+b = 3`, `(a+1)++`, `(int)a = 2`, `f() = 1`, one array assigned to another, and `&` on a `register` variable all compiled. There was no lvalue predicate anywhere to check against.

- [x] **#C47 — Nothing checked that a value could become the type it was assigned to.** **✓ fixed (2026-08-15).** 6.5.16.1 constrains simple assignment and defines `return` and argument passing as conversion "as if by assignment"; none of the three checked anything, so `int *p; p = 1.5;` compiled to a `cvttsd2si` and left the pointer holding 1 — a silent miscompile. Severity follows gcc exactly. One relaxation is deliberate: glibc declares the socket calls with a `transparent_union` parameter, so an argument matching any member of a union parameter is accepted — see #C51.

- [x] **#C48 — A `void` value was a usable operand, and anything was subscriptable.** **✓ fixed (2026-08-15).** 6.5.6p2 and 6.5.15p3 require operands to have a value: `v() + 1` was arithmetic on whatever the call left behind, and `1 ? v() : 2` took whichever arm's type came first. 6.5.2.1p1 needs a pointer on one side of a subscript; `a[0]` where `a` is an `int` was given the element type `int` and indexed anyway.

- [x] **#C49 — Two declarations of one name never had to agree.** **✓ fixed (2026-08-15).** 6.7p4 requires compatible types; `SymbolTable::declare` rejects only two *definitions* at one depth and a function symbol is never marked defined, so two function declarations never collided at all. `int x; double x;` bound the second declarator to the first symbol and emitted `.comm x,4,4` — a `double` store through it runs off the object. The second silent miscompile.

  Adding the check exposed three defects beneath it, all one mistake: **`types_compatible` answered by TypeId identity where it had to answer structurally**, so two spellings of one type read as two types. (1) The base type — a storage class settles on an inner type, so `static char *x[]` was "incompatible" with a `char **` field. (2) The parameter types — every `void *(Parser *)` in CPython's generated parser conflicted with itself, 19 lines. (3) The composite — a tag *names* the type (6.2.7p1), so completing a forward declaration does not create a second one, but the incomplete and complete `CompositeType` values compared structurally, making every function declared before a struct's definition and defined after it a conflicting redeclaration. Each announced itself as a diagnostic that printed both sides identically. `Type::types_compatible` now decides only the shape and is private as `compatible_ignoring_base`; the table-level function recurses. This is the same family `cc/audit.md` already recorded being fixed once at the call-compatibility site — the comparison underneath was never fixed.

- [ ] **#C55 — Trigraphs are off by default.** **Open, Minor — POSIX conformance, deliberate.** The c17 APPLICATION USAGE says it outright (88224): "Some c17 compilers *not conforming to POSIX.1-2024* do not support trigraphs by default." #P11 implemented them behind `--trigraphs` because replacement reaches inside string literals, so `"What??!"` becomes `"What|"` — which is exactly what C17 phase 1 specifies and what the POSIX RATIONALE laments without granting an exemption. A deliberate divergence, not a missing feature; the fix is to flip the default and offer an opt-out. Deliberately out of scope of the 2026-08-15 series.

- [x] **#C57 — an argument after a stacked `__int128` was mislaid, on both targets and by opposite rules.** **✓ fixed (2026-08-15).** Found by a test written for #C43, which asked what happens to the argument *following* the one under test -- something nothing had asked before. **aarch64**: AAPCS64 stage C.10 allocates a 128-bit value to an *even-aligned* register pair, so an odd NGRN skips a register; with five leading `long`s c17 used x5/x6 where gcc uses x6/x7. Stage C.11 then sets NGRN to 8 when an argument does not fit; c17 left it, so the next argument took x7, which the callee was not reading. Three places computed the pair independently and now share one helper. **x86-64**: the opposite rule. SysV 3.2.3 step 5 sends an overflowing argument to memory *whole*, consuming none of the registers it did not fit in; three counters advanced anyway, so the callee expected the trailing argument on the stack while the caller correctly passed it in `%r9`. The two ABIs genuinely disagree here and are deliberately not shared.

- [x] **#C58 — a universal character name was stored as a byte, and the wide encodings were each wrong about a different input.** **✓ fixed (2026-08-15).** C17 5.1.1.2 converts a UCN to the execution character set. The escape parser returned it as a `char` like every other escape, and a narrow literal holds one `char` per *byte*, so `"caf\u00e9"` was five bytes of Latin-1 where gcc gives six of UTF-8 -- wrong bytes and a `sizeof` off by one.

  Fixing that alone breaks the wide encodings, which had the opposite halves right, and decoding bytes back merges `L"\xc3\xa9"`, which C says is two elements because the program named two byte values. The parser sees the literal before escapes are resolved, so a byte of source text and a byte an escape named arrive by different routes; they are different cases of one type now, and each encoding asks for what it needs. All sixteen combinations of four encodings against raw source, a UCN, a byte escape and a non-BMP code point agree with gcc.

  A `char` array *member* initialized from a string literal had the same confusion one layer down: written with Rust's UTF-8 encoding while its terminator was placed by counting characters, so a byte at or above 0x80 ran one long and put the NUL inside the data.

- [x] **#C51 — `__attribute__((transparent_union))` is not implemented.** **Superseded by #C59.** Still unimplemented; the argument check's accommodation for it stands.

- [x] **#C59 — unimplemented attributes were dropped in silence, and `__has_attribute` disagreed with the compiler.** **✓ fixed (2026-08-15).** `vector_size` is an error (ignoring it leaves the type scalar and cannot produce a correct program); anything else unrecognised is a warning. `mode` was meant to be an error too and cannot be -- glibc declares `register_t` with it in `<sys/types.h>` -- so it warns and names the gap. Recognition came from two hardcoded lists that fed `__has_attribute` and that the parser never consulted; they had drifted both ways, answering 0 for `ms_abi` and `gnu_inline`, which the compiler honours, and 1 for four it ignored. One table now, and `-Wno-attributes` for the noise.

  `weak`, `section`, `visibility` and `used` are the four it answered 1 for and ignored; three now reach the object file for variables and functions alike. **`used` is satisfied vacuously**: c17 never prunes an unreferenced static, where gcc does at `-O2`, so the attribute has nothing to do. If dead-global elimination lands, `used` must be consulted then. `doc/ATTR.md`'s claim that every attribute whose absence a program could observe was implemented is now true.

- [x] **#C63 — Symbol attributes leaked onto the next declaration.** **Fixed.** **[probed]** `parse_external_decl` clears the pending alignment, function attributes and asm label but not the pending *symbol* attributes. A function **definition** takes its attributes through the function-attribute path and leaves these behind, so the next declaration to build an `InitDeclarator` claimed them: the variable after a `section(".mytext")` function was emitted into that function's section, and the conflicting `"ax"`/`"aw"` flags made the assembler reject the file — `Error: changed section attributes`. `codegen_symbol_attributes_reach_the_object_file` missed it only because it declares every variable *before* the attributed functions.

- [x] **#C64 — `%rsp`-relative locals were read at the wrong offset while the outgoing argument area was reserved.** **Fixed.** **[probed]** A frame with an over-aligned local addresses locals through `%rsp`, and `push_stack_args` accounts for the reservation with `rsp_adjust`. It cleared that as soon as it had written the stacked arguments — but `%rsp` stays lowered until after the call, and `save_clobbered_arg_regs` and `setup_register_args` run in between. Every register argument was read one slot off: eight `long`s through an over-aligned array summed to 38 rather than 36. The adjustment now ends where the stack is actually restored. Test: `codegen_rsp_relative_locals_across_stacked_args`.

- [x] **#C65 — A zero-initialized definition lost `weak` and its visibility.** **Fixed.** **[probed]** The `.comm`/`.bss` fast path returns before those directives are emitted, and a common symbol carries neither. `__attribute__((visibility("hidden"))) int x;` reached the object file as `GLOBAL DEFAULT COM` where gcc gives `GLOBAL HIDDEN`; a hidden variable escaping as default visibility is an interposition change, not a cosmetic one. A named section was already excluded from that path for the same reason; these two now are as well. Test: `codegen_zero_init_keeps_weak_and_visibility`, checked against `readelf -sW` on gcc's output.

- [x] **#C66 — `weak` on a declaration with no definition was dropped.** **Fixed.** **[probed]** Two halves. The parser collected only the *function* attributes after a declarator — three sites, all of them next to a `pending_fn_attrs.merge` — so `extern int f(void) __attribute__((weak));` lost the attribute before it reached an `InitDeclarator`. And the linearizer had nowhere to put it: an undefined symbol has no definition to hang directives on, so `Module::declared_symbol_attrs` now carries them and both backends emit them standalone. The canonical idiom — `if (f) f();` against a possibly-absent definition — failed to link rather than resolving to null. Test: `codegen_symbol_attributes_do_not_leak_or_vanish`, covering all four spellings and positions, of which only one was broken.

- [x] **#C67 — Read-only data in a named section was writable.** **Fixed.** **[probed]** ELF section flags were chosen from code-versus-data alone, so `section(".rodata.mine") const int` came out `"aw"`; gcc emits `"a"`. `Directive::NamedSection` now carries writability alongside executability, and const data without relocations is neither. Test: `codegen_named_section_flags_follow_constness`.

- [x] **#C68 — `__attribute__((const))` warned.** **Fixed.** **[probed]** `kw.rs` tagged `__const__` and `__const` as recognised attribute names but not the bare `const` spelling, which gcc also accepts. Hand-written code using the short form got a spurious "attribute directive ignored", fatal under `-Werror`; glibc uses the underscore form, which is why the header sweep did not show it.

- [x] **#C69 — Over-aligned floating-point locals were addressed through the wrong base register.** **Fixed.** **[probed]** Found while writing #C64's test, and a much larger defect than the one that led to it. A frame whose locals are over-aligned does `andq $-64, %rsp` and addresses locals through `%rsp`, because the fixed relationship to `%rbp` is gone. `stack_mem` exists to choose the base — and the floating-point paths spelled the `%rbp` case out longhand in **eleven** places, every one of them losing the `%rsp` case. `__attribute__((aligned(64))) double w[10] = {1,...,10};` was zeroed at its real address and initialized somewhere else entirely: `w[0]` read back as 36. `long` was always fine, and so was `aligned(16)` — it takes an alignment past the natural one *and* a floating-point type to reach these paths, which is why nothing had. All eleven now call `stack_mem`. Test: `codegen_overaligned_fp_locals`.

- [x] **#C70 — Truncating a 128-bit value to 32 bits kept the discarded half, on aarch64.** **Fixed.** **[probed]** `emit_int128_trunc` took the low half into a whole X register and called 32 bits "already the right width" — true only of the *store* that usually follows a truncation. Anything that read the pseudo directly saw the bits the truncation was supposed to drop. `__builtin_add_overflow` is exactly that shape (compute wide, narrow, widen back, compare), so it compared its result against an untruncated copy of itself and could **never** report an overflow: `__builtin_add_overflow(UINT_MAX, 1u, &ur)` said no. The generic `Opcode::Trunc` path had the same hole, with the same comment. Both now emit the 32-bit self-move that x86-64 always did. Found by CI on **both** aarch64 targets, which was the first run to include the overflow builtins. Test: `codegen_trunc_to_32_discards_the_upper_half`, which fails on a rebuilt parent.

- [x] **#C71 — `.weak` was emitted on Mach-O, where it is not a directive.** **Fixed.** **[probed]** The macOS assembler rejects it outright as unknown. Mach-O separates the two sides: `.weak_definition` for a definition, `.weak_reference` for a reference to a symbol that may be absent — and `.weak_definition` does not export, so the `.globl` that `.weak` stands in for on ELF still has to be emitted alongside it. `Directive::Weak` now carries a `WeakKind`, which is the same shape the named-section flags took. Only surfaced now because the pre-existing symbol-attribute test is ELF-gated and the declaration-side emission was new.

- [x] **#C73 — The UCN test never tested a UCN.** **Fixed.** Reported by review. `c99_universal_character_names_use_the_execution_encoding` was written to assert that both spellings of a character agree, and every literal in it was the raw source character — the `\u00e9` half was never written, so each pair compared the same input twice. That is the one thing the test existed to check: #C58 was two defects, and the compiler was once correct for one spelling and wrong for the other, so a test built from a single spelling would have passed throughout. Both spellings are now present for all six shapes, and the astral character too. Proven by reintroducing #C58 in each half separately: breaking the narrow UCN path makes it return 2 and breaking the wide one makes it return 9, where before both left it passing.

- [x] **#C72 — Three tests were not portable to macOS aarch64.** **Fixed.** Not compiler defects, but they broke the macOS build: `<uchar.h>` does not exist there (the test needs only the two typedefs); `__float128` does not exist on a target whose `long double` is a `double`, so the control is now behind `#ifdef __SIZEOF_FLOAT128__`; and an ELF section name is not a Mach-O one, so that half is ELF-gated. The weak-declaration test now asserts the *directive* on every target — what c17 emits is c17's business — and runs the link only where the linker's policy on unresolved weak symbols is known.

- [x] **#C74 — A 128-bit multiply read an operand the multiply had already destroyed, on x86-64.** **Fixed.** **[probed]** `expand_int128_mul` builds the product as `lo = a_lo*b_lo`, `hi = umulhi(a_lo,b_lo) + a_lo*b_hi + a_hi*b_lo`, which is correct — aarch64 uses the same expansion and was never wrong. On x86-64 the `umulhi` is `mulq`, which takes an operand in `%rax` and writes the whole product back over `%rdx:%rax`, so an `a_lo` allocated to `%rax` was gone by the time the cross term `a_lo*b_hi` asked for it. Two causes: `opcode_constraints` declared the RAX/RDX clobber for `Opcode::Mul` but had no arm for `Opcode::UMulHi`, the other half of the same instruction; and the operand exemption — an operand of a clobbering instruction may sit in a clobbered register — applied unconditionally, where it is sound only at the ends of a live range. `ConstraintPoint::operand_survives` now states that rule once and both x86-64 sites ask it. It took an operand with a non-zero high half to show: `-1 * -1` returned hi=`0xfffffffffffffffe`, `3 * -4` hi=`0x0e`, while `-3 * 4` was right throughout because the ruined term was multiplied by zero. `__builtin_mul_overflow` computes in 128 bits and inherited it, reporting overflow for a product of 1. Test: `codegen_int128_mul_reads_operands_after_the_hardware_multiply`, which asserts the *high* half of every product — read through a narrowing cast, five of its seven cases pass against the broken compiler. Fifth instance of the operand-clobber family.

- [x] **#C75 — An integer argument was not widened to a 128-bit parameter.** **Fixed.** **[probed]** The predicate deciding whether a call argument needs converting carried `arg_size <= 32 && param_size <= 64`, written when int→long was the only integer widening there was. `param_size <= 64` excludes every 128-bit parameter and `arg_size <= 32` excludes a `long` argument on top of it, so `void f(__int128)` called with an `int` passed the value unconverted and the callee read a register pair of which one half had never been written: both halves garbage on x86-64, both zero on aarch64. Only an explicit cast worked; assignment, `return`, binary operators, initializers and comparisons were all correct, which is why the type looked supported everywhere except where it was passed. The bounds are gone rather than raised — widening is decided by the two sizes and nothing about it stops at 64. Test: `codegen_integer_argument_widens_to_a_128_bit_parameter`, asserting the high half of every argument.

- [x] **#C76 — A three-byte struct argument arrived as one byte.** **Fixed.** **[probed]** A struct small enough to travel in a register travels as its value, and `emit_move` loaded that value with a zero-extending `movz` whenever the width was under 32 bits. `movz` extends from a byte or a word and from nothing else, so a three-byte struct asked for a `Movzx` from 32 bits to 32 bits — an instruction that does not exist — and the assembly printer's `_ => "movzbl"` fallback turned the malformed instruction into a legal *one-byte* load. Swept sizes 1 through 16: 1/2/4/8 are machine widths and 5/6/7 are wide enough to skip the extending path, leaving 3 as the only size, and only as an rvalue, since a named struct is passed from its address. `extending_load_size` now answers `None` for a width with no extending form and those move at the next size up; the printer's fallback is gone, because an unencodable `movz` is a bug in whatever built it and quietly narrowing the load is the worst available response. Nothing in the suite or in a full CPython build reaches the panic that replaces it. Test: `codegen_small_struct_argument_travels_at_its_own_width`. Sixth instance of the aggregate value-vs-address family.

- [x] **#C77 — `int long` was a four-byte `long`.** **Fixed.** **[probed]** C17 6.7.2p2 gives the declaration specifiers as an unordered set, so `int long` names what `long int` names. The specifier tally settled `base_kind` on whichever arrived first and took a size only when nothing had been settled, so `long` reaching an already-`int` tally left the kind at `Int`: `sizeof(long int)` 8 against `sizeof(int long)` 4, and `short` the same. `long long` in either order was already right (the second `long` sets the kind unconditionally) and `long unsigned` was right (`unsigned` never touches the kind), so only a single size after `int` broke. There are **two** copies of this tally — `parser.rs` for declarations, `expression.rs` for type-names — so fixing one left `long int x;` right while `sizeof(int long)` stayed wrong. Both promote an already-`int` kind now. Test: `c99_size_specifiers_are_order_independent` plus parser unit tests, covering declaration, type-name and cast paths.

- [x] **#C78 — `_Generic` never matched the `long` family.** **Fixed.** **[probed]** `_Generic(1L, long: 2, default: 0)` selected the default, and so did every spelling of `unsigned long`, `long long` and `long double`, whenever the controlling expression was a literal or the result of the usual arithmetic conversions — while the same selection on a *variable* of that type matched. The diagnostic printed both sides identically: "selector of type 'long' is not compatible with any association". A specifier list sets `TypeKind::Long` **and** a `TypeModifiers::LONG` bit; the canonical interned types, which is where a literal's type and a conversion's result come from, carry no such bit; and `compatible_ignoring_base` compared the modifier sets. `kind` is compared first and already carries the size, so the bit distinguishes nothing — `SIGNED` had been exempted for exactly this reason, with a comment about two spellings of `int16_t`, and `SHORT`/`LONG`/`LONGLONG` were missed. They join it. `__builtin_types_compatible_p(__typeof__(1L), long)` answered 0 for the same reason. A test asserts directly that nothing which should stay distinct became compatible, rather than trusting the argument. Third instance of the identity-vs-structure family (see #C57).

- [x] **#C79 — An incomplete array was sized by initializer elements, not array elements.** **Fixed.** **[probed]** `int a[][2] = {1,2,3,4}` declared four rows and `struct P d[] = {1,2,3,4}` four elements. The values landed correctly — the linearizer knows that a brace-less aggregate element consumes as many list elements as it has scalar fields (C17 6.7.9p20) — but the parser deduced the bound by counting list elements, so the object was sized at twice or three times what was filled. Nothing warned and the extra elements were zero, so it showed only as `sizeof a / sizeof a[0]` walking past the end. The rule now has one home: `TypeTable::count_scalar_fields` and `ast::is_brace_elision_candidate`, both of which the linearizer already used and the parser now asks instead of approximating. Test: `c99_brace_elision_decides_the_deduced_array_bound`.

- [x] **#C80 — An enumerator outside `int` range was truncated in silence.** **Fixed.** **[probed]** `enum { X = 5000000000 }` gave `X == 705032704` and `enum { H = 0xFFFFFFFFU }` gave `-1`; every enum was four signed bytes. C17 6.7.2.2p4 requires the enumerated type to represent every member, so `enum_underlying_type` now picks the narrowest of int / unsigned int / long / unsigned long that does, and the size, alignment and the constants' type all follow. An enum whose members fit in `int` — nearly all of them — is unchanged. Three places answered the width with a literal 4 rather than asking the composite, so `sizeof` stayed 4 even once the type was right. 6.7.2.2p2 also makes an out-of-range enumerator a constraint violation, which 5.1.1.3 requires be diagnosed: gcc widens without complaint unless asked, c17 widens **and** warns, since silence is what turned a wrong value into a running program. No system header on this machine trips it. Enumerator values are carried at 128 bits from parse onward, the width `eval_const_expr` already returns, so nothing wraps before the range is known. Test: `c99_enum_is_wide_enough_for_its_enumerators`.

- [x] **#C81 — An attribute on any file-scope declarator but the first was a parse error.** **Fixed, and supersedes #C60.** **[probed]** `int a, b __attribute__((unused));` did not compile at file scope, while the identical line inside a function did — the block-scope loop parses attributes after every declarator and the file-scope continuation loop went straight from `parse_declarator` to symbol binding. #C60 recorded this as the attribute *attaching to the declaration* and reaching `a`; verified against a build of `054c8830`, that behaviour has never existed, and the entry was describing something the compiler did not do. Fixing the parse exposed the scope question underneath: `int p, q __attribute__((aligned(64))), r;` names `q` alone, while `int __attribute__((aligned(64))) u, v;` and `_Alignas(64) int w, x;` name the whole declaration. All three fed one `pending_alignas` slot cleared at the semicolon, so every declarator after an attributed one inherited its alignment — `r` came out 64-byte aligned where gcc gives it four. Post-declarator alignment now has its own slot, taken and cleared by the declarator it belongs to. Tests: `codegen_attribute_binds_to_its_own_file_scope_declarator`, `codegen_attributes_on_later_declarators_compile_and_run`, `codegen_attribute_alignment_scope_follows_where_it_is_written`.

- [x] **#C82 — `#pragma pack` was ignored in every form.** **Fixed.** **[probed]** `push`/`pop`, `pack(n)` and `pack()` all produced natural layout; `__attribute__((packed))` worked, which is what made the gap easy to miss. A structure declared under `#pragma pack(1)` was laid out at natural alignment silently, and in disagreement with any gcc-compiled peer sharing it — an ABI break, not a cosmetic one. The obstacle was that packing is the parser's business and pragmas died in the preprocessor: neither a source position nor a token index recorded during preprocessing survives to the parser, because an `#include` is preprocessed into its own vector and spliced in afterwards. So the pragma travels as a marker token, and `extract_pragma_directives` strips the markers from the finished stream and reports where each stood — run once, after preprocessing, when the vector is finally the translation unit in the order the parser walks it. Layout takes an alignment *cap* now rather than a `packed` flag, since that is what the two features share: `packed` is a cap of 1, `pack(n)` a cap of n, tighter wins. Unions are capped the same way. `_Pragma("pack(...)")` is the same directive by C99 6.10.9 and is honoured too, through one token vocabulary and one parser rather than two that agree until they don't. An unbalanced `pop` warns and is ignored, as gcc does. ctags, cflow, cxref and xgettext strip the markers too — they compute no layout, but the markers are not C tokens. Test: `c99_pragma_pack_caps_member_alignment`.

- [x] **#C83 — Thirteen builtins were rejected outright.** **Fixed.** `__builtin_choose_expr`, `strlen`, `strcmp`, `abs`, `labs`, `llabs`, `ffs`, `ffsl`, `parity`(`l`/`ll`), `sqrt`, `copysign` and `trap`. `__has_builtin` answered 0 for each, so guarded code was already correct; the exposure was the unguarded uses in system headers — `__builtin_strcmp` in eight on this machine, `__builtin_strlen` in seven. Most are the library function under a reserved name and lower to an ordinary call by de-prefixing, the path `__builtin___*_chk` already took; an expression node apiece would mean a case in the linearizer and in both backends for something that is already a call, and it settles the evaluation question a textual expansion raises — `__builtin_abs(f())` calls `f` once. `__builtin_trap` maps to `abort`. The names are pre-interned so a translation unit may call `__builtin_strlen` without the header that declares `strlen`, as gcc allows. `__builtin_choose_expr` selects at parse time next to `_Generic`, so the unchosen arm need not type-check — the whole reason glibc reaches for it. Left out and still honestly reported absent: `__builtin_clear_padding` (has to walk a type to find its padding), `__builtin_setjmp`, `__builtin_va_arg_pack`. `__builtin_rotateleft32` was on the candidate list and is **not** a gcc builtin at all — gcc had accepted it only as an implicit function declaration; checking that each name *links* under gcc, rather than merely compiles, is what caught it. Test: `builtins_library_and_bit_builtins_are_available`.

- [x] **#C84 — Plain `char` is signed on aarch64.** **✓ fixed (2026-08-17).** The signedness lived in a modifier bit, and plain `char` has none — so `TypeTable::is_unsigned` answered "signed" on every target, and only the five backend load/copy sites knew better, each carrying its own copy of an `is_unsigned || (is_plain_char && !target.char_signed)` idiom. The result contradicted itself inside one function: `int ld(char *p){ return *p; }` emitted a correct zero-extending `ldrb w1,[x0]` and then `sxtb x0,w0` to undo it, where cross-gcc emits the `ldrb` alone. `TypeTable` now carries `char_signed` and the predicate answers from it, which corrects all 33 call sites at once — the front end's `Zext`/`Sext` choice included — and the five idioms collapse to a plain call, retiring `is_plain_char`. Verified by *running* the output: `c17 --target=aarch64 -S`, assembled and linked with the cross toolchain and executed under `qemu-aarch64-static`, agrees with `aarch64-linux-gnu-gcc` on `(int)(char)-1 == 255`, `(char)-1 < 0` being false, a value read back through memory, division, shifting, and the `signed char`/`unsigned char` controls; x86-64 keeps plain `char` signed. Two things had to move with it: `cflow` reports the *spelling* a declaration used, so it must not follow the target (a separate `spelled_unsigned` accessor now answers that question, and both type printers ask it — otherwise `char` would have printed as `unsigned char`, on aarch64 hosts only, unpinned by any test); and `int_suffix_for_longdouble` asks the *promoted* type, since C converts a sub-`int` value through `int` and truncates, so no narrow type should pick its own helper. `codegen_integer_argument_widens_to_a_128_bit_parameter` used plain `char` to test that signed sources sign-extend and would have failed on the aarch64 runner; it says `signed char` now. Tests: `codegen_plain_char_follows_the_target_signedness` (both targets, both directions), `test_plain_char_signedness_follows_the_target`, `tools_cflow_prints_the_declared_char_spelling`. _(Original finding below.)_ **[probed]** AAPCS64 makes plain `char` **unsigned** on aarch64, and gcc follows: `char c = -1;` gives 255 under `aarch64-linux-gnu-gcc` and -1 under c17 for the same target. Every comparison, promotion and library call involving a plain `char` therefore disagrees with the system libraries and with any gcc-compiled object. Found while verifying #C75's aarch64 results, where the c17 and gcc outputs for a `char` argument differed for a reason unrelated to that fix. `target.char_signed` already exists and is consulted by the load path (`cc/arch/x86_64/codegen.rs`), so the defect may be no more than its value for this target — but the blast radius is every `char` in every program, so it wants its own measurement and its own gate run rather than being folded into an unrelated series.

- [ ] **#C85 — `__mode__` is unimplemented, and warns on nearly every translation unit.** **Open, Minor — noise.** `bits/floatn.h:74` uses `__attribute__((__mode__(TF)))`, so a warning fires for `<stdio.h>` (1), `<stdlib.h>` (2) and `<math.h>` (1) — that is, for almost every program compiled on glibc. The declared type is used unchanged, which happens to be right for the cases glibc uses it in, so this is noise rather than a wrong answer. Deliberately out of scope of the 2026-08-16 series; recorded so the noise is a known cost rather than an unnoticed one. `__has_attribute(mode)` honestly answers 0.

- [x] **#C86 — The integer promotions were not applied before the usual arithmetic conversions.** **✓ fixed (2026-08-17).** **[probed]** `Parser::usual_arithmetic_conversions` dispatched on the *raw* operand kinds, so two operands narrower than `int` matched none of the Int128/LongLong/Long arms and fell to the final "either operand is unsigned" fallback. `unsigned char a=1,b=2` gave `(a-b)/2 == 2147483647` where gcc gives 0, `(a-b)>>1 == 2147483647` where gcc gives -1, and `(a-b)<0 == 0` where gcc gives 1; `unsigned short` the same, and `--dump-ir` showed `divu.32` where the promoted type is `int` and the division must be signed. `unsigned char` runs through the whole of CPython. Found while designing #C84's fix, and it **blocked** it: `_Bool` and plain `char` produced the correct `divs.32` *only* because `is_unsigned` wrongly called them signed, so the two defects cancelled and correcting the predicate alone would have uncancelled them. The linearizer's own `common_type` already promoted — which is why comparisons behaved while arithmetic did not, and why one rule implemented twice disagreed for years. `integer_promote` moves onto `TypeTable` where both can reach it. Test: `c99_integer_promotions_precede_the_usual_arithmetic_conversions`.

- [x] **#C87 — A `_Bool` bit-field read back as −1, and a braced initializer for one truncated without normalizing.** **✓ fixed (2026-08-17).** **[probed]** `struct S { _Bool f:1; }` with `f` set read back **−1**, so `f == 1` was *false*; and `struct S v = {2};` read back **0**. Two independent defects in opposite directions, so a test of a single value would have missed one. The read: `emit_bitfield_load` extends a field whose type is not unsigned and asked `is_unsigned`, which answered from the `UNSIGNED` modifier bit — `_Bool` carries none, there being no `signed _Bool` to distinguish it from — so a one-bit field took `emit_sign_extend_bitfield`, which at width one is `shl 31; sar 31`. C17 6.2.5p6 lists `_Bool` among the standard *unsigned* integer types, so the predicate now says so. The initializer: a braced bit-field initializer converted straight to the field's unsigned *storage* type, skipping the member type and with it the 0/1 normalization every conversion into `_Bool` gets (6.3.1.2). Two sites that read the modifier bit directly rather than asking the predicate — the constant folder for the relational operators, and the x86-64 float-to-int widening — were routed through it, and the `_Bool` exemption in the single-bit-signed-bitfield warning was deleted as redundant. Test: `c99_bool_bitfields_hold_zero_or_one`.

- [x] **#C88 — A variably modified declaration at file scope is accepted.** **✓ fixed (2026-08-18).** The first declarator of a file-scope declaration has its own array-dimension loop -- a fourth copy of array-declarator parsing, not reached by `parse_declarator` -- and it mapped every size it could not fold to `unwrap_or(0)`, recording no VLA and saying nothing. That is why the three existing "variable length arrays cannot have file scope" checks caught `int ok[2], bad[n];` but not `int bad[n];`. The same `Some(_) => None` arm silently accepted a *negative* size, there and in both of `parse_declarator`'s, so `int a[-1];` became an incomplete array at file scope and a zero-length one in a block; 6.7.6.2p1 requires the size to be greater than zero, and zero itself stays accepted as the GNU extension gcc also accepts. Nineteen declarations checked against gcc, rejecting and accepting alike. Tests: `diagnostics_variably_modified_declaration_at_file_scope_is_rejected`, `diagnostics_negative_array_size_is_rejected`, `diagnostics_ordinary_file_scope_arrays_are_accepted`, `test_file_scope_array_size_constraints`. _(Original finding below.)_ **[probed]** `int n; int bad[n];` compiles with no diagnostic and `sizeof bad == 0`; gcc rejects it, *"variably modified 'bad' at file scope"* (C17 6.7.6.2p2 confines a variably-modified ordinary identifier to block scope). Every spelling tried is accepted: with a second declarator, `static`, `extern`, an expression bound, and a function-call bound. Three checks in `parse/parser.rs` already read `!vla_sizes.is_empty()` and raise *"variable length arrays cannot have file scope"*, so the intent is there and something upstream is not reaching them. Found while closing #C52, which was expected to inherit this rejection for `int arr[sizeof(int[n])];` and does not — that declaration is likewise accepted at 0 bytes. Entirely pre-existing and independent of `sizeof`: the reproducers above contain none.

- [x] **#C93 — A character constant did not follow its encoding prefix.** **✓ fixed (2026-08-17).** **[probed]** Two defects in one conversion, found when #C84 turned CI red on both aarch64 runners. The unprefixed form: 6.4.4.4p10 gives it the value of a `char` object converted to `int`, so it follows plain `char`'s target signedness — and #C84 moved the *type* without moving the *constant*, leaving c17 saying `'\x80'` is -128 on aarch64 while `(char)'\x80'` is 128. `enum { PROTO = '\x80' }` then stopped matching a `char` read from a buffer, which is the CPython pickle dispatch shape the regression test was written for: it links, runs, and takes the wrong `switch` arm. The prefixed forms were wrong on **every** target and had been all along — five consumers each re-derived the value from a `char` with a hardcoded `as u8 as i8`, so `L'\x80'` was -128 against gcc's 128, `u'\x80'` was 65408, and `L'\xe9'` was -23 against 233. The conversion now happens once, in the parser, where the prefix is known, and `ExprKind::CharLit` carries the value rather than a `char` for five sites to reinterpret. A universal character name still keeps its code point (`'é'` 233, `'\U0001F600'` 128512), which truncating to a byte would have destroyed. Test: `codegen_char_literal_signedness`, now asserting both targets' answers through `__CHAR_UNSIGNED__` and covering all three prefixes.

- [x] **#C94 — aarch64 zero-width bit-field layout disagrees with gcc.** **✓ fixed (2026-08-18).** C17 6.7.2.1p12 leaves to the ABI whether a zero-width bit-field also raises the enclosing aggregate's alignment: the x86-64 psABI says no, AAPCS64 says it contributes its declared type's alignment, and c17 gave the x86-64 answer on both targets. Unlike an ordinary member's, that contribution survives packing -- gcc gives 8/4 for `__attribute__((packed))` and under `#pragma pack(1)` alike -- so it is accumulated apart from `max_align`, which the pack cap caps. Fixing the struct case exposed the union one, wrong on **both** targets and in the other direction: a zero-width bit-field contributed its declared type's whole size and alignment, making `union { char c; int :0; }` four bytes where gcc gives one. It occupies no storage, so it cannot widen a union. Thirteen shapes now agree with gcc byte for byte on both targets, sizes, alignments and offsets alike. Tests: `c89_zero_width_bitfield_forces_a_boundary` (its target-independent half asserted unconditionally, the rest per target) and `test_zero_width_bitfield_alignment_is_abi_specific`, which checks both targets from one host. _(Original finding below.)_ **[probed]** `struct A { char c; int :0; char d; }` is **5** bytes under c17 on aarch64 and **8** under `aarch64-linux-gnu-gcc`; `struct F { char c; short :0; char d; }` is 3 against 4. c17 gives the x86-64 answer on both targets — gcc gives 5 on x86-64 and 8 on aarch64 — so `c89_zero_width_bitfield_forces_a_boundary`, whose expectations were taken from gcc on x86-64, passes on aarch64 while disagreeing with every object compiled there. Pre-existing and untouched by the signedness series; found by a sweep that ran every C program in the test suite through the cross toolchain under qemu and compared against gcc, which is the check the local suite cannot make (`compile_and_run` always targets the host). That sweep is worth repeating whenever layout or ABI code changes: 243 of the 357 extracted programs build for aarch64, and 241 agree with gcc exactly.

- [x] **#C95 — A jump into the scope of a variably modified identifier was accepted.** **✓ fixed (2026-08-18).** **[probed]** C17 6.8.6.1p1 forbids a `goto` from outside the scope of an identifier with a variably modified type to inside it, and a `switch` reaching a `case` there is the same jump. Not bookkeeping: entering the scope without executing the declaration leaves the object's size never computed, so the array is whatever the stack held. c17 accepted every form and ran. The check walks the body once, recording for each label and each jump which variably modified scopes enclose it; the walk is order-sensitive within a block, since the scope opens at the declaration and runs to the end of the block, which is what makes `goto L; int a[n]; L:` illegal while `goto L; L: ; { int a[n]; }` is fine. Reported at the declaration that would have been skipped, naming it -- `Stmt::Goto` and `Stmt::Default` carry no position, and the declaration is the more useful half. Ten shapes agree with gcc, including the accept side: out of a scope, within one, to a label preceding the declaration, a plain array, and a `case` whose VLA is inside it. Tests: `diagnostics_jump_into_variably_modified_scope_is_rejected`, `diagnostics_legal_jumps_around_variably_modified_scopes_are_accepted`.

- [x] **#C96 — A universal character name naming a forbidden character was accepted.** **✓ fixed (2026-08-18).** **[probed]** C17 6.4.3p2: a UCN may not name a character below 00A0 other than `$`, `@` and `` ` ``, nor one in D800..DFFF. All of it was accepted, and the surrogate half failed worst -- a surrogate has no `char`, so `char::from_u32` returned `None` and all three decoders read that as "not an escape" and carried on with the letter `u`. There are three decoders (two in the lexer, for an identifier that starts with a UCN and one that merely contains one, and one in the parser for string and character literals), each with its own copy of the decode; they now share one predicate and one message, and the raw scalar is checked before `char::from_u32`. The parser carries a forbidden UCN as its own `Escaped` variant, which is what made the compiler name all three consumers. `test_ucn_long_form` had asserted `test\U00000041bc` lexes to `testAbc`, which is the violation itself, and now uses a code point the long form may name. Tests: `diagnostics_forbidden_universal_character_names_are_rejected`, `diagnostics_permitted_universal_character_names_are_accepted`, `test_ucn_forbidden_characters`.

- [x] **#C99 — A `switch` whose body was not a compound statement dropped the test.** **✓ fixed (2026-08-18).** **[probed]** `int f(int x){ switch (x) case 1: return 2; return 0; }` returned **2** for every `x`; gcc returns 0 unless `x` is 1. Two independent defects with one symptom, both about the body being one *statement* rather than a block. (1) `case E : statement` is a single labeled statement in the grammar, but `Stmt::Case` is a marker carrying the value and not the statement it labels -- a flattening that is sound only inside a block, where marker and statement remain adjacent items of one list. A non-compound body has room for exactly one statement, so the marker took the whole body and the labeled statement escaped to become the *following* statement of the enclosing block, reached unconditionally. `parse_switch_body` rebuilds the block the flattening assumes, and leaves a braced body or an unlabeled one exactly as it was, so no existing switch changes shape. (2) `collect_switch_cases` and `linearize_switch_body` both opened `if let Stmt::Block(items) = body`, so a legal `switch (x) while (n < 3) { case 1: n++; }` -- where the label is not a prefix and the parser correctly leaves the body alone -- collected no cases at all and emitted a switch with an empty table, sending every value to the default edge. Both now walk a non-compound body as the one statement it is. Tests: `c89_switch_with_a_non_compound_body_still_tests_the_value` (both label kinds, two labels, an empty labeled statement, `while`/`if`/`do`/`for` bodies, each at a matching and a non-matching value, with the block form and an unlabeled body as controls), plus `test_switch_with_non_compound_body_keeps_its_labelled_statement` and `test_switch_bodies_that_need_no_wrapping_are_unchanged`.

- [x] **#C100 — A statement before the first `case` label ran unconditionally.** **✓ fixed (2026-08-18).** **[probed]** Found by #C99's fix, and older than it: `int f(int x){ int n=0; switch (x) { n = 5; case 1: n += 1; } return n; }` gave 9 at `f(9)` where gcc gives 0, and 6 at `f(1)` where gcc gives 1. C17 6.8.4.2 gives such a statement no edge from the switch -- control leaves for a case label, so anything standing before the first one is unreachable. `linearize_switch` left `current_bb` pointing at the block the switch instruction itself terminates, so the prefix was lowered into it and executed before the dispatch. It now clears `current_bb`, the same "nothing to emit into" state `goto` already leaves behind; the first `case` restores a live block through `switch_bb`. A declaration there stays legal and its initializer is equally unreachable, which is the shape `switch (x) { int y = 7; case 1: ... }` tests. This was wrong in the **block** form too, so it is not a consequence of #C99 -- but #C99's fix extended it to a body that had previously been dropped whole, which is how it surfaced. Tests: `prefix_stmt`, `prefix_only` and `prefix_decl` in `c89_switch_with_a_non_compound_body_still_tests_the_value`.

- [x] **#C101 — The difference of two addresses into one object was not a constant.** **✓ fixed (2026-08-18).** **[probed]** `int a[4]; long d = &a[2] - &a[0];` was *rejected* -- *"non-constant offset in pointer arithmetic initializer"*, and reported at line **0** with no column, because the message used `current_pos` rather than the expression's own. So were `(a+2) - a`, `(char *)&s.b - (char *)&s.a` and `(unsigned long)&x - (unsigned long)&x`; gcc accepts all four, and they are ordinary idioms. C17 6.6 does not list an address difference among the constant expressions, but the distance between two points *in one object* is fixed at layout time, and every implementation folds it. `static_address_difference` folds it, scaling by the pointee size for a genuine `ptr - ptr` (6.5.6p9 counts elements) and not for a subtraction of two addresses already cast to an integer type, which counts bytes. Across two objects the distance is not known until they are laid out, so `&a[0] - &b[0]` stays a diagnostic, matching gcc. The operand resolver is deliberately narrower than `eval_static_address`, which answers for a bare identifier because its callers have already seen the `&`: reaching it from a subtraction would have folded `int v, w; long d = v - w;` to zero. Tests: `diagnostics_address_differences_within_one_object_are_accepted` (five shapes), `diagnostics_address_differences_across_objects_are_rejected` (five, including two ordinary variables and two pointer objects), and `codegen_static_address_difference` for the values and both unit conventions.

- [x] **#C112 — `sizeof` of an incomplete array *expression* answered 0.** **✓ fixed (2026-08-18).** **[probed]** `extern int a[]; sizeof a` compiled and gave 0; gcc rejects it (6.5.3.4p1). #C90 closed the type-name form and recorded this one as "not covered"; neither the type nor the completeness helpers can settle it, because `int[]`, `int[n]` and `int[m]` all intern to one `TypeId` and the extent lives on the declarator. `Symbol::array_is_variably_modified` records whether one was given -- the same shape as the other declaration facts the symbol already carries -- so a VLA's `sizeof` keeps working while an incomplete array's is refused. **Two further defects had to go with it.** The file-scope declarator loop built an absent extent as `Type::array(base, size.unwrap_or(0))`, i.e. `Some(0)`, where `parse_declarator` keeps `None` -- so `int a[];` was indistinguishable from the GNU zero-length `int a[0];`, and the two paths disagreed about what incomplete looks like. And a redeclaration reused the existing symbol with the existing type, so `extern int a[]; int a[4];` left the object incomplete for good and `sizeof a` was refused after the definition; 6.2.7p4 makes the composite carry whichever extent is known. Tests: `diagnostics_sizeof_of_an_incomplete_array_expression_is_rejected` (four), `diagnostics_sizeof_of_complete_array_expressions_is_accepted` (eleven, including a VLA, a zero-length array and both completing forms), and `codegen_sizeof_of_array_objects` for the values.

- [ ] **#C102 — Reading an object in a static initializer is neither folded nor diagnosed.** **Open, Minor -- one missing mechanism behind three symptoms.** **[probed]** `int v; int w = v;` at file scope is accepted and silently yields **0** (gcc: *"initializer element is not constant"*). `const int c = 5; int w = c;` is accepted and correct, but only by the same accident -- nothing folded it, and `Initializer::None` happened to mean zero where 5 was wanted... except it does not: the value is right because the object is `.bss`-adjacent, and the shape one step along, `int w = c + 1;`, is **rejected** where gcc accepts. So the same gap under-diagnoses one shape, accidentally answers a second, and falsely rejects a third. Fixing it properly needs an **initializer-scoped** constant evaluator: gcc folds a `const`-qualified object with a visible constant initializer (silently, even under `-pedantic`), but that folding must not reach `eval_const_expr`, where C requires strictness -- `const int c = 5;` makes `int a[c]` a VLA and `case c:` an error, in gcc as here. A first attempt during the #C101 work added the fold at the `Ident` arm alone and was reverted: it closed the silent zero and left `c + 1` rejected, which is the "right for one shape, wrong for another" failure the 2026-08-17 series existed to remove. Substituting the value into the expression before folding is the obvious route and needs care not to descend into `sizeof`, `_Alignof` or `&`, where the substitution would change the answer.

- [x] **#C103 — A zero-width bit-field changed how the aggregate around it was passed, on both targets.** **✓ fixed (2026-08-18).** **[probed]** `struct A { float f; int :0; };` passed by value went in a general-purpose register on **both** ABIs where gcc uses `%xmm0` / `s0`. Demonstrated across translation units, which is the only way it can be seen: a gcc-compiled caller handing `1.5f` to a c17-compiled callee delivered **1.4013e-45**, and the reverse direction failed the same way. Same-compiler calls agreed by luck, both ends making the same wrong choice, which is why 1,685 tests and the CPython gate never caught it. One cause, two expressions of it -- each ABI asked a bit-field member for its *declared type* rather than the bits it was allocated. System V's `classify_aggregate_eightbyte` gave `int :0` at offset 4 the extent 32..64, so it merged INTEGER over the SSE class the `float` had already put in eightbyte 0; AAPCS64's `try_classify_hfa` saw a non-floating member and refused the aggregate homogeneity outright, and counted it against `MAX_HFA_ELEMENTS` as well, so `struct { float a,b,c,d; int :0; }` was five members and disqualified twice over. 6.7.2.1p12 gives a zero-width bit-field an effect on layout and nothing else, and it is not a member for either classifier; a bit-field of non-zero width is a real integer member and still disqualifies the aggregate, on both targets. gcc notes this ABI changed in its own 12.1, so the rule matched here is the current one. Verified by *executing* nine struct shapes across a gcc/c17 boundary in both directions on x86-64 and, under `qemu-aarch64-static`, on aarch64. Test: `cross_abi_zero_width_bitfield_does_not_change_argument_class`, which asserts each body is byte-identical to the same struct without the bit-field -- the claim being that it changes nothing -- with a positive check on the reference so two equally-wrong functions cannot agree their way to a pass, and a non-zero-width control that must still differ.

- [x] **#C104 — An enumeration with no negative member was signed, where gcc makes it unsigned.** **✓ fixed (2026-08-18).** **[probed]** `(enum E)-1 > 0` was false here and is true under gcc for `enum E { A, B }`; `enum { X = 0x100000000LL }` was `long` where gcc uses `unsigned long`. 6.7.2.2p4 leaves the underlying type to the implementation, requiring only that it represent every member, so neither answer is nonconforming -- but the two disagree about what the same program computes, and gcc's rule (unsigned whenever no enumerator is negative) is the platform's. `enum_underlying_type` preferred `int` for any list fitting in `int`, so it reached the unsigned arm only for a list that did *not* fit. Sizes are unchanged on every shape; an enumeration with a negative member stays signed. Found while fixing #C105, which it silently broke: a bit-field's signedness follows its declared type's. Test: `c99_enum_bitfields_and_enum_signedness`.

- [x] **#C105 — An `enum` bit-field was rejected, and an unnamed bit-field was never validated at all.** **✓ fixed (2026-08-18).** **[probed]** `enum E { A, B }; struct S { enum E e : 2; };` failed with *"bitfield must have integer type"*; gcc accepts it and real headers use it heavily, so this refused valid code rather than merely diverging. `validate_bitfield` tested a hand-written list of `TypeKind`s that omitted `Enum`; 6.7.2.1p5 permits `_Bool`, `signed int`, `unsigned int` and "some other implementation-defined type", and gcc's set is every integer type -- which `TypeTable::is_integer` already spells, including every kind the list named. The same function turned out never to be *called* for an unnamed bit-field, at either of the two sites that parse one, so `struct { float : 3; }` was accepted while `struct { float f : 3; }` was rejected; both now validate. Accepting enum bit-fields immediately exposed #C104: a two-bit field of `enum { A, B, C, D }` holding `D` read back **-1**, because the field's signedness follows the type's and the type was signed. Verified on both targets, by execution under `qemu-aarch64-static` for aarch64. Tests: `c99_enum_bitfields_and_enum_signedness` (values, sizes, signed and unsigned enumerations, and a neighbour either side), `diagnostics_enum_bitfields_are_accepted`, `diagnostics_non_integer_bitfields_are_rejected` (both spellings of a `float` bit-field, plus the width constraint on the unnamed form).

- [x] **#C106 — Four jump and label constraints were accepted in silence, three of them producing broken programs.** **✓ fixed (2026-08-18).** **[probed]** None of these was merely an undiagnosed constraint; each changed what the program did. A `goto` to a label that does not exist minted a basic block through `get_or_create_label` and never terminated it, so control fell out of the function through whatever followed in layout order -- the program **built, linked and segfaulted**. Two labels of one name in one function got the *same* block back and were silently merged, so `L: i++; if (i<2) goto L; L: return i;` **looped forever**. A `break` or `continue` with nothing to jump out of was silently **deleted** -- `int main(void){ break; return 5; }` returned 5 -- because the arms are written `if let Some(&target) = self.break_targets.last()` with no else. And 6.8.4.2p1's requirement that the controlling expression have integer type was unchecked, the type being fetched only to size the instruction, so `switch (d)` on a `double` compiled and took the **wrong branch**. Also 6.8.1p2 for a `case`/`default` outside any `switch`, whose arm was commented "if we encounter them outside a switch, ignore them". The four structural checks go in the whole-body pre-pass rather than the `linearize_stmt` arms that look like the obvious home: a switch body is lowered by `linearize_switch_stmt`, which delegates back into `linearize_stmt` for nested constructs, so an arm there cannot tell "outside every construct" from "reached by delegation" and would false-positive on a legal `case`. The walker already threaded `switch_scopes` and collected labels and gotos for #C95, and carried the comment *"an undefined label is a separate diagnostic"* naming a check that did not exist; it gains a loop and a switch depth. Only the switch-type check stays in `linearize_switch`, which has the type and a real position already. The five jump and label `Stmt` variants gained a `Position` in the same commit -- they are the ones whose diagnostics have no other anchor, and `current_pos` names the last *expression* lowered, so every message would otherwise have pointed at an earlier line. Giving those variants a position also let the **variably-modified-jump** diagnostic of #C95 move to where gcc puts it: it had reported at the *declaration* for want of anything better, and the doc comment justifying that outlived its reason. It now reports at the jump and still names the declaration, which gcc does not -- the position says where to look and the name says what the problem is. `VmDecl.pos` went with the change. Tests: `diagnostics_variably_modified_jumps_point_at_the_jump` asserts line and column, the position being the fix; `diagnostics_stray_jumps_and_labels_are_rejected` (nine) and `diagnostics_legal_jumps_and_labels_are_accepted` (seventeen) -- the accept side being where a check of this shape goes wrong, so it covers a forward `goto`, a label of the same name in two functions, `continue` reaching a loop through an enclosing `switch`, `break` inside a statement expression, Duff's device, and `switch` on `enum`/`char`/`_Bool`/a bit-field/`__int128`.

- [x] **#C107 — `*` on a non-pointer and a call of a non-function were both accepted.** **✓ fixed (2026-08-18).** **[probed]** `int x; return *x;` compiled and dereferenced the integer's *value* as an address, so the program built and **segfaulted** (6.5.3.2p2). `int x; x();` reached the back end and emitted a call to a symbol named `x`, so what should have been a front-end error surfaced as an undefined-reference **link** failure naming a variable that plainly exists (6.5.2.2p1). Both checks sit beside the existing `check_subscript`/`check_not_void` family and use their wording, which is gcc's. The accept side is what makes them non-trivial: a *function designator* may be dereferenced -- `(*f)()` and `(***f)()` are ordinary idioms and `*f` on a function is a no-op, which the result-type computation already modelled -- and a call may go through a pointer, a dereferenced pointer, a cast to one, a struct member or an array element. Tests: `diagnostics_bad_deref_and_call_operands_are_rejected` (six) and `diagnostics_legal_deref_and_call_operands_are_accepted` (thirteen).

- [x] **#C108 — An initializer's type was never checked, at either scope.** **✓ fixed (2026-08-18).** **[probed]** C17 6.7.9p11 gives a scalar initializer the constraints of simple assignment, and p13/p14 confine an aggregate to a brace-enclosed list or -- for a character array -- a string literal. None of it was checked. `int x = s;` from a struct and `int b[3] = a;` from an array both compiled and yielded **zero**; `struct S s = 1;` was accepted; `int *q = d;` from a `double` reached the same `emit_convert` that #C47 had stopped a plain assignment from taking. The *assignment* form of every one of those was already diagnosed, so the same program was accepted or rejected depending on whether the value arrived through `=` in a declaration or `=` in a statement -- which is what made this a gap rather than a policy. `check_initializer_types` is a fourth reporter beside `check_assignment_types`, `check_return_type` and `check_argument_types`, over the same `TypeTable::assignment_fault`; severities therefore follow `AssignFault::is_error` and are gcc's without a table to maintain, an incompatible type being an error and the pointer/integer conversions warnings. Two shapes take gcc's own wording instead, because it is the clearer one and the words a user searches for: an aggregate initialized from something that is not a compatible aggregate, and any array not initialized from a list or a string, are both *"invalid initializer"*. **Corrected after review**: the first version accepted *any* string literal for *any* array, so `int a[] = "hi";` compiled. 6.7.9p14 gives the narrow literal to a *character* array -- all three of `char`, `signed char` and `unsigned char`, which `TypeKind::Char` covers because signedness is a modifier -- while p15 gives a wide one an array whose element type is **compatible with the literal's own**, a finer distinction: `int a[] = L"ab";` is legal where `wchar_t` is `int`, and `unsigned a[] = L"ab";` is not. Comparing the two element types for strict compatibility, the obvious fix, would have rejected `signed char a[] = "hi";` -- so the narrow case tests the kind and the wide case tests kind *and* signedness, both unaffected by a qualifier so `const char a[] = "hi";` still passes. 26 further rows agree with gcc. Measured against `gcc -std=c17` on 31 rows -- 10 rejections at both scopes, 3 warnings, and 21 accept-side controls including struct-from-struct, a wide string into a `wchar_t` array, a braced string, a compound literal, a designated initializer and every ordinary conversion. The check fires **nowhere** in a full CPython -O2 build, which is the measurement the risk deserved. Tests: `diagnostics_bad_initializers_are_rejected`, `diagnostics_converting_initializers_only_warn`, `diagnostics_ordinary_initializers_are_accepted`.

- [x] **#C109 — Four declaration constraints were accepted, each losing information rather than only a diagnostic.** **✓ fixed (2026-08-18).** **[probed]** A repeated enumerator (`enum A { X }; enum B { X };`) kept the first one's value; an enumerator and an ordinary identifier could collide in either order. A repeated parameter name left the second parameter with **no symbol at all**, so every use of the name reached the first. `static static int x;` and `static extern int x;` were both taken, the modifier bits simply OR-ed together. And `extern int e = 1;` at *block* scope declared a name with linkage and then defined it locally. Three of the four needed no new detection: `SymbolTable::declare` already returns `Err(Redefinition)` and both the enumerator and the parameter site discarded it with `if let Ok(...)`, while `check_redeclaration` already had gcc's *"redeclared as a different kind of symbol"* wording and simply excluded enum constants from the kinds it considered. Storage classes gained a tally in the existing `SpecifierTally`, whose `check()` already reported `duplicate '{0}'` for the other specifier families. **`_Thread_local` is deliberately not tallied**: 6.7.1p2 lets it accompany `static` or `extern`, in either order, and counting it would have rejected every ordinary thread-local declaration. The scope distinction on `extern` matters too -- at file scope the same spelling *is* a definition with external linkage, which gcc only warns about, so c17 warns there and errors only at block scope. Zero hits across a full CPython -O2 build. Tests: `diagnostics_repeated_declaration_parts_are_rejected` (ten), `diagnostics_file_scope_extern_initializer_only_warns`, `diagnostics_ordinary_declarations_are_accepted` (thirteen, including both orders of `_Thread_local`, a repeated *qualifier*, and an enumerator shadowed in an inner scope).

- [x] **#C110 — `__attribute__((packed))` did not pack a bit-field.** **✓ fixed (2026-08-18).** **[probed]** `struct __attribute__((packed)) { unsigned a:20, b:20; }` was **8** bytes where gcc gives 5, and eight of fourteen measured shapes disagreed -- a silent ABI mismatch with any gcc-compiled object, and the last of the divergences `cc/doc/TODO.md` recorded as known. The straddle test in `compute_struct_layout` ran whatever the pack cap, because the access path addressed one power-of-two unit and could not span an arbitrary byte range; the two therefore had to change together, and did. **The rule, measured over fourteen shapes on both targets:** under a pack cap the unit rule is switched off *entirely*, not narrowed to the cap. `#pragma pack(2)` lets a 16-bit field starting at bit 1 straddle both the 2- and the 4-byte boundary, which is the measurement that rules out the narrowing reading. A zero-width bit-field still rounds to its declared type's unit even when packed, and on AAPCS64 still raises the aggregate's alignment -- #C94's split is untouched. A packed union member contributes `ceil(width/8)` bytes rather than `sizeof(T)`, making `packed union { unsigned a:20; char c; }` three bytes. **Access** is byte-wise for any span that is not 1, 2, 4 or 8 bytes, which is what gcc emits on both targets and is necessary rather than merely equivalent: a packed struct can be *smaller* than any power-of-two window covering the field -- `{ unsigned c:1; unsigned long long a:64; }` is nine bytes and the field needs all nine -- so a wide load would read past the object, a fault risk at a page boundary and an out-of-bounds report under any sanitizer. A store writes a fully-covered byte outright and read-modify-writes only the bytes it shares, so a neighbour's bits survive and nothing outside the span is touched. The unpacked case keeps the single-unit path with identical operands, which is why no existing bit-field test changed. `storage_unit_size` was renamed **`access_bytes`** in a preceding behaviour-neutral commit, its contract restated -- the old name was only ever true of the unpacked case -- which forced every one of its consumers to be read. Verified by executing sizes, alignments, offsets and value round-trips against gcc on x86-64 and, under `qemu-aarch64-static`, on aarch64; static initializers too, which go through the byte merge rather than the access path. Test: `c99_packed_bitfields_pack_to_the_bit`.

- [x] **#C111 — An object of incomplete type, and a misplaced flexible array member, were both accepted.** **✓ fixed (2026-08-18).** **[probed]** `struct U; struct U u;` compiled at either scope and `sizeof` it answered 0 (6.7p7); nothing in the tree recognised a flexible array member **at all**, so `struct S { int a[]; int b; }` sized the array zero and carried on (6.7.2.1p18). Each check is easy; the accept side is the whole difficulty, and both needed a rule that is not "check it where you see it". **Incomplete objects** cannot be judged at the declaration at file scope, because 6.9.2p3 lets a *tentative* definition be completed later in the translation unit -- `struct U; struct U u; struct U { int a; };` is legal, and forward-declare-then-complete is everywhere in CPython and glibc. File-scope definitions are therefore collected as they are parsed and judged in `check_deferred_incomplete_definitions` at end of translation unit, when nothing more can complete them; block scope has no such grace and is checked immediately. An array is different again: its *element* type must be complete where the array is declared, since the stride is what forms the type, so `struct U; struct U a[2];` is refused even with the tag completed below and even under `extern` -- which is gcc's rule, and needed `array_element_deep` to reach through however many levels. `extern`, a pointer, a typedef and a function return define no object and are exempt. **Flexible array members** turn on telling `int a[]`, which has no extent, from the GNU zero-length array `char d[0]`, which has one that happens to be zero: `unsized_array_levels` separates them, and conflating them would have rejected a shape that appears throughout system headers. Zero hits across a full CPython -O2 build. Tests: `diagnostics_incomplete_objects_and_misplaced_flexible_arrays_are_rejected` (ten) and `diagnostics_complete_objects_and_valid_flexible_arrays_are_accepted` (twenty-three, including three tentative-definition shapes, a mid-struct zero-length array, and a struct with a flexible array member used as an array element).

- [x] **#C113 — Naming a member of an atomic structure or union was not diagnosed.** **✓ fixed (2026-08-18).** **[probed]** `struct S { int a; }; _Atomic struct S s; s.a` compiled in silence; gcc warns *"accessing a member 'a' of an atomic structure"*. C11 6.5.2.3p5 makes it undefined behaviour rather than a constraint violation -- the access touches part of an object whose atomicity covers the whole of it, so the lock the type promises is not taken -- so a warning is the right severity and is what `cc/doc/TODO.md` had listed under "C11 Atomics -- remaining semantic validation" since the atomics work landed. It was the last item on that list, and the omission meant the one operation `_Atomic` exists to prevent was the one c17 did not mention. Both access forms warn, `p->m` judging the *pointee's* atomicity since that is the object it names, and the message distinguishes a structure from a union as gcc's does. What must stay silent is the mirror image and is what the accept-side test pins: it is the **object's** atomicity that counts, not the member's, so `struct { _Atomic int a; } s; s.a` is an ordinary access to an atomic member. Tests: `diagnostics_member_of_an_atomic_aggregate_warns` (seven, including a union, a nested member -- where gcc names the outer one -- and an atomic reached through a typedef) and `diagnostics_ordinary_member_access_stays_silent` (five).

- [x] **#C98 — A bit-field as wide as its own carrier read back as zero.** **✓ fixed (2026-08-18).** **[probed]** `struct { unsigned long long a:64; }` holding `0x0123456789ABCDEF` read back **0**, at `-O0` and `-O2` alike, on both targets; `gcc` is right. The mask was spelled `(1u64 << bit_width) - 1` at three sites in `emit_bitfield_load`/`emit_bitfield_store`, and Rust masks a shift amount to the operand's width -- so `1u64 << 64` is `1`, the mask is `0`, and every read was ANDed away. Width 63 was correct throughout, which is why 1,685 tests and the CPython gate never saw it: nothing in either uses a bit-field as wide as its declared type. Replaced by `bitfield_value_mask`, which shifts `u64::MAX` down instead, so the boundary is the case it is defined at rather than the case it overflows at. The same shape in the static-initializer byte merge (`linearize_init.rs`, a `u128` carrier overflowing at width 128) went with it. **Not fixed, and out of scope as a GNU extension:** an `__int128` bit-field is far more broken than this -- `bitfield_storage_type` has no arm for a 16-byte unit and falls through to `unsigned int`, so `unsigned __int128 a:128` produces assembler errors and a segfault. `validate_bitfield` admitted the type, so the gap was reachable; gcc handles it. **Closed differently after review (2026-08-18)**: the real limit is the *width*, not the type -- `unsigned __int128 a:64` agrees with gcc, while `a:100` read back a wrong value in a release build and **panicked the compiler** in a debug one, on `bitfield_value_mask`'s own assertion. `validate_bitfield` now refuses a width above 64 with a diagnostic naming the limit. That diverges from gcc, which supports these; a diagnostic beats a panic and a wrong answer, and `__int128` is a GNU extension out of scope for implementation. Tests: `diagnostics_bitfield_wider_than_its_carrier_is_rejected`, `diagnostics_bitfields_within_the_carrier_are_accepted`. Tests: `c89_bitfield_as_wide_as_its_carrier_round_trips` (both widths either side of the boundary, signed and unsigned, two full-width fields in one struct, and a field with ordinary members either side so an over-wide mask would corrupt a neighbour), plus the unit test `test_bitfield_value_mask_covers_the_full_carrier`.

- [x] **#C97 — An array with no extent was incompatible with one of any size.** **✓ fixed (2026-08-18).** **[probed]** C17 6.7.6.2p6 makes two array types compatible if their element types are and *both* have a constant size; a side with no extent imposes no size requirement. Comparing the extents for equality turned a legal call into a diagnostic: a parameter `int a[n][m]` decays to `int (*)[m]`, whose pointee has no extent, so passing an ordinary `int m[2][2]` drew *"passing argument 3 as 'int[]*' from 'int[2]*' incompatible pointer type"* where gcc is silent under -Wall -- and the program then compiled and ran correctly, which is the worst shape a warning can have. A mismatch between two constant extents, or in the element type, is still diagnosed. Does **not** reach `__builtin_types_compatible_p`, which still answers `int[]` against `int[2]` with 0 where gcc says 1; that is [[bug_types_compatible_identity]]'s family, not this. Tests: `diagnostics_array_of_unspecified_size_is_compatible`, `diagnostics_incompatible_array_pointers_are_still_diagnosed`.

- [x] **#C89 — `sizeof` through `typeof` answered 0.** **✓ fixed (2026-08-18).** **[probed]** `int n=4; int a[n]; sizeof(typeof(a))` and `sizeof(typeof(int[n]))` both answered **0** where gcc answers 16, and `typeof` appears in real system headers. A `typeof` yielded a bare `TypeId`, and `int[]`, `int[n]` and `int[m]` all intern to one type, so the extent did not survive it -- which also made a `typeof`-derived VLA indistinguishable from an incomplete array and forced #C90's completeness check to carry an explicit exemption for `typeof`, keeping this a wrong answer rather than a refusal. **The two operand forms needed different mechanisms, and that is the finding.** `typeof(type-name)` carries its extent expressions out of the specifier-qualifier list, the way #C52 carries a type-name's own -- declarator levels outermost, specifier levels innermost, because `typeof(int[n])[3]` is 48 and getting the order backwards makes `int[3][n]` and `int[n][3]` the same size, right for one shape and wrong for another. `typeof(expr)` has no extent to carry: the answer lives in the *declaration* of the object, which the linearizer already recorded, so `sizeof(typeof(E))` is rewritten to `sizeof(E)` -- sound because `sizeof` does not lvalue-convert, so no array decays and no qualifier matters, and neither compiler evaluates through `typeof`. With both in place the `from_typeof` exemption is **removed**, so `sizeof(typeof(struct never_defined))` and `sizeof(typeof(int[][n]))` are now refused as gcc refuses them, and `diagnostics_sizeof_of_a_typeof_is_not_rejected` -- whose VLA case multiplied by `* 0` to accommodate the wrong answer -- checks the real size. Eight sizes agree with gcc. Tests: `codegen_sizeof_through_typeof` (both operand forms, both dimension orders, and a second extent so nothing can be folding to a constant behind the test's back). _(Original finding below.)_ Since #C90 refuses `sizeof` of an incomplete type, and a `typeof`-derived VLA is indistinguishable from an incomplete array, the completeness check **deliberately exempts a type-name beginning with `typeof`**.

- [x] **#C90 — `sizeof(int[][n])` is accepted and answers 0.** **✓ fixed (2026-08-18), and it was not only that shape.** C17 6.5.3.4p1 forbids `sizeof` of any incomplete type, and c17 accepted every one. The array case is what kept it open: the type table cannot tell `int[n]` from `int[]`, both simply having no extent. The size expressions #C52 put on the type-name decide it -- a level with an expression is variably modified and complete, one without is incomplete -- so `sizeof(int[][n])`, `sizeof(int[])` and `sizeof(int[][3])` are refused while `sizeof(int[n])` and `sizeof(int[3][n])` are not. A struct or union declared and never defined is refused too. `void` and function types stay accepted, both giving 1: strict C forbids them, gcc allows them, and matching gcc is policy. Twelve operands agree with gcc. _(The expression form, `extern int a[]; sizeof a`, was left uncovered here and is closed by #C112, which gave the symbol the record of incomplete-versus-VLA this needed.)_ Tests: `diagnostics_sizeof_of_an_incomplete_type_is_rejected`, `diagnostics_sizeof_of_complete_types_is_accepted`. _(Original finding below.)_ **[probed]** gcc rejects it: *"invalid application of `sizeof` to incomplete type"* (6.5.3.4p1). #C52's predicate declines the run-time path here on purpose — two absent extents against one size expression, so pairing them would invent a plausible wrong number — which leaves the old answer rather than diagnosing it.

- [x] **#C91 — Pointer comparison is signed.** **✓ fixed (2026-08-18).** C17 6.5.8 compares pointers by address, and an address is unsigned; a pointer is not an integer type, so `is_unsigned` answered false and `a < b` emitted `setl` where gcc emits `setb`. Asserted against the emitted instruction on both targets, with signed and unsigned integer controls, because the case that distinguishes the two conditions -- two addresses straddling the sign bit -- cannot be reached from a portable program. Tests: `codegen_pointer_comparison_is_unsigned`, `codegen_pointer_and_enum_signedness`. _(Original finding below.)_ **[probed]** `int f(char *a, char *b){ return a < b; }` emits `setl` where gcc emits `setb`. C17 6.5.8 compares addresses, and two pointers straddling the sign bit therefore compare wrongly; ordinary in-object comparisons agree with gcc, which is why nothing has tripped over it. `is_unsigned` reports false for a pointer type because the question is only asked of integers, so the relational lowering needs its own answer rather than a wider predicate.

- [x] **#C92 — An enumeration whose values exceed `INT_MAX` stays signed.** **✓ fixed (2026-08-18).** `enum_underlying_type` was already choosing correctly -- `unsigned int` for `enum E { BIG = 0x80000000u }`, with the right size -- but the enum's own type carried no signedness, so an *object* of it was loaded and compared as signed: `e` read back -2147483648 and `e < 0` was true, while the constant `BIG` was right throughout. The enum type now takes the `UNSIGNED` modifier when its underlying type has one. An enumeration holding negatives is unaffected, and one needing 64 bits already worked. Test: `codegen_pointer_and_enum_signedness`. _(Original finding below.)_ **[probed]** `enum E { BIG = 0x80000000u };` gives `BIG == -2147483648` and `BIG > 0` false under c17, against 2147483648 and true under gcc; both size the enum at 4. c17 does warn ("enumerator value 2147483648 is outside the range of 'int'"), so this is not silent, but 6.7.2.2p4 requires the enumerated type to represent every member and an unsigned 32-bit type does. Adjacent to #C80, which widened the *underlying* type selection; the signedness half did not follow — `Type::enum_type` never sets `UNSIGNED`, so every enumeration answers signed.

- [ ] **#C62 — `__builtin_*_overflow` with mixed signedness and a 128-bit destination.** **Open, Minor.** The 128-bit lowering added for the review's finding examines the result directly, with the classic per-operation predicates, on operands converted to the destination type. Where that conversion is not value-preserving — a negative operand with an unsigned 128-bit destination, or an `unsigned __int128` one with a signed destination — the answer follows the converted value rather than the mathematical one, and diverges from gcc: `__builtin_add_overflow(-1, 5u, &u128)` says overflow where gcc says none. Deciding it correctly needs 129 bits, and nothing here is wider than 128. The coherent cases — same type throughout, and narrower operands widening exactly — all match gcc, including `MIN * -1` and `u64max²`, and are pinned by `codegen_checked_arith_128bit`.

- [x] **#C60 — `__attribute__((section(...)))` attaches to the declaration, not the declarator.** **Closed as misdescribed; see #C81.** The behaviour recorded here — the attribute reaching `a` — never existed: `int a, b __attribute__((section(".s")));` was a *parse error* at file scope, verified against a build of `054c8830`. #C81 fixes the parse and the per-declarator scoping the fix exposed.

- [ ] **#C50 — `-l xnet` and `-l y` fail to link.** **Open, Major — POSIX conformance.** 88089-88093: the libraries `c`, `l`, `m`, `pthread`, `rt`, `xnet` and `y` *shall be found* when named as a `-l` option-argument, and — except for the shared libc — need not exist as regular files. **[probed]** five of the seven link; `xnet` and `y` fail, because neither exists on glibc (their interfaces are in libc) and c17 forwards the name verbatim to the host linker. The conformance-matrix row claiming this was the host linker's job was wrong and is reopened above. Fix: map the seven standard names to host equivalents, dropping those whose interfaces libc already provides. Deliberately out of scope of the 2026-08-15 series.

- [ ] **#C51 — `__attribute__((transparent_union))` is not implemented.** **Open, Minor.** glibc declares every socket call with a union parameter carrying it, letting a caller pass any member type — `sendto(..., SAS2SA(&addr), ...)` hands a `struct sockaddr *` to a `__CONST_SOCKADDR_ARG`. c17 records nothing, so #C47's argument check cannot tell a transparent union from an ordinary one; it accepts an argument matching *any* member of a union parameter, which under-diagnoses an ordinary union parameter rather than rejecting every socket program on the platform. Assignment and `return` stay strict, since the attribute governs calls only. Implementing it properly means surviving a typedef's trailing attribute and passing the union as its first member; the ABI half is the risk.

- [ ] **#C56 — `void *` converts to and from a function pointer.** **Open, Trivial — a deliberate match to gcc, not an oversight.** C17 6.5.16.1p1 offers the `void *` carve-out for a pointer to an *object* type, so strict ISO C forbids `FP fp = dlsym(h, "x");` without a cast, and #C47's assignment check permits it because it tests the pointee for `void` without excluding `TypeKind::Function`. **[probed]** `gcc -std=c17` accepts both directions **silently**; only `-pedantic` objects, with "ISO C forbids assignment between function pointer and 'void *'". Diagnosing it would diverge from gcc, which this series matches by policy, and would fire on every `dlsym` call — a conversion POSIX requires to work (XSH `dlsym`). Left as it is; if c17 ever grows a `-pedantic`, this belongs behind it. Raised in review 2026-08-15.

- [x] **#C52 — `sizeof` of a variably-modified type-name answers 0.** **✓ fixed (2026-08-17).** The dimension expressions were computed correctly by `parse_declarator` and dropped one line later in `try_parse_type_name`, which returns a bare `TypeId` — and they cannot be recovered afterwards, because `int[n]`, `int[m]` and `int[]` all intern to one type. They now ride on `ExprKind::SizeofType`; `sizeof` gets a variably-modified-aware entry point and the other eight callers are untouched. `_Alignof` deliberately keeps the old one: 6.5.3.4p3 makes its result a constant, does not evaluate its operand, and it was already right. The size needed no new machinery — `record_vm_extents` and `vm_extent_size` already pair one expression with each absent extent for a declared `int a[n][4][m]`, so `int[3][n]`, `int[n][3]` and `int[n][m]` came out right as soon as the expressions arrived. **Two consequences were worse than the wrong number and are what the fix is really about.** The operand was never *evaluated*: `sizeof(int[f()])` called `f` zero times, where 6.5.3.4p2 requires one call per evaluation of the `sizeof` — now once at statement level, once per iteration in a loop, and not at all in a skipped `&&` operand or an untaken conditional arm. That last case forced `is_pure_expr` to change in the same commit, since reporting `sizeof` pure lets a conditional lower branchlessly and evaluate both arms (6.5.15p4). And the bogus 0 was still an integer *constant* expression, so `int z[sizeof(int[n])];` silently became a zero-length array; both constant folders now decline a variably-modified operand, which makes `z` the 64-byte VLA gcc has and makes `_Static_assert`, a `case` label and an enumerator reject it (`expr_is_runtime` gained the same arm so the `case` message stops blaming the compiler). `sizeof(int[][n])` — two absent extents, one expression, invalid C — is deliberately declined rather than mispaired. Tests: `codegen_sizeof_of_a_variably_modified_type_name`, `test_sizeof_variably_modified_type_name_carries_its_dimensions`, `test_unsized_array_levels`. _(Original finding below.)_ `sizeof(int[n])` is 0 where gcc evaluates it at run time. Measured before the type-name rework, so not a regression from it: the old type-name parser wrote a zero size for a non-constant dimension and `parse_declarator` writes an absent one, and neither carries the size expression to `sizeof`. Fixing it means giving a variably-modified type-name its size expression and evaluating it — a feature, not a repair. Scheduled into the 2026-08-16 series and then **not taken**: a VLA *variable*'s `sizeof` works by reading an element count the declaration stored, and a type-name has no declaration, so the dimension product has to be rebuilt from the declarator's size expressions. A version handling `int[n]` but not `int[3][n]` would be right for one shape and wrong for another, which is the failure mode the rest of that series existed to remove.

- [x] **#C53 — A trailing comma in a parameter list became an extra `int` parameter.** **✓ fixed (the *box* was stale: the fix landed in `6c418656` on 2026-08-17 and was never ticked; found by the 2026-08-18 probe series and closed then).** `void g(int, );` compiled, and the empty slot was taken as an implicit-`int` parameter, so the declaration meant `void(int, int)` -- which, with call arity checked, made the *correct* call `g(1)` the one rejected while `g(1,2)` was accepted. Now diagnosed: *"expected a declaration specifier or '...' after ','"*. C17 requires a declaration specifier or `...` after the comma; C23 allows the comma itself, which c17 does not implement.

- [ ] **#C54 — Read-only accessors on the type table are `#[cfg(test)]`-gated.** **Open, Minor — a source of duplication, not a defect.** `format_type`, `composite`, `array_size` were all test-only, so production code that needed them hand-rolled the same lookups inline; the 2026-08-15 series had to un-gate all three to name types in diagnostics. `cflow.rs:643` still carries its own fourth `format_type`. `array_size` was un-gated in the same way for the excess-initializer check. Relatedly, `parse_function_def` (`parse/parser.rs`) is `#[cfg(test)]`-only, so `cargo build` and `cargo test` compile different parser entry points — a stale call site there is invisible to a release build and only `clippy --all-targets` catches it. The same duplicate-implementation hazard #C44a removed from declarator parsing.

- [ ] **#C12 — `_FORTIFY_SOURCE` compiles but fortifies nothing.** **Open, Minor.** Originally diagnosed as one defect — `__builtin_object_size` answering `(size_t)-1` — it turned out to be six, each hidden behind the one before it, and only visible by fixing a layer and re-measuring. Four are now closed: real object sizes, implicit `__builtin___*_chk` declarations, asm label renaming (below), and `always_inline`. The two that remain are `__gnu_inline__` emitting no out-of-line definition, and folding `__builtin_object_size` *after* inlining rather than before — see `cc/doc/TODO.md` for the measurements. Not a conformance issue — the fortification is a glibc extension — but a security-relevant one for anyone who sets the flag expecting it to work.

- [x] **#C14 — Every `inline` function definition provided an external definition.** **Fixed.** **[probed]** an `inline` function in a shared header — the most ordinary use of `inline` in C — made any two-translation-unit program fail to link with `multiple definition of`; gcc links it. One omission caused it: `INLINE` was missing from `storage_class_mask` (`cc/parse/parser.rs`), so `FunctionDef::is_inline` was **always false** for every ordinary function definition, and with it every consumer downstream — the linkage decision, the C99 6.7.4p3 static-object diagnostics, and the inliner's `has_inline_hint`. Fixing that exposed a second gap: nothing could suppress emission at all, since the old code only *demoted linkage* while C99 6.7.4p6 and GNU inline both require emitting nothing. Now `Function::emit` gates both backends' emit loops, and the four spellings are distinguished — noting that C99 and GNU are **opposite** for `extern inline`, which is what `__attribute__((__gnu_inline__))` (previously unrecognized) selects. "Some declaration says `extern`" is deferred to the symbol, because the standard idiom puts that declaration *after* the definition. Tests: `codegen_inline_in_a_header_links_from_two_units`, `codegen_inline_header_with_one_extern_declaration`, `codegen_inline_spellings_emit_the_right_symbols`, `codegen_inline_definition_reached_through_a_pointer`, and a new two-translation-unit link helper, which the suite previously had no way to express.

- [x] **#C13 — An asm label on a declaration was parsed and ignored.** **Fixed.** **[probed]** `extern int myfn(int) __asm__("realfn");` made gcc emit `call realfn@PLT` and c17 `call myfn@PLT`: c17 accepted the syntax and then called the *declared* name. A wrong-symbol bug that links only by accident, when both names happen to exist — and the reason glibc's `__REDIRECT` fortify wrappers failed to link once #C12's earlier layers were in place. The label now rides on the symbol (`Symbol::asm_label`) and is resolved at the AST→IR boundary, so calls, definitions, globals, `extern_symbols`, GOT/TLS decisions and the macOS underscore all see one consistent name. Tests: `codegen_asm_label_renames_the_symbol` (runtime, incl. renamed definitions and function pointers) and `codegen_asm_label_reaches_the_assembly` (both ABIs, asserting the declared name is emitted nowhere).

### Not confirmed

- [x] **#L7 — K&R old-style parameter default-argument promotion — CLOSED, CLAIM NOT REPRODUCED.** An agent reported that `cc/parse/parser.rs:3405-3448` stores old-style parameter types verbatim without widening `float`→`double` / `char`/`short`→`int` per C99 6.9.1p8, predicting a miscompile. **[probed]** `int takes_float(a) float a; { return (int)(a*2); }` called as `takes_float(3.5)` returns **7** under both `c17` and `gcc`. The predicted failure does not occur on this path. Recorded for completeness; needs a narrower repro before being treated as a real finding, and should not be scheduled as work on the strength of the static reading alone.

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
| `LC_MESSAGES` | CONFORMS | #U7 closed; diagnostic bodies go through `gettext_args` with positional placeholders |
| `NLSPATH` (XSI) | CONFORMS | `gettext()` honors it, and #U7 wrapped c17's own diagnostics |
| `TMPDIR` (XSI) | CONFORMS | one `tempfile::TempDir` per run |

### ASYNCHRONOUS EVENTS

- [x] "Default" — no handlers installed, which is what the spec requires, so this row is closed rather than deferred. Temp files are not removed if the process is signalled; that is a tidiness gap, not a conformance one, since ASYNCHRONOUS EVENTS mandates default handling.

### STDOUT / STDERR

- [x] `-E` output to stdout — `cc/main.rs:387-446`.
- [x] Diagnostics to stderr only — `cc/diag.rs:383-402`; **[probed]**.
- [x] Warnings do not force a non-zero exit — `cc/diag.rs:409-412`.
- [x] The optional `"%s:\n"` per-file header is not emitted — spec says "may", so N/A.
- [x] `-E` `# <line> "<file>"` markers — #U5 closed.

### EXTENDED DESCRIPTION

- [x] Implicit `-l c` after all operands — delegated: the final link invokes the host `cc` (`cc/main.rs:614,1007`) without `-nostdlib`, so libc is always linked last. CONFORMS by delegation.

> **Architectural note (not a finding).** `c17` implements none of Phase 8 itself: it shells out to `as` for assembly (`cc/main.rs:601`) and to `cc` for linking (`614`, `1007`). There is no crt object selection, no dynamic-linker path resolution, and no explicit `-lc` or `-lgcc` anywhere in the crate. This is a legitimate choice — it satisfies the implicit-`-l c` and executable-permission mandates transitively, since every conforming host `cc` does those things — but it is an undocumented hard runtime dependency on `cc` and `as` being on `$PATH`, and it means c17 inherits the host driver's crt and runtime-library decisions rather than making its own. Worth stating in `cc/README.md`.

- [ ] `-l m`, `-l pthread`, `-l rt`, `-l xnet`, `-l l`, `-l y` forwarded verbatim — `cc/main.rs:629-631,1025-1027`. **This row was wrong; reopened as #C50.** "Resolution is the host linker's job" is not what 88089-88093 says: the seven standard libraries *shall be found* when named as a `-l` option-argument, and need not exist as regular files. **[probed]** `-l c`, `-l m`, `-l pthread`, `-l rt` and `-l l` link; **`-l xnet` and `-l y` fail**, because neither exists on glibc — their interfaces live in libc — and c17 forwards the name verbatim to the host linker.
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
| Predefined macros | CONFORMS — #P1, #P7 and #X8 all closed |
| C89 core | CONFORMS (declarators, bitfields, storage classes, tentative definitions, promotions) |
| C99 | CONFORMS for everything audited (VLAs, designated initializers, compound literals, flexible array members, `restrict`, `inline`, `_Bool`, `long long`, `_Complex`, `__func__`, UCNs, hex floats) |
| C11 | CONFORMS for everything audited — #X1, #X3, #X6 and #X7 are all closed, so `_Generic`, `_Atomic` through ordinary operators, the full `<stdatomic.h>` and the `CMPLX` macros join the rest |
| C17 | CLAIMED — `__STDC_VERSION__` is `201710L`, and it is the only language c17 compiles. C17 is DR-only; DR 412 (`_Static_assert` as a struct member) is honored at `cc/parse/parser.rs`, and DR 423 is live now that `_Generic` exists |
| Translation limits (C99 5.2.4.1) | CONFORMS — `Vec`/`BTreeMap`-backed throughout; `cc/tests/c99/translation_limits.rs` exercises 130-member structs, 20 params, 30 cases. **[probed 2026-08-15]** eleven of the 5.2.4.1 minimums measured directly against the built binary (127 nested blocks, 63 nested `#if`s, 12 pointer modifiers, 127 parameters, 4095 macros, a 4095-character string literal, 1023 `case` labels, 1023 struct members, a 63-character identifier, a 4095-character logical line): all pass. The twelfth — 63 nesting levels of *parenthesized declarators* — failed at level **2**, and is #C44. Residual risk: the recursive-descent parser has no explicit depth guard, so very deep nesting depends on stack size |
| Diagnostics for constraint violations (C17 5.1.1.3) | CONFORMS — and this row was **overstated until 2026-08-15**. "For everything audited" was carrying the whole claim: the audited set was small, and a 26-case probe of the built binary found **21 constraint violations accepted in total silence**, two of which silently miscompiled (`int x; double x;` emitted `.comm x,4,4`; `int *p; p = 1.5;` compiled to a `cvttsd2si`). All 21 are now diagnosed at gcc's severity — error where gcc errors, warning where gcc warns — across #C45-#C49. A 35-case matrix (21 violations, 14 accept-side controls) agrees with `gcc -std=c17` on every row. Two places stay deliberately *stricter* than gcc: `return` with a value in a `void` function, and a bare `return` in a non-`void` one, both genuine 6.8.6.4 violations |

### Extensions accepted

Not findings — POSIX permits extensions, and none of these change the meaning
of strictly-conforming source: `__attribute__`, statement expressions,
`typeof`, the `__builtin_*` family, `__c11_atomic_*`, GCC extended inline asm
(incl. `asm goto`), `__int128`, `_Float16`/`_Float32`/`_Float64`,
`__float128`/`_Float128` (IEEE binary128, with the `q` and `f128` suffixes), nullability
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
- [x] `_Atomic` via plain operators — covered three ways, because no one way suffices. `c11_atomic_operators_mega` covers every operator at every lock-free width through every lvalue shape; IR unit tests in `cc/ir/test_linearize.rs` assert the opcode, memory order and CAS-loop block structure against a non-atomic control built from the same AST; and `cc/tests/codegen/atomics_asm.rs` asserts the actual instructions on both arches, each with a plain-`int` control that must stay unlocked. The prediction was right: the pre-existing behavioural test passed against plain `movl` and still does, so it was supplemented rather than replaced.
- [x] `_Generic` — `cc/tests/c11/generic.rs` (selection, decay, qualifier stripping, typedef transparency, nesting, non-evaluation of both the controlling expression and the unselected arms, use as an integer constant expression), constraint violations in `cc/tests/diagnostics/mod.rs`, and parser unit tests in `cc/parse/test_parser.rs` asserting that the selected arm is returned verbatim. _(`CMPLX` is covered by `c11_cmplx_constructs_exactly`; Unicode literal prefixes by `cc/tests/c11/literals.rs`; incompatible typedef redefinition by `diagnostics_incompatible_typedef_redefinition_is_rejected`.)_
- [x] Header-search precedence between builtin headers and `-I`/dir-of-file (#P9) — `preprocessor_local_header_wins_over_builtin`, `preprocessor_dash_i_wins_over_builtin`, `preprocessor_builtin_headers_still_resolve`, plus `preprocessor_missing_include_is_still_an_error` so the shadowing tests cannot pass vacuously.
- [x] aarch64 `long double` round-trip (#H4) — `c17_long_double_round_trip`. Passes on x86_64; aarch64 CI settles the question.

Actively pinning current behavior (must change alongside the fix):
- [x] `cc/tests/c11/core.rs` asserted `__STDC_VERSION__ == 201112L`; it now asserts `201710L`, and `cc/tests/preprocessor/std_dialect.rs` covers the whole `-std=` matrix in both directions.
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
