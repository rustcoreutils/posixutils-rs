# POSIX.1-2024 Conformance Audits — `cc/` utilities

This file collects per-utility POSIX conformance audits for the C compiler
crate. Each audit follows the playbook in `audits.md`.

**First audited:** 2026-08-01 · **Last reconciled:** 2026-08-21 (ISO C rows re-probed)
**Crate:** `cc/` — 113,690 lines of implementation (excluding tests) +
41,949 lines of tests (1,815 `#[test]` functions)
**Spec slices:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/{c17,cflow,ctags,cxref}.md`

| Utility | Binary | Implementation | Spec slice | Spec pages |
|---|---|---|---|---|
| `c17` | `c17` (was `pcc`) | `cc/main.rs` (1488) + the whole compiler (~89 kloc) | `c17.md` (588 lines) | 2717–2730 |
| `cflow` | `cflow` | `cc/cflow.rs` | `cflow.md` (138 lines) | 2637–2640 |
| `ctags` | `ctags` | `cc/ctags.rs` | `ctags.md` (220 lines) | 2865–2869 |
| `cxref` | `cxref` | `cc/cxref.rs` | `cxref.md` (114 lines) | 2879–2881 |

## Status

**Open: 2.** Both are standing decisions rather than work items — #C55
(trigraphs off by default) and #C12 (`_FORTIFY_SOURCE`). #C157 and #C158 came
out of the 2026-08-21 ISO C re-probe and are both closed.

**Everything else is closed.** The per-finding entries used to be kept here
after they were fixed; they are not any more, because 252 of them made the file
unreadable for its actual purpose. What each fix was, and why, is in the commit
that made it — `git log --grep '#C123'` finds one by number. What survives here
is the part that is still a claim about the present: the conformance matrices
below, which cite a file and line for every requirement in each utility's spec
page, and the coverage notes that say where a requirement is still untested.

**How a claim in this file is established.** By probing the built binary
against the spec slice, not by reading the source and not by trusting an
earlier entry in this file — several findings were originally written from a
premise that a probe then disproved. Rows marked **[probed]** were checked that
way. A behavioural change is gated on the full workspace suite, zero clippy
warnings, `cargo fmt`, the CPython 3.12.9 `-O2` acceptance build (its own
`Tests result: SUCCESS` line, not the harness's exit status), and the aarch64
qemu differential sweep, which is the only check that sees the non-host target.

## Naming: `pcc` → `c17`

This audit was written against the name `c17` throughout, while the binary was
still shipped as `pcc`. The rename **has since landed** (see #U8): the binary,
its internal `--c17-*` flags, its diagnostics prefix, its `__VERSION__` string,
its assembly/DWARF producer strings, and its temp-file names all say `c17` now.
Everything below reads literally.

The rename was deliberately kept cosmetic, and this paragraph used to add that
it did not bring the language level with it -- that the binary named `c17` still
advertised `__STDC_VERSION__ == 201112L`. **That is stale (corrected
2026-08-21):** it reports `201710L`, matching gcc under `-std=c17`, for every
`-std=` spelling it accepts. `c17_version_macro_is_c17_whatever_std_says` pins
it.

Note the spec's own FUTURE DIRECTIONS (c17.md 88407–88411): *"Unlike all of the
other non-OB-shaded utilities in this standard, a utility by this name probably
will not appear in the next version of this standard. This utility's name is
tied to the current revision of the ISO C standard."* The name is a moving
target by design; `c17` is correct for POSIX.1-2024.

---

## What the markers mean

- **[probed]** — established by executing the built binary against the spec
  slice. The original audit was produced by read-only agents with no shell, so
  every Critical and Major finding was afterwards re-checked this way, and one
  agent claim did not survive it (#L7, K&R promotion — it never reproduced).
- **[static]** — read from the source only. Not less likely to be true, but not
  demonstrated end to end.

A row with neither marker is a plain citation: the file and line that
implements the requirement.

---

# `c17`

**Implementation:** `cc/main.rs` (1488) — driver; the compiler proper is
`cc/token/` (7.6k), `cc/parse/` (14k), `cc/ir/` (23k), `cc/arch/` (32k),
plus `cc/include/`, `cc/os/`, `cc/abi/`, `cc/target.rs`, `cc/rtlib.rs`.
**Tests:** `cc/tests/` — 231 `#[test]`s, 18,459 lines.

## TL;DR

**Rewritten 2026-08-19.** What stood here was the audit as first written, and
every claim in it had since been fixed: multi-file compilation, the four
missing mandated options, the `-E` line markers, `TMPDIR`, `__STDC_VERSION__`,
`_Generic`, `_Atomic` through ordinary operators, and the constraint-diagnostic
cluster. Leaving it in place made the document describe a compiler that no
longer existed. The old text survives in git history; what follows is the
state as probed against `gcc -std=c17` and the spec slice.

The driver conforms. `-B`, `-c`, `-D`/`-U`, `-E`, `-G`, `-g`, `-I`, `-L`,
`-l`, `-O`, `-o`, `-R`, `-s` are all present and effective; `-U` beats `-D`
regardless of order; the `""`-vs-`<>` header search order is right; every
operand form (`.c`, `.i`, `.a`, `.so`, `.o`) is handled and all operands reach
one link; `a.out` gets the mandated permissions; a compile error still compiles
the remaining operands and exits non-zero; `-E` carries the mandated
`# <line> "<file>"` markers and honours `-o`; and the seven standard `-l`
libraries are all found. 4 200 external identifiers compile against a
mandated minimum of 4 095.

The language is C17, and says so: `__STDC_VERSION__ == 201710L`.

**The sweep that used to be summarised here "finds no gap" was written from
reading, and it was wrong.** Re-probing it on 2026-08-21 -- running the built
binary against `gcc -std=c17`, case by case -- turned up eleven defects in
translation phases 1-4, a 6.4.7 implementation that did not exist, digraph
spellings silently discarded, `#__VA_ARGS__` keeping only its first argument,
a VLA miscompile -- `&a` on a variable-length array yielded the address of its
pointer slot rather than the array, at `-O0` and `-O2` alike, with no test
anywhere in the tree taking a VLA's address -- and two builtin headers
defining names their clauses do not give them. Review of that work then found
three more in the same area and two regressions the pass itself introduced;
see #C159 and the note above the code-generation section. All are fixed and each row of
the ISO C table now names the probe that established it.

One open finding came out of that same pass; see **Open**. And
the distinction the pass turned on is worth stating plainly, because it is the
one this document keeps getting wrong: a row backed by a code citation is a
claim about the source, not about the compiler.

The remaining items are diagnostic-quality or technical debt. Two findings are
**deferred indefinitely by maintainer decision** and are not oversights:

- **#C12** `_FORTIFY_SOURCE` fortifies nothing. Five of six layers are done;
  the sixth is folding `__builtin_object_size` after inlining, and
  `__OPTIMIZE__` has to land in the same change. Not a conformance issue --
  the fortification is a glibc extension.
- **#C55** trigraphs are off by default, where APPLICATION USAGE 88224 says a
  compiler that does so is "not conforming to POSIX.1-2024". Deliberate:
  replacement reaches inside string literals, so `"What??!"` becomes `"What|"`,
  and gcc and clang default them off for the same reason. The `--trigraphs`
  flag implements them exactly.

## Open

Two, and both are standing decisions rather than pending work. They are kept
here so neither gets re-raised as a to-do.

- [ ] **#C55 — Trigraphs are off by default.** **SETTLED by maintainer decision — not deferred, not awaiting anything, and not to be re-raised.** The c17 APPLICATION USAGE says it outright (88224): "Some c17 compilers *not conforming to POSIX.1-2024* do not support trigraphs by default." #P11 implemented them behind `--trigraphs` because replacement reaches inside string literals, so `"What??!"` becomes `"What|"` — which is exactly what C17 phase 1 specifies and what the POSIX RATIONALE laments without granting an exemption. A deliberate divergence, not a missing feature; the fix is to flip the default and offer an opt-out. Deliberately out of scope of the 2026-08-15 series.

- [ ] **#C12 — `_FORTIFY_SOURCE` compiles but fortifies nothing.** **Deferred indefinitely by maintainer decision (2026-08-19). Open, Minor.** Originally diagnosed as one defect — `__builtin_object_size` answering `(size_t)-1` — it turned out to be six, each hidden behind the one before it, and only visible by fixing a layer and re-measuring. Four are now closed: real object sizes, implicit `__builtin___*_chk` declarations, asm label renaming (below), and `always_inline`. The two that remain are `__gnu_inline__` emitting no out-of-line definition, and folding `__builtin_object_size` *after* inlining rather than before — see `cc/doc/TODO.md` for the measurements. Not a conformance issue — the fortification is a glibc extension — but a security-relevant one for anyone who sets the flag expecting it to work.

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

- [x] The seven standard libraries are all found — `cc/linkargs.rs`, consulted at `cc/main.rs`'s single `-l` emission point. **[probed]** all of `c`, `l`, `m`, `pthread`, `rt`, `xnet`, `y` link. This row previously said resolution was the host linker's job, which is not what 88089-88093 says; it was reopened as #C50 and is closed there.
- [x] External symbol significance ≥31 bytes, ≥4095 identifiers per TU — no artificial caps in `cc/symbol.rs`/`cc/strings.rs`.
- [x] "A library shall be searched when its name is encountered" — #U3 closed; a library named before the object that references it now correctly fails to resolve.
- [x] Programming environments (`getconf _POSIX_V8_*`, `*_CFLAGS`/`*_LDFLAGS`/`*_LIBS`) — **N/A to `cc/`**: this is a `getconf`/system-configuration obligation (88105-88179), not something the compiler binary implements. Flagged here only so it is not lost; it belongs to whichever crate owns `getconf`.

### EXIT STATUS / CONSEQUENCES OF ERRORS

- [x] `0` on success, `>0` on error — **[probed]** compile error exits 1; the only `exit(0)` calls are legitimate info queries (`--print-targets` etc.).
- [x] Link failure diagnosed to stderr with non-zero exit — `cc/main.rs:610-611,640-641,1037-1039`; **[probed]**.
- [x] Continue compiling remaining operands after an error — #U2 closed.

### ISO C language conformance summary

**Re-probed 2026-08-21.** Every row below was checked by running the built
binary against `gcc -std=c17` on a purpose-written case, not by reading the
source. That distinction is the whole point of this pass: the rows that had
been written from a code citation rather than a probe are exactly the ones
that turned out to be wrong.

What the re-probe found, in one day: eleven defects in translation phases 1-4
(a row that said "CONFORMS -- handled in `nextchar`/`peekchar`"), a missing
6.4.7 implementation this table did not mention at all, digraph spellings
discarded (a row marked **[probed]**, where the probe had only checked that
digraphs were *accepted*), two VLA defects including a silent miscompile
(under a row claiming "VLAs in every position"), and `#__VA_ARGS__` keeping
only its first argument. All are fixed; the numbers are #C150-#C156 and
`git log --grep` finds each one.

The lesson is recorded rather than the individual findings: **a row here means
nothing unless it names the probe that established it.** Rows are now written
as what was run.

| Area | Status | Probe |
|---|---|---|
| Translation phase 1 (trigraphs) | CONFORMS behind `--trigraphs`, off by default (#C55) | all nine sequences accepted with the flag, rejected without |
| Translation phase 2 (line splicing) | CONFORMS **[probed 2026-08-21]** | splices inside an identifier, inside a UCN's hex digits, between `\` and `u`, inside a string, and inside a macro *name* -- all agree with gcc. Was **not** conforming until #C150: three hand-written copies of the rule disagreed, and a splice before a UCN silently ate source characters |
| Translation phase 3 (comments) | CONFORMS **[probed 2026-08-21]** | `//`, `/* */` spanning lines, `/*/`, comment-as-one-space in `#` stringification; and in assembly, where `'` used to open a literal inside a comment (#C152) |
| 6.4.6 digraphs | CONFORMS **[probed 2026-08-21]** | `<: :> <% %> %: %:%:` each keep their own spelling through `#` and `-E` while meaning the primary token as syntax, as a directive introducer and as `##`. The spelling half was wrong until #C154 |
| 6.4.7 header names | CONFORMS **[probed 2026-08-21]** | `#include <sub//t.h>` and `#include <it's.h>` compile and run. Not implemented at all until #C153 -- a header name was lexed as ordinary tokens, so `//` became a comment and `'` opened a literal |
| Directives, computed `#include`, `_Pragma`, `#include_next` | CONFORMS **[probed 2026-08-21]** | computed include, `__has_include` present and absent, `#line` renaming and renumbering |
| Include cycle/depth guard | CONFORMS **[probed]** | a self-including header exits 1, no hang (`max_include_depth = 200`) |
| Macro expansion (6.10.3) | CONFORMS **[probed 2026-08-21]** | blue paint, rescanning (`f(2)(9)`), mutual recursion, `##` placemarkers, empty arguments, arity diagnostics, `__VA_ARGS__`. `#__VA_ARGS__` kept only its first argument until #C156. One divergence remains, pinned by `preprocessor_va_args_loses_space_before_a_separator`: the splitter discards the separating comma, so `V(a , b)` stringifies as `"a, b"` where gcc gives `"a , b"` |
| `#if` constant expressions | CONFORMS **[probed 2026-08-21]** | `intmax_t` arithmetic, arithmetic right shift, signed wraparound, unqualified-`u` promotion, short-circuit around `1/0`, character constants, `0xFFFFFFFFFFFFFFFF` |
| Predefined macros | CONFORMS **[probed 2026-08-21]** | `__STDC__`, `__STDC_VERSION__ == 201710L`, `__STDC_HOSTED__`, `__FILE__`, `__LINE__`, `__DATE__`, `__TIME__`, `__COUNTER__` |
| C89 core | CONFORMS | declarators, bitfields, storage classes, tentative definitions, promotions. **This row was overstated until 2026-08-21:** an init-declarator list whose first declarator was a function -- `int f(int), g(int);` -- was a syntax error, which is ISO C and so a conformance defect. Found by building sparse, not by re-reading this file |
| C99 | CONFORMS **[probed 2026-08-21, widened after review]** | VLAs: address-of, deref, `sizeof` at depths 0-2, variably modified `typedef`, VLA parameters, and pointer arithmetic in all five spellings -- `p++`, `++p`, `p += 1`, `p + 1`, `1 + p` -- their decrementing forms, and differences whose operands are not bare identifiers. `c99_address_of_a_vla_is_the_array_address`, `c99_deref_of_a_pointer_to_a_vm_array` and `c99_vm_pointer_arithmetic_every_spelling` are what back this row. Also probed: designated initializers, compound literals, flexible array members, `restrict`, `inline`, `_Bool`, `long long`, `_Complex`, `__func__`, UCNs, hex floats |
| C11 | CONFORMS **[probed 2026-08-21]** | one program exercising `_Generic` (including `default:`), `_Atomic` through ordinary operators and through `<stdatomic.h>`, `_Static_assert`, `_Thread_local`, `alignas`/`alignof`/`max_align_t`, anonymous members, `CMPLX`, and all three Unicode literal prefixes |
| C17 | CLAIMED | `__STDC_VERSION__` is `201710L`, and it is the only language c17 compiles. C17 is DR-only; DR 412 (`_Static_assert` as a struct member) is honored at `cc/parse/parser.rs`, and DR 423 is live now that `_Generic` exists |
| Translation limits (C99 5.2.4.1) | CONFORMS **[re-probed 2026-08-21]** | the twelve minimums, including the one that used to fail: 63 nesting levels of parenthesized declarators now compile (#C44 closed). Residual risk unchanged -- the recursive-descent parser has no explicit depth guard, so very deep nesting depends on stack size |
| Identifiers (C17 6.4.2.1, Annex D) | CONFORMS **[probed 2026-08-21, exhaustively]** | The Annex D table is transcribed from GCC's `libcpp/ucnid.tab` (`[C11]`+`[C11NOSTART]`), which states it reproduces ISO/IEC 9899 Annex D; Clang's independent table agrees on every code point, and so does c17's -- compared over all 1.1M code points in both positions, not sampled. Extended characters are accepted written directly as well as as a UCN (#C158), and both spellings name the same identifier. Until then the direct spelling was rejected outright, and the UCN path applied only 6.4.3p2 -- what a UCN may *name* -- so it admitted characters no identifier may contain and let a combining mark come first. **Two deliberate divergences from GCC's binary:** U+FD3E and U+FD3F, which Annex D excludes and which GCC's own table and Clang's both omit |
| Header namespace (C17 7.1.3) | CONFORMS **[probed 2026-08-21]** | `<string.h>`, `<stdio.h>` and `<time.h>` declare what their clauses give them and leave the rest of the namespace alone. Was **not** conforming until #C157: the builtin `<stddef.h>` and `<stdarg.h>` ignored glibc's `__need_*` protocol, so `<string.h>` declared `wchar_t`, `ptrdiff_t` and `max_align_t` and `<stdio.h>` defined the `va_*` macros. `va_list` from `<stdio.h>` is not a leak -- glibc declares it under `__USE_XOPEN2K8`, as POSIX requires, and gcc's default `gnu17` mode does the same |
| Diagnostics for constraint violations (C17 5.1.1.3) | CONFORMS **[probed 2026-08-15]** | a 35-case matrix -- 21 constraint violations, 14 accept-side controls -- agrees with `gcc -std=c17` on every row. Two places stay deliberately *stricter* than gcc: `return` with a value in a `void` function, and a bare `return` in a non-`void` one, both genuine 6.8.6.4 violations |
| Code generation | **NOT ESTABLISHED THE SAME WAY** | see *Code generation evidence* below |

> **A "[probed]" row is only as good as the cases the probe ran.** This row
> first said "pointer arithmetic" on the strength of a probe that had tried
> `p + 1` and `++p`. Review found that `p++` and `p += 1` did not move the
> pointer at all, and that a difference whose left operand was not a bare
> identifier divided by a compile-time size of zero and raised SIGFPE -- three
> more defects (#C159) under a row that had just been rewritten to say it was
> probed. Naming the probe is necessary; it is not sufficient. Where a rule
> has several spellings, the row has to say it covered them, and the test has
> to be the kind that enumerates rather than samples.

### Code generation evidence

Conformance of the *language* is now probed. Correctness of the *output* is
not, and this table should not be read as if it were.

What backs it today: CPython 3.12.9 built and tested at `-O2`
(`run=40,817 skipped=1,857 -- SUCCESS`), 9,200-odd workspace tests, and only
three optimization passes in existence (`instcombine`, `dce`, `inline`), which
is a real if backhanded argument -- most miscompiles live in the passes that
are not there.

What does not back it:

- **No adversarial suite has ever been run.** GCC's `gcc.c-torture/execute`
  and the clang suite are both listed as "Not run against c17" in
  `cc/doc/TODO.md`. A hand-written differential probe of nine ordinary
  constructs (bitfields, unions, struct ABI, varargs, floats, VLAs, control
  flow, `setjmp`, alignment) at `-O0` and `-O2` found a miscompile in the
  first hour (#C155). CPython is one program in a conservative style and will
  never reach `&vla`.
- **The acceptance gate is manual.** CI runs `cargo test` only. The CPython
  build and the aarch64 qemu differential sweep are both run by hand, so
  "40,817 SUCCESS" is a periodic claim, not a continuous one.
- **aarch64 has no equivalent of CPython.** `compile_and_run` always targets
  the host, so aarch64-only defects surface only in the manual sweep.

### Extensions accepted

Not findings — POSIX permits extensions, and none of these change the meaning
of strictly-conforming source: `__attribute__`, statement expressions,
`typeof`, the `__builtin_*` family, `__c11_atomic_*`, GCC extended inline asm
(incl. `asm goto`), `__int128`, `_Float16`/`_Float32`/`_Float64`,
`__float128`/`_Float128` (IEEE binary128, with the `q` and `f128` suffixes), nullability
qualifiers, `#include_next`, `#warning`, digraphs, the C23 one-argument
`_Static_assert`, case ranges, designated-initializer ranges, computed goto,
and the omitted middle operand `a ?: b`. See `cc/doc/ATTR.md` and
`cc/doc/BUILTIN.md`.

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

- [x] The 2026-08-21 ISO C re-probe — every finding it produced carries a test: `test_ucn_across_line_splices`, `test_ucn_splice_disabled`, `test_digraph_hash_not_hashhash_keeps_line_count`, `test_column_saturates_on_very_long_line`, `test_unterminated_literal_yields_the_newline_it_ate`, `test_asm_apostrophe_is_not_a_literal`, `test_asm_comments_are_stripped`, `test_write_token_emits_source_bytes`, `test_tokens_to_source_bytes_keeps_literal_bytes`, `test_header_name_is_opaque`, `test_header_name_only_where_one_can_appear`, `test_digraph_keeps_its_own_spelling` (lexer unit tests); `preprocessor_non_ascii_literal_survives_byte_for_byte`, `preprocessor_synthesized_literals_use_payload_form`, `preprocessor_non_ascii_header_name_opens`, `preprocessor_error_message_spells_every_token`, `preprocessor_u8_prefix_survives`, `preprocessor_header_name_is_one_token`, `preprocessor_digraph_spelling_survives`, `preprocessor_digraphs_still_mean_their_primary_tokens`, `preprocessor_stringified_va_args_keeps_every_argument` (`cc/tests/preprocessor/conformance.rs`); `driver_asm_apostrophe_in_comment` (`cc/tests/driver/mod.rs`); `c99_address_of_a_vla_is_the_array_address`, `c99_deref_of_a_pointer_to_a_vm_array` (`cc/tests/c99/features.rs`).

Actively pinning current behavior (must change alongside the fix):
- [x] `cc/tests/c11/core.rs` asserted `__STDC_VERSION__ == 201112L`; it now asserts `201710L`, and `cc/tests/preprocessor/std_dialect.rs` covers the whole `-std=` matrix in both directions.
- [x] `preprocessor_va_args_loses_space_before_a_separator` pins the one divergence left in `#__VA_ARGS__`: the argument splitter discards the separating comma, so the space before it is gone. Closing it means carrying the separator's spacing out of `collect_macro_args`, and the test must change with the fix.
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
- [x] `file.i` → **not** re-preprocessed — #F10; a `.i` operand goes through the preprocessor's `.i` mode, which consumes linemarkers and pragmas and expands nothing. Sending it *straight to the parser*, as the first fix did, made every real `-E` operand a syntax error.
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

**All findings are closed** (corrected 2026-08-21 — this paragraph still
described the three that were fixed). The tags-file format is right where it
counts: three tab-separated fields, the `/^...$/` anchored pattern, correct
`\/` and `\\` escaping in the correct order, the `M`-prefix rule for `main`,
and correct omission of the two alternate formats the spec forbids. A tag name
appearing in more than one file keeps every entry (#T2); an input error exits
non-zero (#T1); and `-x` emits the mandated `"%s %d %s %s"` with single spaces
and the source line's leading indentation intact (#T3).

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
