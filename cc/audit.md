# POSIX.1-2024 Conformance Audits — `cc/` utilities

This file collects per-utility POSIX conformance audits for the C compiler
crate. Each audit follows the playbook in `audits.md`.

**Date:** 2026-08-01
**Crate:** `cc/` — 89,049 lines of implementation (excluding tests) +
18,459 lines of tests (231 `#[test]` functions)
**Spec slices:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/{c17,cflow,ctags,cxref}.md`

| Utility | Binary | Implementation | Spec slice | Spec pages |
|---|---|---|---|---|
| `c17` | `pcc` | `cc/main.rs` (1488) + the whole compiler (~89 kloc) | `c17.md` (588 lines) | 2717–2730 |
| `cflow` | `cflow` | `cc/cflow.rs` (692) | `cflow.md` (138 lines) | 2637–2640 |
| `ctags` | `ctags` | `cc/ctags.rs` (349) | `ctags.md` (220 lines) | 2865–2869 |
| `cxref` | `cxref` | `cc/cxref.rs` (595) | `cxref.md` (114 lines) | 2879–2881 |

---

## Naming: `pcc` → `c17`

`ALIASES.md` records that posixutils-rs's `pcc` implements POSIX's `c17`. This
audit is written against the name `c17` throughout, per the maintainer's stated
intent to rename the binary. The rename itself is tracked as **#U8**.

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

**4. `gettext()` is a workspace-wide no-op.** The vendored `gettext-rs` shim
forwards `setlocale` to libc but `gettext()` is an identity passthrough. All
four utilities call `setlocale`/`textdomain` correctly, and wrap their clap
help strings in `gettext()`, but runtime diagnostics are hardcoded English.
This is a pre-existing workspace condition, not a `cc/` regression; recorded
once here rather than as four separate findings. Items: #U7, #T5, #F11, #R10.

---

# `c17` (shipped as `pcc`)

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
of predictable `/tmp/pcc_<pid>.*` paths.

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

- [ ] **#U1 — Multi-translation-unit compilation is broken; `.o`/`.a`/`.so` operands are dropped.** `cc/main.rs:918-1057` (esp. 920-922, 1049-1054) and `596-648`. With any `.c`/`.i` operand present, the driver loops `for path in &source_files { process_file(...) }`, and each iteration independently assembles *that file alone* and links it *alone* to `exe_file`. `object_files` (line 922) is only consumed in the `source_files.is_empty()` branch. **[probed]** `pcc m1.c m2.c -o mA` → `undefined reference to 'helper'`, no output produced, exit 1. `pcc m1.c m2.o -o mB` → same. This breaks spec EXAMPLE 1 (`c17 foo.c bar.o`) and EXAMPLE 3 (`c17 -L /a/b/c main.o a.c -l Q b.c -l p`) outright; `c17 -o foobar foo.o bar.o` (objects only) does work. Fix: restructure `main()` into two phases — compile every source operand to a `.o`, then perform exactly one link that includes every produced object plus every `.o`/`.a`/`.so` operand, in argument order.
- [ ] **#U2 — A compile error on one source operand aborts the run instead of continuing.** `cc/main.rs:1049-1054` calls `std::process::exit(1)` on the first `Err`. CONSEQUENCES OF ERRORS (88185-88187): *"it shall write a diagnostic ... and continue to compile other source code operands, but it shall not perform the link phase and it shall return a non-zero exit status."* **[probed]** `pcc -c bad.c good.c` produces **neither** `bad.o` nor `good.o`; `good.c` is never attempted. Fix: accumulate an error flag, `continue` past a failing file, gate the link on that flag, exit non-zero at the end.
- [ ] **#P2 — The null directive `#` deletes the following line of source.** `cc/token/preprocess.rs:892-917`. `handle_directive` unconditionally does `iter.next()` to fetch the directive name without checking `pos.newline`, so a bare `#` (a valid no-op per C17 6.10p7) consumes the *first token of the next line*, misclassifies it as a directive name, and `skip_to_eol` eats the rest of that line. **[probed]** input `#\nint kept_one;\nint kept_two;` produces only `int kept_two;` plus a bogus `warning: unknown preprocessor directive #int`. Silent source deletion. Fix: if the fetched token has `pos.newline == true` (or `iter.next()` is `None`), treat as the null directive and return.
- [ ] **#X1 — `_Atomic` objects accessed via ordinary operators compile to non-atomic code.** `cc/ir/ssa.rs:148` is the only consumer of `is_atomic` anywhere in the IR or codegen (zero hits under `cc/arch/`), and its sole effect is to block SSA register promotion — the same treatment as `volatile`. **[probed]** on `atomic_int g`: `g += 1` emits `movl (%r11),%eax` / `movl %ecx,(%r11)` with **no `lock` prefix and no fence**, while `atomic_fetch_add(&g,1)` correctly emits `lock xaddl`. `g = 5` emits a plain `movl` (not a seq_cst store). This violates C11/C17 6.5.16.2p3 (atomic compound assignment is a single atomic RMW) and the basic data-race freedom guarantee of 6.7.2.4/6.7.3. Code that looks correct races silently. Fix: in the linearizer, detect an `_Atomic`-qualified lvalue in assignment/compound-assignment/`++`/`--` and emit `AtomicStore`/`AtomicFetchAdd`/`AtomicFetchSub`/CAS-loop IR (those opcodes already exist) with `memory_order_seq_cst`.
- [ ] **#P1/#X2 — `__STDC_VERSION__` is `201112L`; the compiler cannot claim C17.** `cc/token/preprocess.rs:549`, hardcoded, with no `-std=` gate (`-std=` is accepted and discarded at `cc/main.rs:710-712`). **[probed]** `int v = __STDC_VERSION__;` preprocesses to `201112L`. This is the load-bearing blocker for the `pcc`→`c17` rename: any source doing `#if __STDC_VERSION__ >= 201710L` takes the wrong branch. Fix: add `-std=c17|c11|c99|gnu*` threading a `CStd` enum into `init_predefined_macros`, defaulting to `201710L`.
- [ ] **#L1 — Implicit-int is silently accepted for declarations and function definitions.** `cc/parse/parser.rs:1954` (`base_kind.unwrap_or(TypeKind::Int)`), reached unconditionally from `parse_external_decl` at `cc/parse/parser.rs:3031`. C99 removed implicit-int; 6.7.2p2 makes "at least one type specifier shall be given" a constraint requiring a diagnostic. **[probed]** `f(void){return 1;}` compiles clean with zero diagnostics. Fix: diagnose when `base_kind.is_none() && typedef_base.is_none()` before defaulting.

### Major

- [ ] **#U3 — `-L`/`-l`/`-R` ordering relative to each other and to pathname operands is architecturally lost.** `cc/main.rs:78` (`files: Vec<String>`), `199-203`, `624-636`, `1016-1031`: clap collects each flag into its own `Vec`, then the link line emits *all* `-L` then *all* `-l`, always after all objects. The spec deviates from XBD 12.2 specifically to say this order is significant (87866-87867), and EXAMPLE 3 depends on it. **[static]** — the source structure makes the interleaving unrecoverable after parsing. Fix: pre-scan raw argv for `-L`/`-l`/`-R`, preserving a single ordered stream mirrored onto the link command.
- [ ] **#U4 — `-B mode`, `-G`, `-R directory`, and `-s` are unimplemented.** 4 of the 14 mandated options. **[probed]** each is rejected by clap as `unexpected argument`. `--shared`/`-fPIC` exist as GCC-compat long flags but are not the POSIX short options, and there is no runpath emission (`-R`) or strip (`-s`) path at all. Fix: add all four; wire `-G` to the existing `--shared`/PIC path, `-R` to `-Wl,-rpath`, `-s` to a strip step, `-B dynamic` to library-selection preference.
- [ ] **#U5/#P8 — `-E` emits none of the mandated `# <line> "<file>"` markers.** `cc/main.rs:356-361`, `401-408` explicitly skip tokens whose text starts with `<STREAM`. STDOUT (88032-88038) says the `-E` output *shall* contain at least one such line for each file processed via `#include`; RATIONALE (88370-88374) states the purpose is makefile dependency generation. **[probed]** `pcc -E t.c | grep -c '^# [0-9]'` → `0`. Fix: emit `# {line} "{path}"` on stream transitions instead of discarding the markers.
- [ ] **#U6 — `TMPDIR` is ignored; temp files use predictable names in a world-writable directory.** `cc/main.rs:567,600,941,947` hardcode `format!("/tmp/pcc_{}.s", process::id())` etc. **[probed]** grep for `TMPDIR` across `cc/*.rs` returns zero matches; a run leaves `/tmp/pcc_<pid>.o`. ENVIRONMENT VARIABLES (88020-88022) requires `TMPDIR` to override the temp directory. Separately, a predictable name in `/tmp` is a symlink/pre-creation hazard. Fix: honor `TMPDIR`, and create temp files with `O_EXCL` + randomized names (or use the `tempfile` crate already in dev-dependencies).
- [ ] **#P6 — `#if` arithmetic is signed-`i64` only; large unsigned constants silently become 0.** `cc/token/preprocess.rs:3732-3744` (`i64::from_str_radix(...).unwrap_or(0)`). C17 6.10.1p4 requires `intmax_t`/`uintmax_t` arithmetic. **[probed]** `#if 0xFFFFFFFFFFFFFFFF` takes the *false* branch. Any `SIZE_MAX`/`UINTMAX_MAX`-style feature test silently misfires. Fix: parse into `u64`/`i128` and thread signedness through `ExprEvaluator`.
- [ ] **#P9 — Builtin headers shadow the user's own headers for quoted includes.** `cc/token/preprocess.rs:1694-1762`: `get_builtin_header` is consulted at 1701-1709, *before* the quote-form same-directory lookup at 1721-1726. The `""` form must search the including file's directory first (87905-87910). **[probed]** a local `stddef.h` next to the source is ignored in favor of the builtin. Any project with its own `limits.h`/`float.h`/`stdbool.h`/`complex.h`/`iso646.h`/`stddef.h` is silently miscompiled. Fix: for quote-form includes, search dir-of-file and `-I` dirs before builtins.
- [ ] **#X3 — `_Generic` is unimplemented.** Zero matches in `cc/parse/expression.rs`, `ast.rs`, `parser.rs`. **[probed]** `_Generic(1, int: 1, default: 0)` → `error: undeclared identifier '_Generic'` then a cascade of parse errors. Blocks strictly-conforming C11/C17 source and any real `<tgmath.h>`. Fix: add as a primary-expression production, lvalue-convert the controlling expression's type per 6.5.1.1p2, match by type compatibility, enforce the "no two compatible associations / at most one default" constraints, and emit only the selected branch.
- [ ] **#L2 — Duplicate `switch` case labels are not diagnosed.** `cc/ir/linearize_stmt.rs:1189-1220`, map built at `1142-1146`, no uniqueness check. C99 6.8.4.2p3 is a constraint. **[probed]** `switch(x){case 1: ...; case 1: ...}` compiles clean; which block wins is arbitrary. Fix: detect duplicates in `collect_switch_cases` and diagnose.
- [ ] **#L3 — `return` is never checked against the function's return type.** `cc/ir/linearize_stmt.rs:70-113`. **[probed]** both `void f(void){ return 1; }` and `int f(void){ return; }` compile clean. C99 6.8.6.4p1 makes both constraint violations. Fix: add the two symmetric checks.
- [ ] **#L6 — Call argument count is never validated against a visible prototype.** `cc/parse/expression.rs:1442-1512` builds the `Call` node with no arity check; `cc/ir/linearize.rs:2325` bounds-checks `arg_idx < params.len()` (written knowing they can diverge) but never flags it. C99 6.5.2.2p2 constraint. **[probed]** both `g(1)` against `int g(int,int)` and `g(1,2,3)` against `int g(int)` compile clean. Fix: compare `args.len()` to `params.len()` (accounting for variadics) and diagnose.
- [ ] **#L5 — `typedef` of a VLA type silently loses the size expression.** `cc/parse/parser.rs:1599-1611`: array declarators store non-constant dimensions as `array_size: None` plus a side-channel `vla_sizes: Vec<Expr>`, which every ordinary declarator path forwards but the `is_typedef` branch never inspects. The typedef becomes indistinguishable from an incomplete array `int a[]`. **[probed]** `typedef int arr_t[n]; arr_t x; x[0]=1;` compiles clean with no diagnostic. Fix: thread `vla_sizes` through the typedef symbol, or reject typedef'd VLAs with a diagnostic.
- [ ] **#X4 — Incompatible `typedef` redefinition is silently accepted, keeping the first type.** `cc/parse/parser.rs:3651-3657` (pattern repeated at ~3156-3170, 3355, 3510, 3730): on `declare()` returning `Err`, the new declaration is discarded and the *existing* symbol reused, with no type-compatibility check. C11/C17 6.7p3 legalizes redefinition only for *compatible* types. **[probed]** `typedef int foo; typedef char foo; foo x;` compiles clean — strictly worse than C89, where any redefinition was flagged. Fix: compare against the existing type and diagnose a mismatch.
- [ ] **#X5 — Unicode literal prefixes `u8""`, `u""`, `U""`, `u''`, `U''` are not lexed.** `cc/token/lexer.rs:634-638` special-cases only `name == "L"`. **[probed]** `u8"x"` → `error: undeclared identifier 'u8'` followed by a confusing parse error, rather than a clear diagnostic. `<uchar.h>` is not bundled (the system one is picked up). Fix: recognize `u8`/`u`/`U` alongside `L`; either implement `char16_t`/`char32_t` semantics or reject with an explicit diagnostic.
- [ ] **#X8 — No `__STDC_NO_*` macro is ever defined, including for genuinely absent features.** **[probed]** `__STDC_NO_ATOMICS__`, `__STDC_NO_THREADS__`, `__STDC_NO_COMPLEX__`, `__STDC_NO_VLA__`, `__STDC_UTF_16__`, `__STDC_UTF_32__`, `__STDC_IEC_559__`, `__STDC_ISO_10646__` are all undefined. For atomics/complex/VLA that is correct (they are supported). But C17 requires `__STDC_NO_THREADS__` to be defined when `<threads.h>` is unavailable, and pcc bundles no `threads.h` — it relies entirely on the host libc, so on a host without one, portable code cannot feature-test and falls off a cliff. Fix: probe host `<threads.h>` per-target and define `__STDC_NO_THREADS__` when absent.
- [ ] **#P7 — `__STDC_IEC_559__`, `__STDC_IEC_559_COMPLEX__`, `__STDC_ISO_10646__` are documented as implemented but do not exist.** `cc/doc/c99-checklist.md:793-795` marks all three `[x]`; a project-wide grep finds zero hits outside that doc. **[probed]** all three undefined. Since pcc's floats are native IEEE-754, `__STDC_IEC_559__` *should* legitimately be defined; its absence makes conforming numeric code take a needlessly conservative path. Fix: define them, or correct the checklist.
- [ ] **#P3 — Incompatible macro redefinition is not diagnosed.** `cc/token/preprocess.rs:1104-1207` overwrites via `HashMap::insert` unconditionally; the existing test `test_macro_redefinition` (4439-4446) documents the silent-override behavior as intended. C17 6.10.3p2 is a constraint requiring a diagnostic unless replacement lists are identical. **[static]** Fix: compare old vs. new on redefinition.
- [ ] **#P4 — Function-like macro argument-count mismatch is not diagnosed.** `cc/token/preprocess.rs:2701-2763`, `2765-2924` use `args.get(idx).cloned().unwrap_or_default()` with no count check. C17 6.10.3p4 constraint. **[static]** Fix: validate arity at the `expand_function_macro` call sites.
- [ ] **#P5 — `#`/`##` placement constraints are unchecked.** `cc/token/preprocess.rs:1210-1330` never rejects `##` at the start/end of a replacement list (6.10.3.3p1) or a `#` not followed by a parameter in a function-like macro (6.10.3.2p1); both become literal tokens. **[static]** Fix: add both checks in `tokens_to_macro_body`.
- [ ] **#X6 — `<stdatomic.h>` is missing most mandated typedefs and `atomic_is_lock_free`.** `cc/include/stdatomic.h:25-37` defines only the fixed-width base types. Absent: `atomic_intptr_t`, `atomic_uintptr_t`, `atomic_size_t`, `atomic_ptrdiff_t`, `atomic_wchar_t`, `atomic_char16_t`, `atomic_char32_t`, all `atomic_{u,}int_{least,fast}N_t` (C11 7.17.6.1), and `atomic_is_lock_free()` (7.17.5.1). **[static]** `atomic_size_t` in particular is common in lock-free code. Fix: add the typedefs and the lock-free query.
- [ ] **#X7 — `<complex.h>` is missing the `CMPLX`/`CMPLXF`/`CMPLXL` macros.** `cc/include/complex.h` defines `I` via `__builtin_complex` but not the C11 7.3.9.5-7 constructors, which exist precisely so `x + y*I` (which can corrupt `NaN`/`Inf` operands) has an exact alternative. **[static]** `cc/doc/c99-checklist.md:645` claims them done; zero hits for `CMPLX` anywhere under `cc/tests`. Fix: define all three in terms of the existing `__builtin_complex`, and correct the checklist.
- [ ] **#H4 — aarch64 `long double` load/store collapses to 64-bit, contradicting its own 128-bit ABI.** `cc/arch/aarch64/float.rs:24-33` maps `TypeKind::Double | TypeKind::LongDouble => FpSize::Double`, while the generic default at `cc/arch/lir.rs:114-115` uses `FpSize::Extended`. Everything around it is consistent with 128-bit: `cc/arch/aarch64/macros.rs:32` advertises `__LDBL_MANT_DIG__ = 113`, `cc/types.rs:1096-1101` sizes `long double` at 128 bits on aarch64/Linux, and `cc/arch/aarch64/mapping.rs:38-215` routes long-double arithmetic to libgcc's IEEE-quad `__addtf3`/`__multf3`/… family. If the load/store path feeding those calls truncates to 64 bits, `long double` is corrupted round-trip on aarch64/Linux. **[static — NOT VERIFIED; this audit ran on x86_64 and the claim is aarch64-only.]** Treat as a lead, not a confirmed defect: write a `long double` round-trip test and run it on aarch64 before scheduling work.

### Minor

- [ ] **#P10 — A comment is not treated as whitespace for `#` stringification.** `cc/token/lexer.rs:760-794` consume comments without setting `self.whitespace`. **[probed]** `#define S(x) #x` with `S(a/**/b)` yields `"ab"`; C17 phase 3 requires the comment to become one space, so it must be `"a b"`. (`S(c d)` correctly yields `"c d"`, confirming the mechanism works and only the comment path is missing.) Fix: set `whitespace = true` when skipping a comment.
- [ ] **#P11 — Trigraphs are unimplemented (still mandatory in C17), and the docs misstate the standard.** No `??` handling anywhere in `cc/token/lexer.rs`. **[probed]** `int main(void)??<return 0;??>` → parse error. Trigraphs were deprecated in C99 and removed only in **C23**; they remain mandatory through C17. POSIX's own RATIONALE (88224) notes that *"Some c17 compilers not conforming to POSIX.1-2024 do not support trigraphs by default"* — i.e. supporting them is the conforming behavior. Real-world impact is near zero, hence Minor. `cc/README.md:117` and `cc/doc/c99-checklist.md:505` both claim removal "in C11", which is wrong regardless of whether trigraphs get implemented. Fix: correct the docs; optionally implement phase 1.
- [ ] **#L4 — `"a" L"b"` (mixed narrow/wide concatenation) is rejected.** `cc/parse/expression.rs:2800-2863`: the narrow and wide handlers each only concatenate with their own kind. C11/C17 6.4.5p5 makes the mixed form well-defined (it was UB in C99), yielding a wide string. **[probed]** — this is *diagnosed*, not silently mis-merged, so it is a clean missing-feature gap rather than a correctness hazard; demoted from the agent's Major to Minor accordingly. Fix: merge the two loops so either kind starting the run promotes the whole concatenation to wide.
- [ ] **#X9 — `_Atomic` is not rejected on array or function types.** No constraint check in `cc/types.rs` or `cc/parse/parser.rs`. 6.7.2.4/6.7.3 forbid it. **[static]** Fix: check where `TypeModifiers::ATOMIC` meets an array/function `TypeKind`.
- [ ] **#P13 — `#line` argument errors are silently swallowed.** `cc/token/preprocess.rs:2540-2576` returns without diagnosing an unparsable or out-of-range (>2147483647, 6.10.4p3) line number. **[static]**
- [ ] **#P14 — `&&`/`||` in `#if` do not short-circuit.** `cc/token/preprocess.rs:3373-3391` always evaluate both operands. Currently harmless (division by zero is separately guarded at 3503-3527) but a latent trap. **[static]**
- [ ] **#P15 — `-D` cannot define a function-like macro.** `cc/token/preprocess.rs:191-232` always builds an object-like macro, so `-D'FOO(x)=x+1'` makes `"FOO(x)"` the macro *name* — a silent no-op. Not POSIX-mandated (the spec says only "name"), but a universal expectation. **[static]**
- [ ] **#P16 — GNU named-variadic macros drop all but the first extra argument.** `cc/token/preprocess.rs:1152-1187` treats the identifier before `...` as an ordinary positional parameter. GNU extension, not C17-mandated. **[static]**
- [ ] **#P12 — `_GNU_SOURCE`/`_XOPEN_SOURCE=700` are predefined unconditionally on Linux.** `cc/os/linux.rs:26-31`, before user `-D`/`-U`. POSIX only *encourages* restricting visibility here (88196-88203), so this is not a violation. **[static]**
- [ ] **#U7 — Runtime diagnostics are hardcoded English.** `cc/diag.rs:299-403` and every `eprintln!` in `cc/main.rs`. `setlocale` *is* called (`cc/main.rs:870`) and help strings *are* wrapped, so only the diagnostic strings are missing — but see cross-cutting note 4: `gettext()` is currently an identity function workspace-wide, so wrapping them changes nothing until that is addressed.
- [ ] **#U8 — The binary is named `pcc`, not `c17`.** `cc/Cargo.toml:27-29`. Tracked per the maintainer's stated rename plan. Note that #P1/#X2 (`__STDC_VERSION__`) must land with or before the rename, or the renamed binary will misreport its own language level.
- [ ] **#U9 — `-` is accepted as a pathname operand though STDIN says "Not used."** `cc/main.rs:321-323`, `840`. A harmless GCC-compatible extension; conforming applications never pass a bare `-`. Document rather than remove.
- [ ] **#U10 — `-c -o out` with multiple inputs overwrites `out` once per input, silently.** `cc/main.rs:575`. The spec leaves this unspecified (88338-88343) so it is not a defect; a one-line warning would help. Not required for conformance.
- [ ] **#L8 — `cc/doc/c99-checklist.md` overclaims and should not be used as compliance evidence.** Every item in #L1–#L6 has a corresponding `[x]` (e.g. lines 353, 431, 564, 580, 856-858). The checklist treats "parses without panicking" as "conforms". Fix: correct the entries and add rows for implicit-int and duplicate-case detection, which the checklist does not track at all.
- [ ] **#X10 — `cc/doc/c11-checklist.md` *under*-reports anonymous struct/union inside `union`.** Lines 138-139/199-200 mark these unimplemented, but `cc/parse/parser.rs:2061-2147` uses one shared member-parsing path whose anonymous-member branch (2135-2147) is gated only on `is_struct_or_union && is_special(b';')`, with no `is_union` restriction — so it already works. **[static]** Fix: add a regression test, then tick the boxes.
- [ ] **#H1 — `<stdint.h>` and `<uchar.h>` are not bundled; the host's are used.** **[probed]** all of `float.h iso646.h limits.h stdalign.h stdarg.h stdbool.h stddef.h stdnoreturn.h complex.h stdatomic.h` are bundled in `cc/include/` and compile; `stdint.h`, `uchar.h`, and `threads.h` resolve to the system copies. C17 4p6 puts `<stdint.h>` in the *freestanding* set, which the implementation must supply itself. It works today because `cc/arch/mod.rs:112-231` predefines the full GCC-compatible `__INTn_TYPE__`/`__INTn_MAX__` surface glibc's `<stdint.h>` expects — a functional but not freestanding-capable design that hard-depends on a full libc sysroot even for trivial programs. `cc/tests/c99/stdlib_headers.rs:12` documents the delegation explicitly. Fix: bundle `stdint.h` (and `uchar.h` if #X5 is addressed).
- [ ] **#H10 — `_POSIX_C_SOURCE` defaults to `200809L` (POSIX.1-2008), not `202405L` (POSIX.1-2024).** `cc/os/mod.rs:24`. **[probed]** Every delegated system header gates POSIX.1-2024 prototypes behind this macro, so a `c17`-branded, POSIX.1-2024-targeting compiler exposes a 2008-era system interface by default. (`_XOPEN_SOURCE=700` at `cc/os/linux.rs:30` is likewise the 2008-era value.) Fix: bump the defaults; `-D` already overrides them.
- [ ] **#H9 — `xmmintrin.h`/`emmintrin.h` are registered builtin headers that unconditionally `#error`.** `cc/include/xmmintrin.h:13`, `cc/include/emmintrin.h:13`, wired at `cc/builtin_headers.rs:49-53,72-73`. **[probed]** Not ISO/POSIX-mandated, so no conformance impact on its own — but combined with #P9 (builtins win over `-I`), registering them means a user who *does* have a real SSE intrinsics header on their include path gets pcc's hard failure instead. Fix: drop them from `get_builtin_header()` so the normal search can find a real one.
- [ ] **#H8 — `FLT_ROUNDS` is a static `1`.** `cc/include/float.h:69`. C17 5.2.4.2.2p8 requires it to reflect the current rounding mode; glibc's own `<float.h>` calls `__flt_rounds()`. pcc has no `<fenv.h>` coordination, so a runtime `fesetround()` is never reflected. **[probed]** Low impact; note only.
- [ ] **#H12 — `-ffreestanding` is accepted and silently ignored; `__STDC_HOSTED__` is always `1`.** The generic `-f*` catch-all at `cc/main.rs:767` swallows it, and `__STDC_HOSTED__` is hardcoded `"1"` in two places (`cc/token/preprocess.rs:550`, `cc/os/mod.rs:22`) with no path that flips it. **[probed]** Consistent with #H1 (there is no real freestanding mode), but silently accepting a flag that does nothing is misleading. Fix: diagnose it as unsupported, or implement it.
- [ ] **#H11 — `cc/rtlib.rs` is not the runtime-helper source of truth its doc comment claims.** `cc/rtlib.rs:9-14` says it "maps C type operations to their corresponding runtime library function names", but it holds only the 12 `_Float16` conversion pairs. The load-bearing names are hardcoded at their call sites: `__divti3`/`__udivti3`/`__modti3`/`__umodti3` at `cc/arch/mapping.rs:1326-1329`, the whole `__*tf*` quad family at `cc/arch/aarch64/mapping.rs:38-215`, and bare `memcpy`/`memset` at `cc/arch/x86_64/features.rs:1236,1270`. **[static]** Not a correctness bug — all are stable libgcc/compiler-rt export names — but documentation drift worth fixing before more targets land.

### Not confirmed

- [ ] **#L7 — K&R old-style parameter default-argument promotion — CLAIM NOT REPRODUCED.** An agent reported that `cc/parse/parser.rs:3405-3448` stores old-style parameter types verbatim without widening `float`→`double` / `char`/`short`→`int` per C99 6.9.1p8, predicting a miscompile. **[probed]** `int takes_float(a) float a; { return (int)(a*2); }` called as `takes_float(3.5)` returns **7** under both `pcc` and `gcc`. The predicted failure does not occur on this path. Recorded for completeness; needs a narrower repro before being treated as a real finding, and should not be scheduled as work on the strength of the static reading alone.

## Detailed conformance matrix

### SYNOPSIS

- [x] Options interspersed with operands — clap parses flags anywhere; `cc/main.rs:78`.
- [x] Grouping option letters "need not be recognized" — spec-permissive, N/A.
- [x] `--` end-of-options — **[probed]** `pcc -c -- t.c` works.
- [x] At least one pathname operand required — `cc/main.rs:78`.
- [ ] `-L`/`-l`/`-R` order significant — **MISSING**, #U3.

### OPTIONS

| Option | Status | Evidence |
|---|---|---|
| `-B mode` | **MISSING** | #U4 |
| `-c` | CONFORMS | `cc/main.rs:134-135,573-594` |
| `-D name[=value]` | CONFORMS | `cc/main.rs:116-117`; **[probed]** default value is `1` |
| `-D`/`-U` precedence (`-U` wins regardless of order) | CONFORMS | `cc/token/preprocess.rs:3800-3818`; **[probed]** both orders leave the name undefined — exactly as 87878-87880 requires |
| `-D` limits (≥2048 bytes, ≥256 names) | CONFORMS | unbounded `Vec` |
| `-E` | PARTIAL | works, but no line markers — #U5 |
| `-G` | **MISSING** | #U4 |
| `-g` | CONFORMS | `cc/main.rs:147-148,578-579,602-603` |
| `-I directory` | CONFORMS | `cc/main.rs:122-123`; order preserved, unbounded (≥10 floor met) |
| `-I` search algorithm (`""` vs `<>`) | CONFORMS *except builtins* | `cc/token/preprocess.rs:1720-1737` matches 87905-87910 precisely; but builtins pre-empt it — #P9 |
| `-L directory` | PARTIAL | forwarded, ordering lost — #U3 |
| `-l library` | PARTIAL | forwarded; `.so`-vs-`.a` selection delegated to the host linker (acceptable); interspersion lost — #U3 |
| `-O optlevel` | CONFORMS | `cc/main.rs:184-186`; `cc/opt.rs:72-75` disables all passes at `0`; omitted ⇒ `0` (spec: default unspecified) |
| `-o outfile` | CONFORMS (single-file) | `cc/main.rs:552,575,597` |
| `-R directory` | **MISSING** | #U4 |
| `-s` | **MISSING** | #U4 |
| `-U name` | CONFORMS | `cc/main.rs:119-120` |

### OPERANDS

- [x] `file.c`, `file.i` — `cc/main.rs:838-840`; **[probed]** `.i` from `pcc -E` recompiles.
- [x] `file.o`, `file.a`, `file.so` (incl. versioned `libfoo.so.N`) recognized — `cc/main.rs:849-856`.
- [x] Unrecognized suffixes warned and skipped — `cc/main.rs:925-929`; spec calls this implementation-defined.
- [ ] Operands actually combined into one link — **MISSING**, #U1.

### STDIN / INPUT FILES / OUTPUT FILES

- [x] STDIN not used (bare `-` is an opt-in extension, #U9).
- [x] `-c` without `-o` produces `$(basename pathname .c).o` — `cc/main.rs:539-548,575`; **[probed]**.
- [x] `a.out` default — `cc/main.rs:597`; **[probed]**.
- [x] Executable permission bits `S_IRWXU|S_IRWXG|S_IRWXO` minus umask — **[probed]** `a.out` is `0775` under `umask 0002`. Delegated to the host linker, which satisfies the mandate.
- [ ] `.o`/`.a`/`.so` operands linked — **MISSING**, #U1.

### ENVIRONMENT VARIABLES

| Var | Status | Notes |
|---|---|---|
| `LANG`, `LC_ALL`, `LC_CTYPE` | CONFORMS | `setlocale(LcAll,"")` at `cc/main.rs:870` forwards to libc |
| `LC_MESSAGES` | PARTIAL | locale set, diagnostics hardcoded English — #U7 |
| `NLSPATH` (XSI) | PARTIAL | moot while `gettext()` is an identity stub |
| `TMPDIR` (XSI) | **MISSING** | #U6 |

### ASYNCHRONOUS EVENTS

- [x] "Default" — no handlers installed, which is the conforming baseline. Temp files are not cleaned up if the process is signalled (see #U6's fix, which would also address this).

### STDOUT / STDERR

- [x] `-E` output to stdout — `cc/main.rs:387-446`.
- [x] Diagnostics to stderr only — `cc/diag.rs:383-402`; **[probed]**.
- [x] Warnings do not force a non-zero exit — `cc/diag.rs:409-412`.
- [x] The optional `"%s:\n"` per-file header is not emitted — spec says "may", so N/A.
- [ ] `-E` `# <line> "<file>"` markers — **MISSING**, #U5.

### EXTENDED DESCRIPTION

- [x] Implicit `-l c` after all operands — delegated: the final link invokes the host `cc` (`cc/main.rs:614,1007`) without `-nostdlib`, so libc is always linked last. CONFORMS by delegation.

> **Architectural note (not a finding).** `c17` implements none of Phase 8 itself: it shells out to `as` for assembly (`cc/main.rs:601`) and to `cc` for linking (`614`, `1007`). There is no crt object selection, no dynamic-linker path resolution, and no explicit `-lc` or `-lgcc` anywhere in the crate. This is a legitimate choice — it satisfies the implicit-`-l c` and executable-permission mandates transitively, since every conforming host `cc` does those things — but it is an undocumented hard runtime dependency on `cc` and `as` being on `$PATH`, and it means pcc inherits the host driver's crt and runtime-library decisions rather than making its own. Worth stating in `cc/README.md`.

- [x] `-l m`, `-l pthread`, `-l rt`, `-l xnet`, `-l l`, `-l y` forwarded verbatim — `cc/main.rs:629-631,1025-1027`. The spec permits these to not exist as regular files; resolution is the host linker's job.
- [x] External symbol significance ≥31 bytes, ≥4095 identifiers per TU — no artificial caps in `cc/symbol.rs`/`cc/strings.rs`.
- [ ] "A library shall be searched when its name is encountered" — same defect as #U3.
- [ ] Programming environments (`getconf _POSIX_V8_*`, `*_CFLAGS`/`*_LDFLAGS`/`*_LIBS`) — **N/A to `cc/`**: this is a `getconf`/system-configuration obligation (88105-88179), not something the compiler binary implements. Flagged here only so it is not lost; it belongs to whichever crate owns `getconf`.

### EXIT STATUS / CONSEQUENCES OF ERRORS

- [x] `0` on success, `>0` on error — **[probed]** compile error exits 1; the only `exit(0)` calls are legitimate info queries (`--print-targets` etc.).
- [x] Link failure diagnosed to stderr with non-zero exit — `cc/main.rs:610-611,640-641,1037-1039`; **[probed]**.
- [ ] Continue compiling remaining operands after an error — **MISSING**, #U2.

### ISO C language conformance summary

| Area | Status |
|---|---|
| Translation phase 1 (trigraphs) | MISSING (#P11) |
| Translation phase 2 (line splicing) | CONFORMS — handled in `nextchar`/`peekchar` (`cc/token/lexer.rs:322-417`), so transparent to identifiers, literals, and comments alike |
| Translation phase 3 (comments) | PARTIAL — #P10 |
| Directives | CONFORMS except #P2, #P3, #P4, #P5, #P13 |
| Computed `#include`, `_Pragma`, digraphs, `#include_next` | CONFORMS — **[probed]** computed include and `_Pragma` both work |
| Include cycle/depth guard | CONFORMS — **[probed]** a self-including header exits 1, no hang (`max_include_depth = 200`) |
| Macro expansion (blue paint, rescanning, `##` placemarkers) | CONFORMS |
| `#if` constant expressions | PARTIAL — #P6, #P14 |
| Predefined macros | PARTIAL — #P1, #P7, #X8 |
| C89 core | CONFORMS (declarators, bitfields, storage classes, tentative definitions, promotions) |
| C99 | Mostly CONFORMS (VLAs, designated initializers, compound literals, flexible array members, `restrict`, `inline`, `_Bool`, `long long`, `_Complex`, `__func__`, UCNs, hex floats). Gaps: #L1, #L2, #L3, #L4, #L5, #L6 |
| C11 | PARTIAL — `_Static_assert`, `_Alignas`/`_Alignof`, `_Noreturn`, `_Thread_local`, anonymous struct/union, and the explicit atomic API all work. Gaps: #X1, #X3, #X4, #X5, #X6, #X7, #X9 |
| C17 | NOT CLAIMED — #P1/#X2. C17 is DR-only; DR 412 (`_Static_assert` as a struct member) is honored at `cc/parse/parser.rs:2116-2120`. DR 423 is moot until `_Generic` exists |
| Translation limits (C99 5.2.4.1) | CONFORMS — `Vec`/`BTreeMap`-backed throughout; `cc/tests/c99/translation_limits.rs` exercises 130-member structs, 20 params, 30 cases. Residual risk: the recursive-descent parser has no explicit depth guard, so very deep nesting depends on stack size |
| Diagnostics for constraint violations (C17 5.1.1.3) | PARTIAL — present for undeclared identifiers, `const` assignment, bad member access, file-scope/struct-member VLA, non-constant enum, `_Alignas` non-power-of-two, typedef-with-initializer. Missing for #L1, #L2, #L3, #L6, #P3, #P4, #P5, #X4, #X9 |

### Extensions accepted

Not findings — POSIX permits extensions, and none of these change the meaning
of strictly-conforming source: `__attribute__`, statement expressions,
`typeof`, the `__builtin_*` family, `__c11_atomic_*`, GCC extended inline asm
(incl. `asm goto`), `__int128`, `_Float16`/`_Float32`/`_Float64`, nullability
qualifiers, `#include_next`, `#warning`, digraphs, and the C23 one-argument
`_Static_assert`. See `cc/doc/ATTR.md` and `cc/doc/BUILTIN.md`.

## Test coverage signal

Not covered:
- [ ] More than one `.c`/`.i` operand in a single invocation — `cc/tests/common/mod.rs:57-125` always builds exactly one source and one `-o`. Would have caught #U1 immediately.
- [ ] A `.c` operand combined with a `.o`/`.a`/`.so` operand (spec EXAMPLE 1).
- [ ] `-L`/`-l` interleaved with pathname operands (spec EXAMPLE 3).
- [ ] `-B`, `-G`, `-R`, `-s` (they do not exist).
- [ ] `-E` output containing `# <line> "<file>"` markers.
- [ ] `TMPDIR` honored for temp files.
- [ ] A compile error on a non-final operand followed by a later successful one.
- [ ] Negative-path diagnostics generally: there is **no** test asserting that a malformed program is *rejected*. The suites prove accepted programs run correctly, not that invalid programs are diagnosed. A `cc/tests/diagnostics/` suite asserting `error_count() > 0` for #L1–#L6, #P3–#P5, #X4, #X9 would close the largest structural gap.
- [ ] Bare `#` null directive; macro redefinition/arity diagnostics; large `#if` hex constants; comment-as-whitespace stringification; trigraphs.
- [ ] `_Atomic` via plain operators — a naive test would *pass* today (#X1 only manifests under concurrency or assembly inspection), so the test must assert on generated assembly containing `lock`.
- [ ] `_Generic`, Unicode literal prefixes, `CMPLX`, incompatible typedef redefinition.
- [ ] Header-search precedence between builtin headers and `-I`/dir-of-file (#P9).
- [ ] aarch64 `long double` round-trip (#H4) — the highest-value net-new test, since it would settle an open question rather than confirm a known bug.

Actively pinning current behavior (must change alongside the fix):
- [ ] `cc/tests/c11/core.rs:301` asserts `__STDC_VERSION__ == 201112L` (#P1/#X2).
- [ ] `cc/token/preprocess.rs:4439-4446` (`test_macro_redefinition`) documents silent macro override as intended (#P3).

---

# `cflow`

**Implementation:** `cc/cflow.rs` (692 lines)
**Tests:** `cc/tests/tools/cflow.rs` (147 lines, 1 mega-test) + `cc/tests/cflow/test.c`
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/cflow.md`

## TL;DR

The forward flowgraph's primary path is close to the spec — indentation,
the `name: int(), <file line>` definition form, and the `<>` undefined-reference
form all match the spec's worked EXAMPLE. But the utility exits 0 no matter
what fails, `-i x` (data-symbol inclusion) is entirely inert so the spec's own
EXAMPLE output cannot be reproduced, the back-reference line for repeated
callees uses an invented `name  {N}` format instead of the documented
`%d %s:%s`, and definition line numbers point at the type-specifier line rather
than the declarator line. Object files and `.l`/`.y` inputs — both required by
INPUT FILES — are unsupported.

## Priority issues

### Critical

- [ ] **#F1 — Exit status is always 0.** `cc/cflow.rs:626-692`: `main()` unconditionally returns `ExitCode::SUCCESS`; per-file errors (649-658) and swallowed parse errors (488-491) are only `eprintln!`'d. **[probed]** a file with a compile error prints a diagnostic and exits 0. Fix: accumulate an error flag and return `ExitCode::FAILURE`.
- [ ] **#F2 — `-i x` has no effect; the spec's own EXAMPLE cannot be reproduced.** `cc/cflow.rs:87-88,98,634`: `CallGraph::_include_data` is written and never read; `extract_calls_from_expr`/`extract_calls_from_stmt` (157-351) only collect `ExprKind::Call` targets, so plain object references are never graph nodes. **[probed]** running the spec's EXAMPLE verbatim, `cflow -i x file.c` omits the mandated line `4 i: int, <file.c 1>` entirely. Fix: collect referenced file-scope object identifiers and splice them in when `include_data` is set.

### Major

- [ ] **#F3 — The back-reference line does not match the documented output format.** `cc/cflow.rs:549-552` prints `"{ref}{indent}{name}  {{{prev}}}"`. STDOUT mandates `"%d %s:%s\n"` and *"subsequent references to that name contain only the reference number of the line where the definition can be found."* **[probed]** a diamond call graph yields `5         shared  {3}` — no colon, and `{3}` braces inconsistent with the tool's own `<>` conventions. Recursion produces the same shape. The shipped mega-test cannot catch this: its only repeated call is de-duplicated at extraction time (line 168). Fix: emit the colon form.
- [ ] **#F4 — `-D`/`-I`/`-U` relative order is not preserved.** `cc/cflow.rs:49-56` + `cc/token/preprocess.rs:3795-3818`. `cflow` explicitly overrides XBD 12.2 to make this order significant (cflow.md 88920-88921) — the *opposite* of `c17`'s rule — but it shares `c17`'s "all `-D`s then all `-U`s" code path. **[probed]** via the shared path: `-DFOO=1 -UFOO -DFOO=2` leaves `FOO` undefined; per cflow's spec it should be `2`. Fix: parse into one ordered list and replay in order.
- [ ] **#F5 — Object-file input is unsupported.** `cc/cflow.rs:647-666` recognizes only `c|h|l|y|i` and stubs `s`; everything else, including `.o`, hits "unknown file type". DESCRIPTION and INPUT FILES require object files, and STDOUT even specifies their distinct definition format ("filename and location counter ... for example, `text`"). **[static]** Fix: read ELF/Mach-O symbol tables, or document the gap.
- [ ] **#F6 — `.l`/`.y` operands are parsed as plain C.** `cc/cflow.rs:647-648,484-492` routes them through the identical C pipeline. OPERANDS requires they be "processed as appropriate" for lex/yacc. Real `.l`/`.y` source is not valid C, so the parse fails, the error is swallowed (488-491), and the file contributes nothing — silently. **[static]** Fix: pre-run `lex`/`yacc`, or refuse them explicitly like `.s`.

### Minor

- [ ] **#F7 — Non-positive `-d` is not ignored.** `cc/cflow.rs:42-43,536-541`. The spec requires *"Attempts to set the cut-off depth to a non-positive integer shall be ignored."* **[probed]** `-d 0` truncates output to the root line only, instead of behaving as if `-d` were absent; `-d -1` is a clap usage error. Fix: accept a signed value and fall back to unbounded for anything `<= 0`.
- [ ] **#F12 — Definition line numbers point at the type-specifier line, not the declarator line.** `cc/cflow.rs:557-560`. **[probed]** for the spec's EXAMPLE source, pcc reports `main: int(), <file.c 5>` and `f: int(), <file.c 12>` where the spec's expected output is `<file.c 6>` and `<file.c 13>` — the lines holding `main(void)` and `f()`, not the preceding bare `int`. Off-by-one whenever the return type is on its own line (a common style, and the style used by the spec's own example). Fix: attribute the definition to the declarator's position.
- [ ] **#F8 — `-r` ordering ignores `LC_COLLATE`.** `cc/cflow.rs:135,147,592` use byte-order `.sort()`. ENVIRONMENT VARIABLES names `LC_COLLATE` specifically for `-r` ordering. **[static]** Fix: sort via `plib::locale::strcoll`.
- [ ] **#F9 — The `-i` option-argument is not validated.** `cc/cflow.rs:46-47,634-635` silently accept anything other than `x`/`_`. **[static]**
- [ ] **#F10 — `.i` operands are preprocessed a second time.** `cc/cflow.rs:647-648,464-478`. Inherited from `c17`'s OPERANDS: the processing already done by `-E` *"shall not be repeated."* Usually idempotent. **[static]**
- [ ] **#F11 — `NLSPATH`/`LC_MESSAGES` are no-ops.** See cross-cutting note 4.

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
- [ ] `file.i` → re-preprocessed (#F10).
- [ ] `file.y`, `file.l` → parsed as C (#F6).
- [ ] `file.s` → explicitly refused (`cc/cflow.rs:660-661`). The spec permits "more limited" processing, so refusal is defensible; refusing *and* exiting 0 (#F1) is not.
- [ ] Object files → unsupported (#F5).

### STDIN / STDERR / OUTPUT FILES

- [x] STDIN not used. CONFORMS.
- [x] stderr used only for diagnostics. CONFORMS.
- [x] OUTPUT FILES "None". CONFORMS.

### ENVIRONMENT VARIABLES

- [x] `LANG`, `LC_ALL` — via `setlocale` (`cc/cflow.rs:627`). CONFORMS.
- [ ] `LC_COLLATE` — #F8.
- [ ] `LC_MESSAGES`, `NLSPATH` — #F11.

### STDOUT

- [x] First-definition line `"%d %s:%s\n"` with `type(), <file line>` — **[probed]** matches the spec EXAMPLE's shape exactly.
- [x] Indentation ≥1 column per level — 4 spaces/level (`cc/cflow.rs:543`), matches the EXAMPLE.
- [x] Undefined reference `<>` — **[probed]** `3         h: <>` matches the EXAMPLE. (Note: an identifier that is never *declared* is a hard compile error and is dropped rather than shown as `<>`; the spec's example declares `int h();`, so this is correct behavior for the specified case.)
- [ ] Line-number attribution — #F12.
- [ ] Back-reference form — #F3.
- [ ] Data-symbol definition form (`i: int, <file.c 1>`) — #F2.
- [ ] Object-file definition form — #F5.

### EXIT STATUS / CONSEQUENCES OF ERRORS

- [ ] `>0` on error — **DIVERGES**, #F1.

## Test coverage signal

Not covered:
- [ ] Back-reference/diamond call graphs (#F3) — structurally impossible with the current fixture.
- [ ] `-i x` (#F2), `-i` validation (#F9).
- [ ] `-D`/`-U` interleaving (#F4).
- [ ] Object-file, `.l`, `.y` operands (#F5, #F6).
- [ ] Exit status on any failure (#F1).
- [ ] `-d 0` / negative depth (#F7).
- [ ] Exact line-number attribution (#F12) — the existing test only checks the first line starts with a digit and contains `<`/`>`.
- [ ] `LC_COLLATE` ordering under `-r` (#F8).

---

# `ctags`

**Implementation:** `cc/ctags.rs` (349 lines)
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

- [ ] **#T1 — Exit status stays 0 on every input error.** `cc/ctags.rs:296-313,342-349`. Unreadable files, swallowed parse errors (144-148), and unsupported extensions (306-312) all print to stderr without setting an error flag; only a tags-file write failure returns non-zero. **[probed]** `ctags nosuch.c` → `No such file or directory` then exit 0; `ctags z.pas` → `unknown file type` then exit 0. Fix: track `had_error` and return 1.
- [ ] **#T2 — Tags are silently lost when a name repeats.** `cc/ctags.rs:285,299,318,334`: `all_tags: BTreeMap<String, TagEntry>` is keyed by name alone, so `insert` overwrites. **[probed]** two files each defining `init` produce a tags file containing only `init  f2.c` — the `f1.c` definition vanishes with no diagnostic. The spec's "implementation-defined" latitude for duplicates covers *how* they are presented, not silently discarding valid distinct tags. Fix: key by `(name, file, line)` or store `Vec<TagEntry>` per name.

### Major

- [ ] **#T3 — `-x` output diverges from the mandated format.** `cc/ctags.rs:79-87` uses `format!("{:<16} {:>6} {:<20} {}", ...)` and `.trim()`s the line text. STDOUT (91264-91265) mandates `"%s %d %s %s"` where `<text>` is *"the text of line `<line-number>` of file `<filename>`"*. **[probed]** output is column-padded, and a function written as `    int indented_fn(...)` loses its leading indentation. Both break naive field-splitting and misreport the line's text. Fix: emit single-space-separated fields and do not trim.
- [ ] **#T4 — `-x` sort order ignores `LC_COLLATE`.** `cc/ctags.rs:285,316-320`. The spec has an asymmetry worth preserving: the *tags file* must be sorted in the **POSIX locale's** collating sequence (so byte order is correct there, and the current `BTreeMap` happens to satisfy it), while **`-x` output** must honor `LC_COLLATE`. One map serves both. **[static]** Fix: sort `-x` separately via `plib::locale::strcoll`.

### Minor

- [ ] **#T6 — Multi-line `typedef`s are missed.** `cc/ctags.rs:211-248`: `is_typedef_line` requires the `typedef` keyword and the type name on the same physical line, so the ubiquitous `typedef struct { ... } Foo;` is never tagged. **[static]** Fix: track brace depth, or use the symbol table.
- [ ] **#T7 — The two SYNOPSIS forms are not mutually exclusive.** `cc/ctags.rs:34-51,316-346`. Per XBD 12.2 guideline 8, separate synopsis lines signal exclusivity; `-x` silently ignores `-a`/`-f`. **[static]** Fix: add a clap conflict group.
- [ ] **#T8 — The tags file is written non-atomically.** `cc/ctags.rs:329` (`File::create`) truncates in place, so an interrupted write leaves a corrupt tags file for concurrent `vi`/`ex` readers. Not spec-mandated, but `plib::io::write_atomic` already exists and was adopted crate-wide in `dev/` for exactly this. **[static]**
- [ ] **#T9 — Input is UTF-8-only.** `cc/ctags.rs:108` (`fs::read_to_string`) hard-errors on non-UTF-8 source regardless of `LC_CTYPE`; combined with #T1 that becomes an unreported failure. **[static]**
- [ ] **#T5 — Runtime diagnostics bypass `gettext`.** See cross-cutting note 4.

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
- [ ] `LC_COLLATE` for **`-x`** — #T4.
- [ ] `LC_CTYPE` — UTF-8 pinned (#T9).
- [ ] `LC_MESSAGES`, `NLSPATH` — #T5.

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
- [ ] `>0` on error — **MISSING**, #T1.

## Test coverage signal

Not covered:
- [ ] Duplicate tag names across files (#T2).
- [ ] Exit status on unreadable / unsupported / parse-failing input (#T1).
- [ ] Exact `-x` format, incl. padding and preserved indentation (#T3) — the test only does `.contains()` checks.
- [ ] `LC_COLLATE` for `-x` (#T4).
- [ ] Default `-f tags` filename (tests always pass `-f`).
- [ ] Escaping of `/` and `\` inside a pattern.
- [ ] Multi-line `typedef struct {...} Name;` (#T6).
- [ ] `-x` combined with `-a`/`-f` (#T7).
- [ ] Non-UTF-8 input (#T9).
- [ ] Sort order across an `-a` append boundary.

---

# `cxref`

**Implementation:** `cc/cxref.rs` (595 lines)
**Tests:** `cc/tests/tools/cxref.rs` (155 lines, 1 mega-test) + `cc/tests/cxref/test.c`
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/cxref.md`

## TL;DR

The CLI surface is complete and the listing has the right general shape —
symbol, file, containing function, line numbers, with `*` marking the declaring
reference. Underneath, the data is substantially wrong: `#define` symbols are
never cross-referenced at all, the defining line number is `0` for any
declaration without an initializer (which is most of them), uninitialized
locals are dropped entirely, a function's own definition is filed under its own
scope instead of global scope, and the mandated per-file name line is never
emitted — which also makes `-c` a no-op, since output is identical with and
without it. As with its siblings, every error path returns 0.

## Priority issues

### Critical

- [ ] **#R1 — `#define` symbols are never cross-referenced.** `cc/cxref.rs:348-443`. Preprocessing (378-390) fully expands macros before parsing, and nothing captures macro definitions beforehand, so they cannot appear in the listing. **[probed]** a file containing `#define MAXV 10` used later produces no `MAXV` row at all. Fix: capture macro name+line at `#define` time and feed them into `CrossRef`.
- [ ] **#R2 — Defining line numbers are `0` for uninitialized declarations; uninitialized locals vanish.** `cc/cxref.rs:432` (`.unwrap_or(0)` for file-scope declarations lacking an initializer) and `269-273` (block-scope declarations without an initializer are skipped outright). The root cause is that `InitDeclarator`/`Declaration` in `cc/parse/ast.rs:949-966` carry no position, so the line is only recoverable from the initializer expression. **[probed]** for `int g; int fn(void); int fn(void){ int loc; ... }`, both `g` and `fn` report `*0`, and `loc` never appears. Fix: add a position field to the declarator and thread it through independent of the initializer.
- [ ] **#R3 — Exit status is always 0.** `cc/cxref.rs:529-595`: parse errors (398-404), unreadable files (573-575), and rejected operands (583-586) are `eprintln!`-only; only an `-o` create failure returns 1. **[probed]** a file with a compile error prints a diagnostic and exits 0. Fix: track an error flag across the loop.

### Major

- [ ] **#R4 — The mandated per-file name line is missing, which also makes `-c` a no-op.** `cc/cxref.rs:450-523` never emits a standalone filename line; the filename is only a per-row column. STDOUT (91628-91629): *"If the −c option is not specified, each portion of the listing shall start with the name of the input file on a separate line."* **[probed]** `cxref p.c q.c` and `cxref -c p.c q.c` produce **byte-identical** output — the tool always behaves as if `-c` were given. Fix: when `!combined`, print the filename on its own line before each file's sorted block.
- [ ] **#R5 — A function's own definition is mis-scoped under itself.** `cc/cxref.rs:411-412` calls `set_function(&name)` *before* `add_definition(&name, ...)`. The spec asks for the name of the function in which a symbol appears *"if it is not a function name itself"*. **[probed]** `fn` appears both as a bogus global row `*0` and as a row scoped to `fn` itself. Fix: add the definition before setting the scope.
- [ ] **#R6 — `-D`/`-I`/`-U` relative order is not preserved.** `cc/cxref.rs:53-60` + `cc/token/preprocess.rs:3801-3818`. Like `cflow`, `cxref` explicitly makes this order significant (cxref.md 91587-91588), the opposite of `c17`'s rule, but shares `c17`'s code path. **[probed]** `-DFOO=1 -UFOO -DFOO=2` leaves `FOO` undefined; it should be `2`. Fix: one ordered list, replayed in order.
- [ ] **#R7 — The symbol listing ignores `LC_COLLATE`.** `cc/cxref.rs:105,451` iterate a `BTreeMap<String, SymbolInfo>` in byte order. **[static]** Fix: sort via `plib::locale::strcoll`.

### Minor

- [ ] **#R8 — The operand extension gate rejects valid C source.** `cc/cxref.rs:564-586` accepts only `.c`/`.h`; OPERANDS imposes no naming rule. **[static]**
- [ ] **#R9 — Struct/union member names are not tracked.** `cc/cxref.rs:189-191` recurses into the base expression and discards the field identifier. Soft gap — the spec is generic about "symbols". **[static]**
- [ ] **#R11 — `-w` does not bound the row prefix.** `cc/cxref.rs:480-486` builds the name/file/function columns unconditionally; only line-number continuation wrapping respects `width` (492-513), so a long filename alone can exceed the requested width. **[static]**
- [ ] **#R10 — `LC_MESSAGES`/`NLSPATH` are no-ops.** See cross-cutting note 4.

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

- [ ] `file` — any C source pathname; restricted to `.c`/`.h` (#R8).
- [x] Multiple operands, per-file vs combined accumulation — `cc/cxref.rs:558-587` (but see #R4).
- [x] STDIN not used.

### ENVIRONMENT VARIABLES

- [x] `LANG`, `LC_ALL` — `setlocale` at `cc/cxref.rs:530`.
- [ ] `LC_COLLATE` — #R7.
- [x] `LC_CTYPE` — passed to libc.
- [ ] `LC_MESSAGES`, `NLSPATH` — #R10.

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

- [ ] `>0` on error — **DIVERGES**, #R3.

## Test coverage signal

Not covered:
- [ ] `-c` versus default output shape (#R4) — nothing would detect that they are identical.
- [ ] Exact line numbers attached to `*` markers (#R2) or correct scoping (#R5).
- [ ] `#define` symbols in the listing (#R1).
- [ ] `-w` wrapping/truncation (#R11).
- [ ] `-D`/`-I`/`-U`, including order-sensitivity (#R6).
- [ ] Multiple file operands.
- [ ] Exit status on any error path (#R3).
- [ ] Non-`.c`/`.h` operands, unreadable files, parse errors.
- [ ] `LC_COLLATE` ordering (#R7).

---

# Suggested PR groupings

Ordered roughly by value-per-unit-risk. Each is independently landable.

- **PR A — "Exit status and error propagation"**: #F1, #T1, #R3, #U2.
  Three of four utilities never report failure, and `c17` aborts instead of
  continuing. Small, mechanical, high user-visible value, and it makes every
  later PR's tests able to assert failure.

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
  trigraph claims in `README.md`/`c99-checklist.md`. **This PR is the
  prerequisite for the `pcc`→`c17` rename** — landing the rename first would
  ship a `c17` binary that reports itself as C11. Note that
  `cc/tests/c11/core.rs:301` currently asserts `__STDC_VERSION__ == 201112L`,
  so the test must move with the macro.

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

- **PR H — "`ctags` data correctness and format"**: #T2, #T3, #T8.
  Re-key the tag map, fix the `-x` format to the mandated `%s %d %s %s`, and
  adopt `plib::io::write_atomic`.

- **PR I — "`cflow` output format"**: #F2, #F3, #F12.
  Make `-i x` real, fix the back-reference line, and attribute definitions to
  the declarator line. Together these make the spec's worked EXAMPLE
  reproducible byte-for-byte, which is a good acceptance test.

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

- **Deferred / not scheduled**: #F5 and #F6 (`cflow` object-file and
  `lex`/`yacc` input) are real spec gaps but represent substantial new
  subsystems. #L7 (K&R promotion) is unconfirmed — it did not reproduce — and
  #H4 (aarch64 `long double`) is unverified because this audit ran on x86_64;
  both need a repro before being scheduled. The `getconf`
  programming-environment obligations (c17.md 88105-88179) belong to whichever
  crate owns `getconf`, not here.
