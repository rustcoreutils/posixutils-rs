# POSIX.1-2024 Conformance Audits — `dev/` utilities

This file collects per-utility POSIX conformance audits for the development
utilities crate. Each audit follows the playbook in `audits.md`.

## Cross-cutting infrastructure (2026-06-03)

The audits surfaced four shared patterns affecting most utilities. The
following work landed in `plib` and closes audit items as a side effect:

- **`plib::diag`** — shared `setlocale` + `textdomain` + diagnostic surface
  with stderr routing, GCC-style `<util>: <source>:line:col: error: <msg>`
  format, and an `exit_status()` accumulator (replaces local `dev/yacc/diag.rs`
  and `dev/lex/diag.rs`).
- **`plib::io::write_atomic`** — tempfile + fsync + atomic rename, preserves
  existing file mode.
- **`plib::archive`** — System V `"/"` symbol-table writer extracted from
  `dev/ar.rs`; now shared with `dev/strip.rs`.
- **`plib::locale`** — libc `isprint(3)` / `strcoll(3)` / `strftime(3)`
  wrappers that honor `LC_CTYPE`, `LC_COLLATE`, `LC_TIME`, `TZ`.

### Items closed as a side effect

| Audit item | Status | How closed |
|---|---|---|
| yacc #7 (no setlocale / LC_* env vars) | **closed** | `plib::diag::init_locale("yacc")` in `main`. |
| yacc #8 (English diagnostics) | **partial** | diag emits via stderr + uniform prefix; strings themselves still need `gettext()` wrapping for full closure. |
| lex #L4 (no setlocale) | **closed** | `plib::diag::init_locale("lex")` in `main`. |
| lex #L5 (English diagnostics) | **partial** | same as yacc #8. |
| ar #A7 (LC_TIME/TZ ignored by `-tv`) | **closed** | `list_member` now formats via `plib::locale::strftime` → honors `LC_TIME` + `TZ`. |
| ar #A7 (LC_MESSAGES) | **partial** | diag plumbing in place; string-level `gettext()` deferred. |
| ar #A10 (non-atomic archive rewrite) | **closed** | 8 `File::create` + stream sites replaced with `plib::io::write_atomic`. |
| nm #N12 (diagnostics on stdout) | **closed** | both `println!("Failed …")` sites now route through `plib::diag::error` → stderr. |
| nm #N14 (English diagnostics) | **partial** | diag plumbing in place; string-level `gettext()` deferred. |
| strings #S2 (`\n` included in strings) | **closed** | switched to `plib::locale::isprint`; `isprint('\n') == false`. |
| strings #S6 (isprint not locale-aware) | **closed** | `plib::locale::isprint` uses libc. |
| strings #S7 (first error aborts loop) | **closed** | `process_files` continues on error; exit via `plib::diag::exit_status()`. |
| strip #ST2 (errors logged but exit 0) | **closed** | `strip_file` records errors via `diag::error`; `main` exits via `exit_status()`. |
| strip #ST5 (archive symtab stale) | **closed** | `strip_archive` rebuilds the `"/"` symbol-table via `plib::archive::write_sysv_symbol_table` after stripping members. |
| strip #ST6 (non-atomic file rewrite) | **closed** | `std::fs::write` replaced with `plib::io::write_atomic`. |
| strip #ST8 (English diagnostics) | **partial** | diag plumbing in place; string-level `gettext()` deferred. |

The "closed" items have new tests in `plib` (`io::tests::write_atomic_*`,
`archive::tests::*`, `locale::tests::*`, `diag::tests::*`). The pre-existing
189 dev-crate tests continue to pass.

The 30+ per-utility correctness bugs (yacc #1-4 codefile boilerplate, ar #A1
date field, ar #A2 basename, ar #A3 mode-flag bundling, ar #A4 `-T`, ar #A5
`-v on -m`, ar #A6 long names, nm #N1-N5 stubs, strings #S1 no-operand stdin,
strip #ST1 non-ELF members dropped, strip #ST3 Mach-O, strip #ST4 `.o`
relocations, etc.) remain candidates for follow-up PRs.

---

## `yacc`

**Implementation:** `dev/yacc/` (main.rs 275, lexer.rs 837, parser.rs 800, grammar.rs 841, first_follow.rs 402, lr0.rs 356, lalr.rs 602, codegen.rs 2047, verify.rs 263, diag.rs 139, error.rs 47 — ~6.6 kloc)
**Tests:** `dev/tests/yacc/mod.rs` (3501 lines, 150 `#[test]`s)
**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 §3, pp. 3661–3678
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/yacc.md`
**Date:** 2026-06-02

### TL;DR

The LALR(1) generator core (tables, conflict resolution, error recovery, action
transformation, mid-rule actions) is solid and well-tested. POSIX conformance
gaps are concentrated in the generated code file's *boilerplate* — the
`YYEMPTY`/`YYEOF` macros mandated by Austin Group Defect 1269 are absent, the
`yyparse()` prototype mandated by Defect 1388 is missing, and the
`yyerror`/`yylex` prototypes lack the required `#ifndef` guards. The yacc
utility itself does no locale setup, reads no `LC_*` variables, and produces
hardcoded-English diagnostics. The `-v` description file is not produced when
errors abort the run before codegen, and the runtime debug code is only
emitted when `-t` is set (preventing `-DYYDEBUG=1` from enabling debug).

### Priority issues

> **Resolved 2026-06-04.** All open yacc items are addressed (commits on
> branch `yacc-posix-conformance`), each with new tests in
> `dev/tests/yacc/mod.rs`:
> - **#1–#4** code-file boilerplate (YYEMPTY/YYEOF, yyparse proto, `#ifndef`
>   guards) + **#14** yynerrs documented.
> - **#6** YYDEBUG now controls debug at compile time (`-DYYDEBUG=1` works
>   without `-t`); `-t` only sets the default.
> - **#5/#13** `-v` description always produced (stub on early failure) +
>   internal-table-limits line.
> - **#10/#12** `-p` no longer mangles user token names; char literals
>   outside `1..=255` rejected.
> - **#9** `--` end-of-options handled.
> - **#8** runtime diagnostics routed through `gettext()`.
> - **#7** already closed (plib::diag, 2026-06-03). **#11** WON'T FIX (see below).

#### Critical

- [x] **#1 — `YYEMPTY` `#define` missing in code file.** `dev/yacc/codegen.rs:81-195` (`generate_code_file`). POSIX 123712–123716 (Issue 8, Austin Group Defect 1269): the code file shall contain `#define` statements for `YYEMPTY` (negative integer, parenthesized) and `YYEOF` (value 0). `grep -n 'YYEMPTY' dev/yacc/*.rs` finds zero occurrences. Fix: emit `#define YYEMPTY (-2)` and `#define YYEOF 0` in the code-file preamble, after the token translation table.
- [x] **#2 — `YYEOF` `#define` missing in code file.** Same site as #1. Fix bundled with #1.
- [x] **#3 — `yyparse(void)` prototype missing in code file.** `dev/yacc/codegen.rs:172-175`. POSIX 123720–123723 (Issue 8, Austin Group Defect 1388) mandates prototypes for `yyerror`, `yylex`, AND `yyparse` in the code file. Implementation emits only the first two. Fix: emit `int yyparse(void);` alongside the others.
- [x] **#4 — `yyerror`/`yylex` prototypes are not `#ifndef`-guarded.** `dev/yacc/codegen.rs:172-175`. POSIX 123724–123727: "The declarations of yyerror() and yylex() shall be protected by `#ifndef` or `#if` preprocessor statements such that each is only visible if a preprocessor macro with the name yyerror or yylex, respectively, is not already defined". The code emits unconditional prototypes, defeating user override via macro. Fix: wrap each in `#ifndef yyerror`/`#endif` and `#ifndef yylex`/`#endif` (with the `yy` replaced by `sym_prefix` when `-p` is used).

#### Major

- [x] **#5 — `-v` description file not produced when grammar/lex/parse step fails.** `dev/yacc/main.rs:157-188`, `dev/yacc/codegen.rs:60-78`. POSIX CONSEQUENCES OF ERRORS (123202–123204): "summary information in the description file *shall always be produced* if the −v flag is present" (emphasis mandated by `shall`). Today, `lexer::lex`, `parser::parse`, or `grammar::Grammar::from_parsed` returning `Err` short-circuits before `codegen::generate` runs, so no `y.output` is written. Fix: write a partial description file in the error path of `run()` when `-v` was requested, before propagating the error.
- [x] **#6 — Runtime debug code is only generated when `-t` is set; `-DYYDEBUG=1` alone has no effect.** `dev/yacc/codegen.rs:99-116`, `308-310`, `935-940`, plus every `if opts.debug_enabled` block (`1045-1054`, `1161-1182`, `1222-1246`, `1281-1290`, `1393-1413`, `1431-1440`, `1452-1461`). POSIX 123150–123154: "If `YYDEBUG` has a non-zero value, the debugging code shall be included." The spec only governs YYDEBUG's *default value* via `-t`. Implementation gates the debug code itself at codegen time, so a compile-time `-DYYDEBUG=1` against a no-`-t` build produces a parser whose `yydebug=1` does nothing. Fix: always emit the debug code wrapped in `#if YYDEBUG` and always declare `yydebug`; let `-t` only flip the default-value `#define`.
- [x] **#7 — `setlocale(LC_ALL, "")` is never called; no `LC_*` env var is consulted.** `dev/yacc/main.rs:261-275`, `dev/yacc/diag.rs:106-108`. POSIX 123679–123696 mandates `LANG`/`LC_ALL`/`LC_CTYPE`/`LC_MESSAGES`/`NLSPATH` shall affect yacc execution. `grep -nE 'setlocale|LC_ALL|LC_CTYPE|LC_MESSAGES|env::var\("(LANG|LC_|NLSPATH)' dev/yacc/*.rs` returns no matches. Fix: call `setlocale(libc::LC_ALL, "")` in `main()` (the utility, distinct from the *yacc-library* `main()` mentioned at 123693). ✓ closed by cross-cutting plib::diag wiring (2026-06-03).
- [x] **#8 — Diagnostic messages are hardcoded English.** `dev/yacc/diag.rs:106` (`writeln!(stderr, "{}: {}: {}", ...)`), every `eprintln!` site in `dev/yacc/main.rs:200-247`. POSIX 123688–123690: `LC_MESSAGES` shall determine "the format and contents of diagnostic messages written to standard error". `grep -n 'gettext' dev/yacc/*.rs` returns no matches. Fix: route diagnostic strings through `gettextrs::gettext()` after `setlocale` per project convention.

#### Minor

- [x] **#9 — `--` end-of-options marker not handled.** `dev/yacc/main.rs:63-143`. POSIX SYNOPSIS states `yacc` "shall conform to XBD Section 12.2 ... except for Guideline 9." XBD 12.2 still requires that `--` terminate options so a grammar file literally named `-foo.y` can be passed. The hand-rolled argv parser at `main.rs:75-127` treats any `-`-prefixed token as an option. Fix: treat `--` as a terminator and pass subsequent argv elements as operands.
- [x] **#10 — `-p sym_prefix` mangles user-defined token names.** `dev/yacc/codegen.rs:225-231` rewrites token `#define`s as `{SYM_PREFIX_UPPER}_{TOKEN}` (e.g. `-p foo` → `FOO_NUM`). POSIX 123655–123660 scopes `-p`'s renaming to "external names produced by yacc" — `yyparse`, `yylex`, `yyerror`, `yylval`, `yychar`, `yydebug` — explicitly *not* user-declared tokens (which the lexer in another translation unit must keep referring to as `NUM`). The "Local names may also be affected" clause is for internal yacc names, not user-visible token defines. Fix: leave user token names alone; rename only the six spec-mandated symbols and yacc-internal locals.
- [ ] **#11 — Undocumented `--strict` flag visible in usage and accepted on the CLI.** `dev/yacc/main.rs:69-74, 146-155`. The flag controls a yacc-internal optimization toggle (`build_packed_tables` consistent-state suppression). It is non-POSIX and not gated behind a feature flag, yet appears in `print_usage()`. Fix: either hide it from `--help`/usage text or move it behind a build feature; do not advertise it as part of the public surface. **WON'T FIX (2026-06-04):** deliberately kept advertised and functional — the integration test harness drives every grammar through both default and `--strict` modes, so the flag is part of the de-facto surface.
- [x] **#12 — Multi-byte / non-ASCII character literals silently accepted.** `dev/yacc/lexer.rs:203-303` (`read_char_literal`) and `dev/yacc/grammar.rs:537-544`. POSIX RATIONALE 124342–124346: "Multi-byte characters should be recognized by the lexical analyzer and returned as tokens. They should *not* be returned as multi-byte character literals." A grammar containing e.g. `'é'` is accepted, the codepoint is cast to `i32`, and used as the token number. Token numbers > 255 silently collide with auto-assigned tokens (starting at 257) if the codepoint lands in that range, but the `'\0'` NUL case at `grammar.rs:543` is the only one rejected (via the duplicate-token-number check against `EOF=0`). Fix: reject character literals with codepoints outside `1..=255` with a clear diagnostic.
- [x] **#13 — Description file omits "Limits for internal tables" report.** `dev/yacc/codegen.rs:1738-1964` (`generate_description_file`). POSIX 123740–123743 + 124312–124326: "Limits for internal tables ... shall also be reported, in an implementation-defined manner." The current "Grammar summary" block reports terminals/non-terminals/rules/states/actions but not the spec's named limit set (`{NTERMS}`, `{NNONTERM}`, `{NPROD}`, `{NSTATES}`, `{MEMSIZE}`, `{ACTSIZE}`). Note: spec explicitly permits "implementation may use dynamic allocation techniques and have no specific limit values to report" — so leaving this blank is conforming, but a one-line "(dynamic, no fixed limits)" annotation would close the spec line item.
- [x] **#14 — `yynerrs` extern declared as a public symbol; not POSIX-mandated.** `dev/yacc/codegen.rs:168` emits `extern int yynerrs;`, and `codegen.rs:932` defines it. POSIX 123655–123660 lists only `yyparse`/`yylex`/`yyerror`/`yylval`/`yychar`/`yydebug` as `-p`-affected external names; `yynerrs` is a historical Bison-ism. Harmless on its own, but flagged because it bloats the prefix-renaming surface and is undocumented. Acceptable to keep, just note it as a non-spec extension.

### Detailed conformance matrix

#### SYNOPSIS / argv parsing

- [x] `-dltv` short options grouping handled — `main.rs:75-127` walks each char of a `-…` cluster.
- [x] `-b` and `-p` accept both `-b prefix` and `-bprefix` forms — `main.rs:79-99, 102-119`.
- [x] Single `grammar` operand required, multiple rejected — `main.rs:128-140`.
- [ ] **`--` end-of-options unsupported.** (#9, Minor) `main.rs:75` treats any `-`-prefixed token as an option.
- [ ] **`--strict` extension exposed in usage text.** (#11, Minor) `main.rs:154`.
- [x] Unknown short options rejected with usage diagnostic — `main.rs:123-125`.

#### OPTIONS

| Opt | Status | Notes (file:line) |
|---|---|---|
| `-b file_prefix` | CONFORMS | `codegen.rs:60-77` uses prefix for `y.tab.c`/`y.tab.h`/`y.output`. |
| `-d` | CONFORMS | `codegen.rs:71-75` writes header file only when set. |
| `-l` | CONFORMS | `codegen.rs:130-131, 144-145, 185-186, 1309-1311, 1710-1711` gates every `#line` emission. |
| `-p sym_prefix` | PARTIAL | Renames functions/variables (#10 Minor: also mangles user token defines, codegen.rs:226-231). |
| `-t` | DIVERGES | (#6 Major) Debug code emission is itself gated on `-t`, not just the YYDEBUG default. |
| `-v` | PARTIAL | (#5 Major) Description file not produced when run aborts before codegen. |

#### OPERANDS / STDIN / INPUT FILES

- [x] `grammar` is a required pathname operand — `main.rs:128-140`.
- [x] STDIN "Not used" — only `fs::read_to_string` of `grammar` is performed (`main.rs:162`); no `stdin()` call anywhere (`grep -n 'stdin' dev/yacc/*.rs` → 0 matches).
- [x] Input file format conforms to EXTENDED DESCRIPTION (declarations / `%%` / rules / optional `%%` / programs) — `parser.rs:179-202`.

#### ENVIRONMENT VARIABLES

| Var | Status | Notes |
|---|---|---|
| `LANG` | MISSING | (#7 Major) Never read. |
| `LC_ALL` | MISSING | (#7 Major) Never read. |
| `LC_CTYPE` | MISSING | (#7 Major) Never read; lexer always uses Rust `char` semantics. |
| `LC_MESSAGES` | MISSING | (#7/#8 Major) No locale-driven diagnostics. |
| `NLSPATH` (XSI) | MISSING | (#8 Major) No message catalog support. |
| `setlocale(LC_ALL, "")` call | MISSING | (#7 Major) Never invoked by `main()`. |

#### ASYNCHRONOUS EVENTS

- [x] Default — yacc is non-interactive and spec says "Default", so no explicit signal handling is required. `grep -nE 'SIGCONT|SIGWINCH|signal_hook|libc::signal' dev/yacc/*.rs` → 0 matches (expected).

#### STDOUT / STDERR

- [x] STDOUT "Not used" — main.rs and codegen.rs only write files; no `println!`/`stdout()` call.
- [x] Diagnostics and conflict reports go to stderr — `diag.rs:107` writes via `io::stderr()`; `main.rs:200-247` uses `eprintln!`.
- [x] Conflict reports are in unspecified format but include counts — `main.rs:213-220, 240-248`.

#### OUTPUT FILES

##### Code file (`y.tab.c`)

| Required by spec | Status | Notes |
|---|---|---|
| `extern int yychar` declaration | CONFORMS | `codegen.rs:167`. |
| `#define YYEMPTY ...` | MISSING | (#1 Critical) Never emitted. |
| `#define YYEOF 0` | MISSING | (#2 Critical) Never emitted. |
| Copy of header `#define`s | CONFORMS | `codegen.rs:138` always emits token defines into code file. |
| `void yyerror(const char *);` prototype | PARTIAL | (#4 Critical) Present (`codegen.rs:174`) but not `#ifndef`-guarded. |
| `int yylex(void);` prototype | PARTIAL | (#4 Critical) Present (`codegen.rs:173`) but not `#ifndef`-guarded. |
| `int yyparse(void);` prototype | MISSING | (#3 Critical) Never emitted. |
| Prototypes after `%{...%}`, before semantic actions | CONFORMS (for the two present) | `codegen.rs:128-133` writes prologue, then `172-175` writes prototypes, then `generate_parser` body. |
| `%union` → `YYSTYPE` typedef + `extern YYSTYPE yylval` | CONFORMS | `codegen.rs:142-161` + `166`. |
| No `main()` declaration unless in `%{...%}` | CONFORMS | `codegen.rs` never emits a `main()` definition. |

##### Header file (`y.tab.h`)

- [x] Contains `#define`s associating token numbers with names — `codegen.rs:1704`.
- [x] If `%union` used, `YYSTYPE` typedef + `extern YYSTYPE yylval` included — `codegen.rs:1708-1730`.
- [x] Does not declare `yyerror()` or `yylex()` — `generate_header_file` does not emit them.

##### Description file (`y.output`)

- [x] Grammar, terminals, non-terminals, state descriptions, conflict summary — `codegen.rs:1748-1958`.
- [ ] **Internal-table limits report** (#13 Minor) — `codegen.rs:1887-1897` reports counts but not the spec's named limit set; acceptable per "no specific limit values to report" but worth a clarifying line.
- [ ] **Always produced when `-v` set** (#5 Major) — fails when an earlier stage errors.

#### EXTENDED DESCRIPTION

##### Lexical structure

- [x] Comments `/* ... */` recognized — `lexer.rs:131-164`.
- [x] Names: letters, periods, underscores, non-initial digits — `lexer.rs:175-186`.
- [x] Character literals support all ISO C escape sequences — `lexer.rs:203-303` (simple, octal, hex).
- [x] `yy`/`YY` reserved-prefix warning — `grammar.rs:407-420`.
- [ ] **Multi-byte char literals not rejected.** (#12 Minor) `lexer.rs:203-303`.

##### Declarations section

- [x] `%token [<tag>] name [number] ...` — `parser.rs:323-400`.
- [x] `%left`/`%right`/`%nonassoc` with precedence assignment in declaration order — `parser.rs:234-251`.
- [x] `%type <tag> name...` requires tag — `parser.rs:402-430`.
- [x] `%start name` overrides default — `parser.rs:256-267`.
- [x] `%union { body }` — `lexer.rs:460-476`, `parser.rs:216-229`.
- [x] `%{ ... %}` prologue copied to code file with `#line` — `parser.rs:208-215`, `codegen.rs:128-134`.
- [x] First-appearance token-number assignment is sticky — `grammar.rs:382-403`.
- [x] Duplicate token numbers raise an error — `grammar.rs:394-402, 423-431`.
- [x] Non-terminal with no rules raises an error — `grammar.rs:349-359`.
- [x] `%expect`/`%expect-rr` extension supported and integrated with conflict reporting — `parser.rs:268-303`, `main.rs:194-256`. (Non-POSIX extension, harmless.)

##### Grammar rules

- [x] `A : BODY ;` with `|` continuation — `parser.rs:432-468`.
- [x] Semantic actions `{ ... }` recognized; mid-rule actions converted to anonymous non-terminals — `parser.rs:518-535`, `grammar.rs:469-491`.
- [x] `%prec name` overrides rule precedence — `parser.rs:499-516`, `grammar.rs:496-509`.
- [x] Default `$$ = $1` for actionless rules with non-empty RHS — `codegen.rs:1313-1324`.
- [x] `$$`, `$n`, `$0`, `$-n` rewriting — `codegen.rs:1517-1688`.
- [x] `$<tag>$` / `$<tag>n` explicit-tag rewriting — `codegen.rs:1551-1597`.
- [x] Warns when `$$`/`$n` lacks declared type under `%union` — `codegen.rs:1538-1547, 1628-1638`.

##### Conflicts

- [x] Precedence/associativity resolution: higher prec wins; equal prec → left=reduce, right=shift, nonassoc=error — `lalr.rs:422-475`.
- [x] Default shift/reduce conflict resolution = shift — `lalr.rs:396-399`.
- [x] Default reduce/reduce conflict resolution = earlier rule — `lalr.rs:402-407`.
- [x] Precedence-resolved conflicts not counted in totals — only unresolved go into `lalr.conflicts`; `count_conflicts` reads from that map (`lalr.rs:57-81`).

##### Error handling

- [x] `error` token reserved, default value 256 — `grammar.rs:153`.
- [x] `%token error <n>` overrides value — `parser.rs:374-394`, `grammar.rs:163-190`.
- [x] `YYERROR` triggers error handling without calling `yyerror` — `codegen.rs:971-973, 1368-1379` (errlab calls yyerror; YYERROR jumps to errlab1 directly).
- [x] `yyerror("syntax error")` called only when not recovering — `codegen.rs:1370-1377`.
- [x] Three-symbol normal-shift recovery counter — `codegen.rs:1421` (`errflag = 3`), `1254` (decrement on shift).
- [x] `yyerrok` resets recovery — `codegen.rs:982`.
- [x] `yyclearin` discards lookahead — `codegen.rs:985`.
- [x] `YYRECOVERING()` returns 1 / 0 — `codegen.rs:991` (`errflag != 0` evaluates to int 0 or 1 in C).
- [x] Pop-until-shift-error / abort on empty stack — `codegen.rs:1421-1469`.
- [x] EOF discards lookahead during recovery → abort — `codegen.rs:1390-1391`.
- [x] `YYACCEPT` → return 0; `YYABORT` → return non-zero — `codegen.rs:976-979, 1473-1481`.

#### Interface to the lexical analyzer

- [x] `yychar` holds returned token; remapped via `yytranslate[]` — `codegen.rs:1158-1211`.
- [x] `yylex()` ≤ 0 treated as YYEOF (0) — `codegen.rs:1160`.
- [x] Final-state + `yychar == 0` → accept — `codegen.rs:1186-1191`.
- [ ] `YYEOF`/`YYEMPTY` macros not made available to lexer code — (#1/#2 Critical). The lexer cannot symbolically test against these values.

#### Algorithms / consistent states

- [x] LALR(1) tables computed — `lalr.rs` + `first_follow.rs` + `lr0.rs`.
- [x] Consistent-state optimization: skip `yylex()` when only one reduce action — `codegen.rs:1140-1155, 698-701`. `--strict` disables it for callers that need exact yylex timing.
- [x] Packed tables formally verified against canonical tables every run — `verify.rs:42`, called from `codegen.rs:526`.

#### EXIT STATUS / CONSEQUENCES OF ERRORS

- [x] 0 on success — `main.rs:261-275` returns normally.
- [x] Non-zero on error — `main.rs:265-273` exits 1 on any error.
- [x] Per-`%expect` mismatch → non-zero exit — `main.rs:194-256`.
- [ ] **`-v` description file not produced on error** (#5 Major).

### Test coverage signal

Tests cover the high-confidence golden paths well (150 `#[test]`s including a CPython 3.9 grammar end-to-end). Gaps that map to findings above:

- [ ] No test asserts `YYEMPTY` / `YYEOF` are present in generated `y.tab.c` (#1, #2).
- [ ] No test asserts `int yyparse(void);` prototype is present (#3).
- [ ] No test asserts `#ifndef yyerror` / `#ifndef yylex` guards (#4).
- [ ] No test exercises the `-v` + early-failure path (#5).
- [ ] No test verifies that a no-`-t` build with `-DYYDEBUG=1` produces working debug output (#6).
- [ ] No test asserts `setlocale` is called or that `LANG`/`LC_MESSAGES` influences diagnostics (#7, #8).
- [ ] No test exercises `--` end-of-options (#9).
- [ ] No test asserts that `-p` leaves user-defined token names unprefixed (#10).
- [ ] No test rejects multi-byte char literals (#12).

### Suggested PR groupings

- **PR A — "Code-file boilerplate per Austin Group Defects 1269 & 1388"**: #1, #2, #3, #4. Tightly coupled (all `generate_code_file` + tests asserting their presence). Smallest unit of work that closes 4 Critical items.
- **PR B — "`-v` description always emitted; full limit report"**: #5, #13. Both touch `generate_description_file` and the run-loop ordering.
- **PR C — "YYDEBUG controls runtime, not codegen"**: #6. Move every `if opts.debug_enabled` gate inside `#if YYDEBUG`/`#endif` and always emit `yydebug`.
- **PR D — "Locale + i18n plumbing"**: #7, #8. Wire `setlocale` and route diagnostics through `gettextrs` per project convention.
- **PR E — "argv conformance + extension hygiene"**: #9, #11. `--` handling; hide `--strict`.
- **PR F — "Token-name preservation under `-p`"**: #10. Surgical fix in `generate_token_defines`.
- **PR G — "Reject multi-byte char literals"**: #12. Lexer-side reject + test.

---

## `lex`

**Implementation:** `dev/lex/` (main.rs 454, lexfile.rs 1369, codegen.rs 2172, dfa.rs 496, nfa.rs 637, pattern_escape.rs 259, pattern_validate.rs 283, diag.rs 120 — ~5.8 kloc)
**Tests:** `dev/tests/lex/mod.rs` (2821 lines)
**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 §3, pp. 3085–3096
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/lex.md`
**Date:** 2026-06-02

### TL;DR

The scanner generator core (NFA → DFA → minimized DFA → direct-coded C, with
start conditions, trailing context, BOL anchoring, REJECT, yymore, yyless,
substitution definitions, %array/%pointer, table-size declarations) is solid.
POSIX conformance gaps cluster in two areas: (a) the generated `lex.yy.c`
emits the lex-library functions `yywrap()` and `main()` inline, which spec
102022–102031 says "shall appear only in the lex library accessible through
the −l l operand" — breaking the conforming-application override path; and
(b) the `input()` helper returns C's `EOF` (-1) where the spec mandates 0, and
`unput()` is declared `static void` where the spec mandates `int unput(int)`.
Locale handling is minimal: `gettext()` decorates clap help strings only,
`setlocale` is never called, and runtime diagnostics are hardcoded English.

### Priority issues

> **Resolved 2026-06-04.** All notable lex items except #L1 are addressed
> (branch `lex-posix-conformance`), each with new tests in
> `dev/tests/lex/mod.rs`:
> - **#L2/#L3** input() returns 0 at EOF; unput() is `int unput(int c)`.
> - **#L6/#L7** `%`-declared table sizes trigger statistics and are
>   documented as advisory (dynamic allocation).
> - **#L8/#L9/#L13** `-o` hidden from `--help`; `-n`/`-v` mutually exclusive;
>   "Output written" chatter removed.
> - **#L11/#L12** warn on C trigraphs in copied code and on NUL escapes in
>   patterns.
> - **#L10** `<STATE><<EOF>>` start-condition-scoped EOF rules.
> - **#L5** runtime diagnostics routed through `gettext()`.
> - **#L4** already closed (plib::diag, 2026-06-03).
> - **#L1 DEFERRED**: yywrap()/main() still emitted by default (the generated
>   yylex() calls yywrap() on every EOF path, so lex.yy.c must link
>   standalone); the library-vs-documented-gate decision is punted.

#### Critical

- [ ] **#L1 — Generated `lex.yy.c` defines `yywrap()` and `main()` instead of leaving them to the lex library.** **DEFERRED (2026-06-04):** the generated yylex() calls yywrap() on every EOF path, so lex.yy.c must link standalone; the library-vs-documented-gate decision is punted. `dev/lex/codegen.rs:1940-1965` (`write_default_yywrap_main`). POSIX 102022–102031: `int yywrap(void)` and `int main(int, char *[])` "shall appear *only* in the lex library accessible through the −l l operand; they can therefore be redefined by a conforming application." Emitting them inline (guarded only by a string-match on `user_subs.contains("yywrap")` / `"int main"`/`"void main"`) means a user who provides their own definitions in a *separate* `.c` file will hit duplicate-symbol linker errors against `-l l`. Fix: drop the default emissions; supply them via a separate `libl` archive shipped alongside the utility, or via a documented `-DYY_NO_DEFAULTS`-style gate.
- [x] **#L2 — `input()` returns `EOF` (-1) where POSIX mandates 0 on end-of-file.** `dev/lex/codegen.rs:761-770`. POSIX 102013–102017: `int input(void)` "Returns the next character from the input, or zero on end-of-file." Implementation does `return getc(yyin);` which yields `EOF` (typically -1). User code that compares `input() == 0` to detect EOF (as POSIX allows) will loop forever; user code that compares against `EOF` (as POSIX does not specify) happens to work. Fix: replace with `int ch = getc(yyin); return ch == EOF ? 0 : ch;`.
- [x] **#L3 — `unput()` signature is `static void unput(int c)` instead of `int unput(int c)`.** `dev/lex/codegen.rs:776-821`. POSIX 102018–102021 prototype is `int unput(int c)`. The function is also declared `static`, which precludes user code in a separate translation unit from calling it (the spec says these "are accessible to user code included in the lex input"; "included in the lex input" arguably permits `static`, but the spec's return-type mandate is unconditional). Fix: change to `int unput(int c) { ...; return c; }` and either drop `static` or document the in-translation-unit constraint.

#### Major

- [x] **#L4 — `setlocale(LC_ALL, "")` is never called; `LC_MESSAGES`/`LC_CTYPE`/`LC_COLLATE`/`LANG`/`LC_ALL`/`NLSPATH` env vars never read.** ✓ closed by cross-cutting plib::diag wiring (2026-06-03). `dev/lex/main.rs:261-369`. POSIX 101713–101732 lists each variable and mandates its effect on execution. `grep -nE 'setlocale|LC_ALL|LC_CTYPE|LC_COLLATE|LC_MESSAGES|NLSPATH|env::var\("LANG' dev/lex/*.rs` returns zero hits. The `gettext()` decorations in `main.rs:29-44` are no-ops without `setlocale`. Fix: call `setlocale(libc::LC_ALL, "")` at the top of `main`.
- [x] **#L5 — Runtime diagnostic messages are hardcoded English.** `dev/lex/diag.rs:107`, every `state.error()`/`state.warning()` call in `dev/lex/lexfile.rs` (e.g. `:166-175, 288, 294, 304, 745, 770, 797`), `dev/lex/main.rs:68, 300, 365`. POSIX 101725–101727: `LC_MESSAGES` shall determine "the format and contents of diagnostic messages written to standard error." `grep -n 'gettext' dev/lex/lexfile.rs` returns no hits — only the clap help strings in `main.rs` are translated, and even those don't take effect without #L4. Fix: route diagnostic strings through `gettextrs::gettext()` after `setlocale`.
- [x] **#L6 — Statistics not emitted when only `%p`/`%n`/`%a`/`%e`/`%k`/`%o` table sizes are declared (no `-v`).** `dev/lex/main.rs:355-362`. POSIX 101742–101745 / 101755–101756: "These statistics *may also* be generated if table sizes are specified with a '%' operator in the Definitions section, as long as the −n option is not specified." The "may" makes this discretionary, but the implementation already parses and stores the table-size declarations (`lexfile.rs:270-279`) and reports them in `write_stats` — so making them gate stats emission too is a small change. As-is, declaring `%n 600` has *no* observable effect (no stats produced, no documentation of how the number affects lex). Fix: emit stats when `!args.no_stats && (args.verbose || !lexinfo.table_sizes.is_empty())`.
- [x] **#L7 — Table-size declarations parsed but their effect is undocumented and untested.** `dev/lex/lexfile.rs:270-279`. POSIX 101836–101842: "The implementation shall document how these numbers affect the lex utility and how they are related to any output that may be generated by the implementation should limitations be encountered. It shall be possible to determine from this output which of the table size values needs to be modified to permit lex to successfully generate tables." Implementation uses dynamic allocation (spec-permitted per RATIONALE) but never produces table-overflow diagnostics referencing these letters, nor does the `--help` text describe their effect. Fix: either document the no-op behavior in `--help`/manpage with a "dynamic allocation; values accepted for compatibility but not used" line, or wire the values into actual size hints / overflow messages.

#### Minor

- [x] **#L8 — Non-POSIX `-o`/`--outfile` option exposed in usage.** `dev/lex/main.rs:40-41`. SYNOPSIS in spec (101680) is `lex [-t] [-n|-v] [file...]`. There is no `-o`. The Bison-style extension is harmless functionally but pollutes the public CLI surface. Fix: hide with `#[arg(hide = true)]` or move behind a feature flag.
- [x] **#L9 — `-n` and `-v` are not mutually exclusive at the CLI level.** `dev/lex/main.rs:31-38`. POSIX SYNOPSIS uses `[-n|-v]` notation; while implementations historically allowed both with one winning, clap can enforce mutual exclusion via `conflicts_with`. Current `args.verbose && !args.no_stats` (`main.rs:355`) silently lets `-n` win, which works but doesn't surface user error. Fix: add `conflicts_with = "verbose"` on `-n` (or vice versa).
- [x] **#L10 — `<<EOF>>` rules don't support start-condition prefixes.** `dev/lex/lexfile.rs:738-761`. The handler explicitly hard-codes `start_conditions: Vec::new()` with comment "<<EOF>> doesn't support start conditions in this simple impl". POSIX is silent on `<<EOF>>` (it's a flex extension), so this is not a spec violation — but the codegen does branch on `is_eof` rules with start conditions (`codegen.rs:687-748` handles "single conditional EOF rule"), suggesting the gap is recognized internally. Fix: parse `<STATE><<EOF>>` form, or document the limitation.
- [x] **#L11 — No detection of C-language trigraphs in copied code blocks.** `dev/lex/lexfile.rs:217-232, 229-230` (the `%{ … %}` and `<blank>`-prefixed copy paths). POSIX 101797: "C-language code in the input shall not contain C-language trigraphs." This is an *application* obligation, not a lex obligation — flagging only as a candidate diagnostic enhancement, not a spec violation. Confirmed CONFORMS but useful as a future quality warning.
- [x] **#L12 — Bracketed character class with NUL (`\0`) gives undefined behavior per spec.** `dev/lex/pattern_escape.rs`, octal/hex escape paths. POSIX 101898–101900: "If all of the digits are 0 (that is, representation of the NUL character), the behavior is undefined." Implementation appears to translate `\0` through to regex_syntax which may accept it. Fix: emit a warning when `\0`/`\x00`/`\000` appears in a pattern.
- [x] **#L13 — "Output written to <file>" extra eprintln is non-POSIX chatter.** `dev/lex/main.rs:365`. Harmless, but POSIX STDERR (101746–101756) only describes diagnostic / statistics messages — emitting a success notice on every run differs from historical lex. Fix: gate behind a `--verbose-build`-style flag or remove.

### Detailed conformance matrix

#### SYNOPSIS / argv parsing

- [x] `-t`, `-n`, `-v` accepted via clap — `main.rs:31-38`.
- [x] `--` end-of-options handled (clap default).
- [x] Bundled short options (`-tnv`) handled (clap default).
- [x] Multiple file operands accepted — `main.rs:43-44`.
- [x] `-` operand routes to stdin at that position in the file list — `main.rs:51-56`.
- [x] No files → stdin — `main.rs:266-268`.
- [ ] **`-o`/`--outfile` non-POSIX option exposed.** (#L8 Minor).
- [ ] **`-n` and `-v` not enforced as mutually exclusive.** (#L9 Minor).

#### OPTIONS

| Opt | Status | Notes (file:line) |
|---|---|---|
| `-n` | CONFORMS | `main.rs:31-32, 355`. |
| `-t` | CONFORMS | `main.rs:34-35, 328-332` switches output to stdout; `355-360` routes stats to stderr per spec 101698-9. |
| `-v` | PARTIAL | (#L6 Major) Only emits stats on `-v`, not on `%`-declared table sizes. |
| `-o file` | DIVERGES | (#L8 Minor) Non-POSIX extension. |

#### OPERANDS / STDIN / INPUT FILES

- [x] `file...` operands concatenated to form single lex program — `main.rs:48-76`, `286`.
- [x] `-` operand reads stdin — `main.rs:52-53`.
- [x] No file operands → reads stdin — `main.rs:266-268`.
- [x] Input files are text — `read_line` based parsing in `concat_input_files`.

#### ENVIRONMENT VARIABLES

| Var | Status | Notes |
|---|---|---|
| `LANG` | MISSING | (#L4 Major) Never read; no `setlocale`. |
| `LC_ALL` | MISSING | (#L4 Major) Never read. |
| `LC_COLLATE` | MISSING | (#L4 Major) Affects ranges/equivalence classes — but `pattern_escape.rs:97, 141` already assumes POSIX locale, which the spec at 101729-32 explicitly allows ("behavior is unspecified" if non-POSIX). Strictly conforming, just with an `unspecified` outcome rather than honor. |
| `LC_CTYPE` | MISSING | (#L4 Major) Same as `LC_COLLATE` — POSIX locale assumed. |
| `LC_MESSAGES` | MISSING | (#L4/#L5 Major) Diagnostics hardcoded English. |
| `NLSPATH` (XSI) | MISSING | (#L5 Major) No message catalog support. |
| `setlocale(LC_ALL, "")` | MISSING | (#L4 Major) Never invoked. |

#### ASYNCHRONOUS EVENTS

- [x] Default — lex is non-interactive batch utility; spec says "Default", no explicit handlers required. `grep -nE 'SIGCONT|SIGWINCH|signal_hook|libc::signal' dev/lex/*.rs` → 0 matches.

#### STDOUT / STDERR

- [x] `-t` set → C source to stdout, stats to stderr — `main.rs:328-332, 356-360`.
- [x] `-t` not set → C source to `lex.yy.c`, stats to stdout — `main.rs:331, 357-360`.
- [x] Diagnostic messages target stderr — `diag.rs:107` writes via `io::stderr()`.
- [ ] **"Output written to ..." chatty notice** (#L13 Minor) — `main.rs:365`.

#### OUTPUT FILES (`lex.yy.c`)

| Spec mandate | Status | Notes |
|---|---|---|
| `yytext` declared (array or pointer) | CONFORMS | `codegen.rs:312-325`. Default = pointer (impl-defined per spec 101822). |
| `yyleng` declared | CONFORMS | `codegen.rs:330` (`int yyleng;`). |
| `yyin` declared | CONFORMS | `codegen.rs:331`. |
| `yyout` declared and defaults to stdout | CONFORMS | `codegen.rs:332, 1163`. |
| Default action: copy unmatched input to output | CONFORMS | `codegen.rs:1434, 1487, 1619, 1809` emit `putc(*YYTOKEN++, yyout)`. |
| `int yylex(void)` generated | CONFORMS | `codegen.rs:1158`. |
| `int yymore(void)` accessible to user code | CONFORMS | `codegen.rs:389-391` `#define yymore() (yy_more_flag = 1)`. |
| `int yyless(int n)` accessible to user code | CONFORMS | `codegen.rs:394-400`. |
| `int input(void)` returns 0 on EOF | DIVERGES | (#L2 Critical) Returns `EOF` (-1). |
| `int unput(int c)` | DIVERGES | (#L3 Critical) Signature is `static void unput(int c)`. |
| `int yywrap(void)` only in lex library | DIVERGES | (#L1 Critical) Emitted inline at `codegen.rs:1944-1948`. |
| `int main(int, char *[])` only in lex library | DIVERGES | (#L1 Critical) Emitted inline at `codegen.rs:1959-1964`. |
| Non-{input,unput,main} external names begin with `yy`/`YY` | CONFORMS | All other generated names checked: `yytext`, `yyleng`, `yyin`, `yyout`, `yylex`, `yywrap`, `yymore`, `yyless`, `yy_*` internals, plus spec-named macros `ECHO`/`REJECT`/`BEGIN`/`INITIAL`/`YY_START`. |

#### EXTENDED DESCRIPTION

##### Lexical structure / sections

- [x] Three sections separated by `%%` — `lexfile.rs:255-256, 837-838`.
- [x] First `%%` required; second optional — `lexfile.rs:837-839` only transitions to `UserCode` if second `%%` seen.
- [x] Lines starting with `<blank>` in Definitions copied to external def area — `lexfile.rs:297-299`.
- [x] `%{ … %}` blocks in Definitions copied verbatim — `lexfile.rs:217-232`.
- [x] Indented / `%{ … %}` content at start of Rules section copied into `yylex()` body — `lexfile.rs:851-852` pushes to `internal_defs`; codegen places these inside `yylex` before main loop (per `internal_defs` use in codegen).

##### Definitions

- [x] `name substitute` substitution definitions — `lexfile.rs:300-303`, expansion at `560-697`.
- [x] `{name}` substitution recognition (not inside `[ ]` or `"..."`) — `lexfile.rs:563-690` tracks `in_brace`, `in_quotes` state.
- [x] `%s`/`%start` inclusive start conditions — `lexfile.rs:258-260`.
- [x] `%x` exclusive start conditions — `lexfile.rs:261-263`.
- [x] `%array` / `%pointer` selection — `lexfile.rs:264-269`.
- [x] Table-size declarations `%p %n %a %e %k %o` accepted — `lexfile.rs:270-279`.
- [ ] **Table-size declarations have no documented effect** (#L7 Major).

##### Rules

- [x] ERE followed by `<blank>+` then action — `lexfile.rs:737-813`.
- [x] `<state>r` / `<state1,state2,…>r` start-condition prefix — `lexfile.rs:701-721`.
- [x] `r/x` trailing context — `pattern_validate::parse_anchoring_and_trailing_context`, `main.rs:127-145`.
- [x] `^r` BOL anchor — handled by `pattern_validate::parse_anchoring_and_trailing_context`, `rule.bol_anchor`.
- [x] `r$` EOL anchor (equivalent to `r/\n`) — same path; recorded as trailing context.
- [x] `"..."` literal strings with `\`-escapes — `lexfile.rs:605-650`.
- [x] Substitution wrap-in-parens for quantifier correctness — `lexfile.rs:664-670`.
- [x] Action `;` (empty C statement) valid — accepted as non-empty action text.
- [x] Missing action diagnosed — `lexfile.rs:796-798`.
- [x] Multi-line braced actions handled — `lexfile.rs:315-387, 844-850`.

##### Special actions

- [x] `|` fall-through to next rule's action — covered by codegen rule dispatching (shared accept state to next rule).
- [x] `ECHO` macro — `codegen.rs:258-260`.
- [x] `REJECT` macro + history stack — `codegen.rs:381-383, 359-374, 1497+`.
- [x] `BEGIN(newstate)` — `codegen.rs:282-309`. `BEGIN(0)` and `BEGIN(INITIAL)` both work.

##### Functions / macros visible to user code

| Symbol | Status | Notes |
|---|---|---|
| `int yylex(void)` | CONFORMS | `codegen.rs:1158`. |
| `int yymore(void)` | CONFORMS | Macro at `codegen.rs:389-391`. |
| `int yyless(int n)` | CONFORMS | Macro at `codegen.rs:394-400`. |
| `int input(void)` | DIVERGES | (#L2) wrong EOF return. |
| `int unput(int c)` | DIVERGES | (#L3) wrong signature. |
| `int yywrap(void)` | DIVERGES | (#L1) should be in libl, not lex.yy.c. |
| `int main(int, char *[])` | DIVERGES | (#L1) should be in libl, not lex.yy.c. |

##### Regular expressions in lex

- [x] ERE per XBD §9.4 used as base — `regex_syntax` with `dot_matches_new_line(false)` (`main.rs:103-104`).
- [x] `^` only at start, `$` only at end — `pattern_validate::validate_pattern_restrictions` (`lexfile.rs:770`).
- [x] `/` trailing-context restrictions (r cannot include further `/` or `$`; x cannot include `^`, `/`, `$`) — `pattern_validate`.
- [x] `\<digits>` octal escapes — `pattern_escape::translate_escape_sequences`.
- [x] `\x<digits>` hex escapes — same.
- [x] `[...]` bracket expressions including `[:class:]`, `[=c=]`, `[.c.]` — `lexfile.rs:448-466`, `pattern_escape::expand_posix_bracket_constructs`.
- [x] `.` does not match `<newline>` — `main.rs:104`.
- [ ] **`\0`/`\x00` undefined-behavior NUL not warned** (#L12 Minor).
- [ ] **Trigraphs not flagged in copied C blocks** (#L11 Minor; spec is an app constraint).

#### Interactive commands

- [x] N/A — lex is non-interactive.

#### EXIT STATUS / CONSEQUENCES OF ERRORS

- [x] 0 on success — `main.rs:368`.
- [x] Non-zero on error — `main.rs:293` exits 1 if parse errors recorded by `diag::has_errors()`; `?` propagates other errors to `main` which returns `Err`.
- [x] State of `lex.yy.c` unspecified on non-zero exit — `main.rs:331` truncates the file at `create`, and any later error leaves it partially written. Acceptable per spec 101687-9: "state of this file is unspecified if lex exits with a non-zero exit status."
- [x] CONSEQUENCES OF ERRORS = Default — no special policy required.

### Test coverage signal

Tests cover end-to-end generation and many ERE edge cases. Gaps that map to findings:

- [ ] No test asserts `input()` returns 0 (not -1) on EOF (#L2).
- [ ] No test asserts `unput()` is declared `int unput(int)` (#L3).
- [ ] No test verifies that user-provided `yywrap`/`main` in a *separate* translation unit do not collide with the generated defaults (#L1).
- [ ] No test exercises `setlocale` or `LC_MESSAGES`-driven diagnostics (#L4, #L5).
- [ ] No test exercises stats emission triggered by `%n`/`%p` declarations alone (#L6).
- [ ] No test exercises `<STATE><<EOF>>` start-conditioned EOF rules (#L10).

### Suggested PR groupings

- **PR A — "Move `yywrap`/`main` out of lex.yy.c per POSIX 102022-31"**: #L1. Largest single change; ship a tiny `libl`-equivalent (or document linkage path) alongside.
- **PR B — "Fix `input()`/`unput()` signatures and EOF semantics"**: #L2, #L3. Both surgical edits in `write_helper_functions`.
- **PR C — "Locale + i18n plumbing"**: #L4, #L5. `setlocale` in `main`; route runtime diagnostics through `gettext`.
- **PR D — "Honor table-size declarations as stats trigger; document effect"**: #L6, #L7.
- **PR E — "argv hygiene"**: #L8, #L9, #L13. Hide `-o`, enforce `-n`/`-v` mutual exclusion, silence the "Output written" notice (or gate it).
- **PR F — "Pattern edge-case diagnostics"**: #L11, #L12. Warn on trigraphs in copied blocks; warn on `\0` in patterns.
- **PR G — "`<STATE><<EOF>>` parsing"**: #L10. Lexfile-side fix + test.

---

## `ar`

**Implementation:** `dev/ar.rs` (776 lines, single file)
**Tests:** `dev/tests/dev-tests.rs` ar_* group (~26 tests; binary `.correct.a` golden files in `dev/tests/ar/`)
**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 §3, pp. 2632–2639
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/ar.md`
**Date:** 2026-06-02

### TL;DR

Functionally implements the seven mode flags (`-d`/`-m`/`-p`/`-q`/`-r`/`-t`/`-x`) plus the `-a`/`-b`/`-i` positioning options and most of the modifiers — but two serious correctness bugs land in the Critical bucket. First, `ArchiveMember::read` stores `SystemTime::elapsed()` (seconds *since* the file's mtime, i.e. its age) in the archive's `date` header field, where every other tool expects a Unix timestamp; every newly-archived file ends up showing a date near `1970-01-01` on `ar -tv` and breaks `ar -ru`. Second, member-name comparison uses the operand verbatim instead of its last pathname component, so `ar -d archive subdir/foo.o` silently fails to find `foo.o` in the archive. Mode-flag bundling (`ar -dv`, `ar -rv`, `ar -tv`) is also not recognized because `-d`/`-r`/`-t` are wired as separate clap subcommand *names*. The `-T` flag is parsed but never consulted; the `-m` subcommand lacks any `-v` field at all. No locale handling (`LC_TIME`/`TZ`/`LC_MESSAGES`/`NLSPATH`) is in place.

### Priority issues

> **Resolved 2026-06-05.** #A1-A6, #A8, #A9, #A11 are closed and #A7's
> string-translation remainder is done (branch `dev-utils-posix-conformance`),
> each with new tests: date = mtime epoch + operand-basename matching; bundled
> mode flags (`-rv`/`-tv`/`-dv`); `-T` truncation + `-m -v`; System V `//`
> long-name table; `-p` operand prefix, ls-style setuid/setgid/sticky mode
> bits, clearer `-r` no-files diagnostic, and gettext-routed diagnostics.
> #A10/#A7-locale were already closed. #A12 (clap `--help`) is left as a
> harmless note.

#### Critical

- [x] **#A1 — `ArchiveMember::read` stores file *age* (not Unix epoch seconds) in the date field.** `dev/ar.rs:180-184`:
  ```
  let date = file_metadata.modified().ok()
      .map(|t| t.elapsed().ok().map(|d| d.as_secs()).unwrap_or_default())
      .unwrap_or_default();
  ```
  `SystemTime::elapsed()` returns "how long ago this time was" — for a file modified an hour ago it returns `3600`, which is then written to the ASCII `date` header. POSIX RATIONALE 84572 (BSD archive description) and 84507-84513 (`-tv` date format) treat that field as the file's `st_mtime` (Unix epoch). Symptoms: `ar -tv` lists every newly-archived member with a date near `1970-01-01`; `ar -ru` (#A1-dependent) compares age-in-seconds against a real timestamp and chooses wrongly; round-tripping through another `ar` reads the bogus value back. Fix: use `t.duration_since(SystemTime::UNIX_EPOCH)?.as_secs()`. The `ar_compare_approx_test` helper masks this regression by fuzzing date bytes — add a strict-date test.
- [x] **#A2 — Operand-to-archive-member lookup uses the full operand instead of its last pathname component.** Spec 84379-84380: "The comparison of file operands to the names of files in archives shall be performed by comparing the last component of the operand to the name of the file in the archive." Affected sites: `delete_cmd` (`dev/ar.rs:476` passes `&file` whole), `move_cmd` (`:498, :506-507` use `posname` and `file` whole), `print_cmd` (`:553`), `list_cmd` (`:703`), `extract_cmd` (`:743`). Only `replace_cmd` (`:624`) does the right thing (`Path::new(file).file_name().unwrap()`). Result: `ar -d libfoo.a some/dir/bar.o` silently leaves `bar.o` in place because no archive member has the literal name `some/dir/bar.o`. Fix: route every operand through `Path::new(op).file_name().unwrap_or(op.as_ref())` before calling `member_index`.

#### Major

- [x] **#A3 — Bundled mode-flag + modifier (`ar -dv`, `-rv`, `-tv`, `-xv`, `-pv`, `-cv`) is not recognized.** `dev/ar.rs:118-134` declares the mode flags as clap *subcommand names* (`#[command(name = "-d", …)]`, etc.). Clap matches the subcommand by exact token, so `ar -dv archive file...` errors out because no subcommand `-dv` exists. POSIX SYNOPSIS (84349-84361) shows the mode flag and modifiers as separate option letters; XBD 12.2 (which ar conforms to, sans Guideline 9) mandates that grouped single-char options like `-dv` be equivalent to `-d -v`. Fix: parse the first argv token by hand (split mode letter from any trailing letters) and feed the canonicalized form to clap, or restructure to a top-level flag set instead of subcommands.
- [x] **#A4 — `-T` (allow filename truncation) is accepted but never consulted.** `dev/ar.rs:111-113` declares `allow_truncation`, but `extract_member` (`:720-731`) creates the file at `Path::new(&member.name)` with no consideration of NAME_MAX or the flag. Spec 84418-84421: "By default, extracting a file with a name that is too long shall be an error; a diagnostic message shall be written and the file shall not be created. The −T option allows truncation." Today: neither the default error nor the `-T` truncation behavior is implemented. Fix: stat `_PC_NAME_MAX` for the parent dir; without `-T`, error when `member.name.len() > NAME_MAX`; with `-T`, truncate.
- [x] **#A5 — `-v` is not accepted on the `-m` (move) subcommand.** `dev/ar.rs:39-45` (`MoveArgs`) has only `insert_args` and `files`. Spec SYNOPSIS 84350-84353: `ar -m [-v] archive file...` (and the `-a`/`-b`/`-i` variants), all with optional `-v`. `ar -m -v archive file` is rejected. Fix: add `#[arg(short = 'v')] verbose: bool` to `MoveArgs` and wire it through `move_cmd` for `"m - %s\n"` per historical ar (spec leaves the move-verbose format unspecified — match historical practice).
- [x] **#A6 — Filenames longer than 15 bytes are rejected outright; no System V long-name (`//`) member is written.** `dev/ar.rs:437-447` (`format_name_for_header`) errors with "file name is too long" when `name.len() > 15`. Spec OPERANDS 84443-84445: "The implementation's archive format shall not truncate valid filenames of files added to the archive" — i.e., long names must be supported by the archive format, not rejected. The chosen System V format addresses this via a `//` string-table member (offsets via `/n` name encoding); the implementation generates the magic, member headers, and symbol-table member but never a `//` long-name table. Fix: emit a `//` member when any name exceeds 15 bytes and encode such names as `/<offset>`.
- [x] **#A7 — Locale handling: `setlocale` never called; `LC_TIME`, `TZ`, `LC_MESSAGES`, `NLSPATH`, `TMPDIR` ignored.** ✓ partially closed by cross-cutting work (2026-06-03): `setlocale` now called via `plib::diag::init_locale`; `-tv` date formatting routes through `plib::locale::strftime` (honors LC_TIME + TZ). LC_MESSAGES string-translation and TMPDIR remain open. `dev/ar.rs:765-776` (main). Spec ENVIRONMENT VARIABLES 84452-84469 lists all of these. Concrete consequences: (a) `list_member` (`:677, :685`) builds `DateTime::from_timestamp(...)` in UTC and formats with the hardcoded `DATE_FORMAT = "%b %e %H:%M %Y"`, so `TZ` and `LC_TIME` cannot alter the date column the spec mandates at 84507-84515; (b) every `eprintln!("ar: ...")` and `format!("ar: {}: ...", ...)` site (`:165, :169, :239, :243, :439, :491, :499, :515, :530, :560, :580, :603, :608, :617, :707, :751`) is hardcoded English, defeating `LC_MESSAGES`; (c) `gettext()` decorations on clap help strings (`:23, :32, …`) are no-ops without `setlocale`. Fix: call `setlocale(libc::LC_ALL, "")` at entry; switch `DateTime::format` to a locale-aware path (or call `strftime(3)` via libc); route runtime diagnostic strings through `gettext`.

#### Minor

- [x] **#A8 — `-p` verbose prefix uses the archive member name even when file operands were given.** `dev/ar.rs:552-558` always writes `print!("\n<{}>\n\n", member.name.to_string_lossy())`. Spec STDOUT 84476-84479: "where file is the operand specified on the command line, if file operands were specified, and the name of the file in the archive if they were not." Fix: write `file` (the operand) inside the `if !args.files.is_empty()` branch.
- [x] **#A9 — `format_mode` does not render setuid / setgid / sticky bits.** `dev/ar.rs:666-674` returns nine chars from a `["---", … "rwx"]` table indexed by 3-bit triples. Spec STDOUT 84500-84504: "<member mode> Shall be formatted the same as the <file mode> string defined in the STDOUT section of `ls`, except that the first character, the <entry type>, is not used." `ls`'s mode string encodes `S/s` in the exec-x position for setuid (bit `0o4000`) / setgid (bit `0o2000`) and `T/t` in the world-x position for sticky (`0o1000`). Fix: post-process the third character of each triple based on the high three mode bits.
- [x] **#A10 — Archive updates are non-atomic; a crash during write leaves a truncated archive.** ✓ closed by cross-cutting plib::io::write_atomic (2026-06-03): all 8 sites now write to a tempfile and `rename(2)` over the target. `dev/ar.rs:483-484, 519-520, 533-534, 565-566, 592-593, 660-661, 714-715, 758-759` all do `std::fs::File::create(archive_path)` (truncate-on-open) followed by streaming `archive.write`. POSIX doesn't strictly mandate atomicity, but CONSEQUENCES OF ERRORS = Default + the spec's "archive ... can be moved as a file" model strongly implies the file is in a defined state. Fix: write to `archive_path.with_extension(".tmp.XXXXXX")` via `tempfile` then `rename(2)` over the original. Wires into a future `TMPDIR` (#A7) honor.
- [x] **#A11 — `-r` with no `file` operands errors instead of leaving behavior undefined.** `dev/ar.rs:607-609` returns `"ar: missing archive operand"` (and `:606-611` requires at least the archive path). Spec 84408-84409: "If no files are specified and the archive exists, the results are undefined." An error is a permissible interpretation of "undefined," but historical ar is silent + non-zero exit; the diagnostic mistakenly claims the *archive* is missing when it's actually the *file* list. Fix: produce a clearer diagnostic, or no-op silently with a non-zero exit.
- [ ] **#A12 — `--version` / `--help` are reachable but POSIX SYNOPSIS doesn't list them.** Inherited from clap; harmless extension. Worth noting only because clap's auto-generated `--help` interleaves the subcommand-named mode flags in a way that misleads readers about the POSIX surface.

### Detailed conformance matrix

#### SYNOPSIS / argv parsing

- [x] All seven mode flags (`-d`, `-m`, `-p`, `-q`, `-r`, `-t`, `-x`) routable — `dev/ar.rs:118-134`.
- [x] `-a` / `-b` / `-i` mutually exclusive (`InsertArgs` group `multiple = false`) — `:20-28`.
- [x] `-i` accepted as alias of `-b` (`short_alias = 'i'`) — `:26`.
- [x] `--` end-of-options handled by clap.
- [ ] **Mode-flag bundling (`-dv`, `-rv`, `-tv`, `-xv`, …) rejected.** (#A3 Major).
- [ ] **`-T` parsed but inert.** (#A4 Major).
- [ ] **No `-v` on `-m`.** (#A5 Major).

#### OPTIONS

| Spec opt | Status | Notes (file:line) |
|---|---|---|
| `-d` (delete) | CONFORMS | `:472-486`. |
| `-m` (move) | PARTIAL | (#A5 Major) Missing `-v`. |
| `-p` (print) | PARTIAL | (#A8 Minor) Verbose name source wrong; (#A2) operand lookup. |
| `-q` (quick append) | CONFORMS | `:572-596`. `-v` accepted but spec defines no format; impl silently ignores (acceptable). |
| `-r` (replace/add) | CONFORMS | `:598-664`. |
| `-t` (list) | PARTIAL | (#A1 Critical) bogus dates; (#A2) operand lookup; (#A9 Minor) mode flags. |
| `-x` (extract) | PARTIAL | (#A4 Major) `-T` no-op; (#A2) operand lookup. |
| `-a` posname | CONFORMS | `:489-535`. |
| `-b` posname | CONFORMS | same. |
| `-i` posname | CONFORMS | alias of `-b`. |
| `-c` (no create msg) | CONFORMS | `:579-581, :616-618`. |
| `-C` (no-replace on extract) | CONFORMS | `:720-724`. |
| `-s` (force symtab regen) | CONFORMS | `:564-567, :713-716, :757-760` — works for `-p`/`-t`/`-x`. (Implicit for `-d`/`-m`/`-q`/`-r` because they always rewrite the symtab.) |
| `-T` (allow truncation) | MISSING | (#A4 Major). |
| `-u` (update on `-r`) | PARTIAL | (#A1) Logic correct; comparison values are wrong because `member.date` is age, not epoch. |
| `-v` (verbose) | PARTIAL | (#A5 Major) Missing on `-m`. |

#### OPERANDS / STDIN

- [x] `archive` pathname operand accepted — every subcommand's first positional.
- [x] `file` operands accepted variadic — every subcommand.
- [ ] **Operand basename comparison missing** (#A2 Critical) — affects `-d`/`-m`/`-p`/`-t`/`-x`.
- [x] STDIN "Not used" — `grep -n 'stdin' dev/ar.rs` → 0 matches.

#### INPUT FILES

- [x] Archive parsing via `object::read::archive::ArchiveFile::parse` — `:247`. System V/GNU format read.
- [x] Errors on malformed archive — `:254` returns `"ar: invalid archive format"`.

#### ENVIRONMENT VARIABLES

| Var | Status | Notes |
|---|---|---|
| `LANG` | MISSING | (#A7 Major) Never read; no `setlocale`. |
| `LC_ALL` | MISSING | (#A7 Major) Never read. |
| `LC_CTYPE` | MISSING | (#A7 Major) UTF-8 assumed via Rust default; archive bytes handled via `OsStr`/`Vec<u8>` so single-byte content survives, but multi-byte-locale operand collation isn't honored. |
| `LC_MESSAGES` | MISSING | (#A7 Major) All diagnostics English. |
| `LC_TIME` | MISSING | (#A7 Major) `list_member` uses fixed `%b %e %H:%M %Y` regardless of locale. |
| `NLSPATH` (XSI) | MISSING | (#A7 Major) No message catalog. |
| `TMPDIR` | MISSING | (#A11 Minor) Not used because archive is rewritten in place (#A10). |
| `TZ` | MISSING | (#A7 Major) `DateTime::from_timestamp` is UTC; no `TZ` consultation. |

#### ASYNCHRONOUS EVENTS

- [x] Default — ar is non-interactive; spec says "Default". `grep -nE 'SIGCONT|SIGWINCH|signal_hook|libc::signal' dev/ar.rs` → 0 matches.

#### STDOUT

| Spec format | Status | Notes |
|---|---|---|
| `-dv` → `"d - %s\n"` | CONFORMS | `:478` matches. |
| `-pv` → `"\n<%s>\n\n"` then contents | PARTIAL | (#A8 Minor) member name vs operand. |
| `-rv` add → `"a - %s\n"` | CONFORMS | `:638`. |
| `-rv` replace → `"r - %s\n"` | CONFORMS | `:633`. |
| `-t` → `"%s\n", <file>` | CONFORMS | `:689`. |
| `-tv` → mode user/group size mon day hr:min year file | CONFORMS-ish | `:679-686` matches the field order/format; (#A1 Critical) values for date are garbage; (#A9 Minor) setuid/setgid/sticky bits not shown. |
| `-xv` → `"x - %s\n"` | CONFORMS | `:726`. |

#### STDERR

- [x] Only diagnostic messages on stderr — every error path uses `eprintln!`/`Err(...)` which Termination prints to stderr.
- [x] Create-archive diagnostic written when `-c` not set — `:580, :617`.
- [x] Create-archive diagnostic does not change exit status — neither call short-circuits.

#### OUTPUT FILES (archive format)

- [x] System V / GNU format chosen (per RATIONALE 84538-84542 "format is not described"); magic `!<arch>\n`, 60-byte ASCII headers, terminator `\`<newline>` — `:286, :199-219, `object` crate constants.
- [x] Symbol table member (`/`) emitted before file members — `:286-292`.
- [x] Symbol table from text/data/TLS symbols of recognized object files — `:455-470`.
- [x] 2-byte alignment with newline pad — `:204, :214-216`.
- [ ] **No `//` long-name table** (#A6 Major).
- [ ] **Date field is age-in-seconds, not Unix epoch** (#A1 Critical).

#### EXIT STATUS / CONSEQUENCES OF ERRORS

- [x] 0 on success — `main` returns `Ok(())` → exit 0.
- [x] Non-zero on error — `Result::Err` via `?` propagates to `main`; Rust `Termination` for `Box<dyn Error>` exits non-zero.
- [x] CONSEQUENCES OF ERRORS = Default — no special policy required.
- [ ] **Non-atomic archive updates** (#A10 Minor) — leaves archive truncated on crash.

### Test coverage signal

- `ar_compare_test` does byte-exact comparison; `ar_compare_approx_test` is used for archive-creation paths and (per the helper name) likely fuzzes date/uid/gid bytes — which is why #A1 has gone undetected.
- No test verifies that `ar -d archive subdir/foo.o` finds `foo.o` (#A2).
- No test exercises mode-flag bundling `ar -dv …` / `ar -rv …` / `ar -tv …` (#A3).
- No test exercises long filenames > 15 bytes (#A6).
- No test exercises `-T` on a name exceeding NAME_MAX (#A4).
- No test exercises `-m -v` (#A5).
- No test exercises locale-driven date format (#A7).
- `ar_print_test` exists for `-p`/`-pv`/`-p`-some, but no test pins the "operand vs archive name" choice in the prefix (#A8).

### Suggested PR groupings

- **PR A — "Fix archive date field and operand basename comparison"**: #A1, #A2. The two Critical correctness bugs; trivial diffs, both need new (strict, non-approx) tests.
- **PR B — "Mode-flag bundling per XBD 12.2"**: #A3. Restructure CLI from clap subcommands to a top-level flag matrix, or pre-canonicalize argv. Likely the largest single change.
- **PR C — "Implement `-T` and the missing default-extract truncation diagnostic"**: #A4. New code in `extract_cmd`/`extract_member`, plus `_PC_NAME_MAX` lookup.
- **PR D — "Long filename support via `//` member"**: #A6. Touches `Archive::write_symbol_table`-adjacent code and `format_name_for_header`.
- **PR E — "`-v` on `-m`"**: #A5. Smallest fix; add field + use site + test.
- **PR F — "Locale + i18n plumbing"**: #A7. `setlocale`, `gettext`-route diagnostics, locale-aware `-tv` date formatting (`strftime` via libc, or chrono with `Local`+`TZ`).
- **PR G — "Output hygiene"**: #A8, #A9, #A11. Verbose prefix source; mode-string special bits; clearer `-r` no-files diagnostic.
- **PR H — "Atomic archive rewrite"**: #A10. Temp-file + rename; wires `TMPDIR` honor as a bonus.

---

## `nm`

**Implementation:** `dev/nm.rs` (144 lines, single file)
**Tests:** none — no `test_nm_*` entries in `dev/tests/dev-tests.rs`; no fixtures directory.
**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 §3, pp. 3265–3269
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/nm.md`
**Date:** 2026-06-02

### TL;DR

This is a stub. The file opens with a TODO header acknowledging it: "vary output based on args / sort output." Every spec option (`-A`, `-e`, `-f`, `-g`, `-o`, `-P`, `-t`, `-u`, `-v`, `-x`) parses fine via clap, but with one tiny exception (the `SymbolKind::Section | SymbolKind::File` skip) *not one* of them is consulted in the printing path. The `file` operand is also declared as `String`, not `Vec<String>`, so `nm a.o b.o` errors at the clap layer; archive (`.a`) input is unhandled because `object::File::parse` doesn't accept ar archives; output is always 16-hex-digit value + type letter + name, never the `-P` portable format and never sorted; and the two `println!("Failed to ...")` diagnostic sites in `show_object_file` write to stdout, not stderr. There are no tests at all. This audit treats the file as the punch list for actually implementing the utility.

### Priority issues

> **Resolved 2026-06-05.** nm was reimplemented from a stub
> (branch `dev-utils-posix-conformance`): #N1-N11 and #N13-N17 are closed
> (#N12 was already closed). Variadic `file...` operands, ar-archive input,
> the `-P` portable format with `-t`/`-o`/`-x` radix + size column,
> name/value sort via `strcoll`, `-g`/`-u`/`-e` filtering, `-f`, `-A`,
> per-file/per-member headers, gettext diagnostics, `-g`/`-u` mutual
> exclusion, dropped non-POSIX `--long` aliases, and a newline-in-pathname
> check. Adds a cc-built fixture and 10 behavioral tests.

#### Critical

- [x] **#N1 — Only one `file` operand accepted; `file...` (variadic) is the spec.** `dev/nm.rs:65-66` declares `file: String`. POSIX SYNOPSIS 108708/108709 mandates `file...`. Today `nm a.o b.o` errors at clap with "unexpected argument". Fix: `file: Vec<String>` with `num_args = 1..` and loop in `main`.
- [x] **#N2 — Archive (`.a`) inputs not handled.** `dev/nm.rs:113` uses `object::File::parse`, which only parses individual object files / executables. Spec INPUT FILES 108740-108742 explicitly includes "an object-file library whose format is the same as those produced by the ar utility." Today `nm libfoo.a` errors at `File::parse`. Fix: try `object::read::archive::ArchiveFile::parse` first; on success, iterate members and recurse `File::parse` on each, emitting the `"%s[%s]:\n"` (or `"%s[%s]: "` with `-A`) header per member per spec STDOUT 108797-108807.
- [x] **#N3 — `-P` portable output format not implemented.** `dev/nm.rs:53-54` declares the flag; `print_symbol` (`:69-101`) never consults `args.portable`. Spec STDOUT 108784-108792 mandates one of three exact formats depending on `-t d`/`-t o`/`-t x`: `"%s%s %s %d %d\n"` / `"%s%s %s %o %o\n"` / `"%s%s %s %x %x\n"` for `<library/object name>, <name>, <type>, <value>, <size>`. None of this is emitted. Fix: branch in `print_symbol` on `args.portable`, format value/size via `%d`/`%o`/`%x` per `args.out_type`, prepend `<library/object name>` per `-A` rules.
- [x] **#N4 — `-A` (print pathname per line) not implemented.** Same site. `args.print_name` is read at parse time and never used again. Spec OPTIONS 108719 + STDOUT 108767, 108794-108799. Fix: thread the current filename (and, for archives, the member name) into `print_symbol` and prepend `"%s: "` or `"%s[%s]: "` per spec.
- [x] **#N5 — Default symbol-name sort missing.** Spec STDOUT 108765-108766: "the output shall be sorted by symbol name according to the collation sequence in the current locale." `dev/nm.rs:123-128` iterates `file.symbols()` / `file.dynamic_symbols()` in object-file order and prints inline. Fix: collect into `Vec`, sort by `(name, value)` using `LC_COLLATE`-aware comparison (or libc `strcoll(3)` via FFI), then print.

#### Major

- [x] **#N6 — `-t format` not honored.** `dev/nm.rs:56-57` parses to `OutputType::{D,O,X}` with default `D`; `print_symbol` (`:98`) unconditionally prints `{:016x}`. Spec OPTIONS 108725-108730 and STDOUT 108784-108792. Fix: format the value column per the selected base; collapse `-o`/`-x` into `-t o`/`-t x` per spec 108724/108733.
- [x] **#N7 — `-o` and `-x` aliases not honored.** Same root cause as #N6; `args.octal` and `args.hex` are dead reads.
- [x] **#N8 — `-g` / `-u` / `-e` symbol-class filtering not implemented.** `dev/nm.rs:44-60` declares all three; the loop in `show_object_file` prints every symbol that isn't `SymbolKind::Section`/`SymbolKind::File`. Spec 108720, 108723, 108731. Fix: in `print_symbol`, early-return when `args.global && !symbol.is_global()`, when `args.undef && !symbol.is_undefined()`, or when `args.external_only && !(symbol.is_global() || matches!(symbol.scope(), SymbolScope::Linkage | SymbolScope::Compilation))`.
- [x] **#N9 — `-v` (sort by value) not implemented.** `dev/nm.rs:62-63` parses `value_sort` but it is never read. Spec 108732. Fix: switch the comparator built in #N5 to `(value, name)` when `args.value_sort`.
- [x] **#N10 — `-f` not implemented; section symbols are *unconditionally* suppressed.** `dev/nm.rs:70-72` early-returns for `SymbolKind::Section | SymbolKind::File`, equivalent to always-suppressing the `.text`/`.data`/`.bss` symbols regardless of `-f`. Spec 108720-108722: "−f Produce full output. Write redundant symbols (.text, .data, and .bss), normally suppressed by default." Fix: when `args.full`, emit those section symbols too (and keep `SymbolKind::File` suppressed since it isn't part of the "redundant symbols" set).
- [x] **#N11 — Per-file `"%s:\n"` / `"%s[%s]:\n"` headers missing.** Spec STDOUT 108800-108807: when `-A` is *not* specified and either there is more than one `file` operand or the single operand names a library, a header line shall be written before that file's symbols. Today no header is emitted in any case. Tied to #N1 and #N2.
- [x] **#N12 — Diagnostic messages go to stdout, not stderr.** ✓ closed by cross-cutting plib::diag wiring (2026-06-03): both `println!` sites now route through `plib::diag::error` → stderr. `dev/nm.rs:109, :116` use `println!("Failed to open file '{}': {}", ...)` / `println!("Failed to parse file '{}': {}", ...)`. Spec STDERR 108810: "The standard error shall be used only for diagnostic messages." Fix: `eprintln!`. Also prefix with `nm: ` per project convention.
- [x] **#N13 — Symbol size column missing from output.** Spec STDOUT 108781 ("The size associated with the symbol, if applicable") and the `-P` formats at 108787-108791 both require `<size>`. `object::Symbol::size()` is available. Fix: emit it; for the default (non-`-P`) format, append after the value when nonzero.
- [x] **#N14 — Runtime diagnostic strings hardcoded English.** `dev/nm.rs:109, :116` plus the eventual `eprintln!` rewrite. Spec ENVIRONMENT VARIABLES 108756-108758: `LC_MESSAGES` "Determine the locale that should be used to affect the format and contents of diagnostic messages written to standard error." `setlocale` is already called at `:135` and `gettext` is in scope — but the runtime strings aren't routed through `gettext()`. Fix: wrap each runtime diagnostic in `gettext(...)`.

#### Minor

- [x] **#N15 — `-g` and `-u` not enforced as mutually exclusive.** `dev/nm.rs:44-45, :59-60`. Spec SYNOPSIS uses `[-g|-u]`. Both can be set together today. Fix: `conflicts_with = "undef"` on `-g` (or vice versa). (Becomes meaningful only once #N8 lands.)
- [x] **#N16 — Non-POSIX `--long` aliases exposed in `--help`.** `dev/nm.rs:35-66` decorates each clap arg with both `short` and `long` (`--print-file-name`, `--external`, `--extern-only`, `--undefined-only`, `--format`, `--portability`). Spec SYNOPSIS has only short forms. Harmless functionally but advertises a non-POSIX surface. Fix: drop the `long = "..."` attributes, or hide via `hide_long_help`.
- [x] **#N17 — No newline-in-pathname diagnostic (Issue 8 Defect 251 future direction).** Spec FUTURE DIRECTIONS 108854-108858 encourages implementations to report an error if directed to display a pathname containing `<newline>` bytes. Pre-tracked here so the box appears once the operand+library plumbing (#N1/#N2/#N4) lands.

### Detailed conformance matrix

#### SYNOPSIS / argv parsing

- [x] Clap handles `--`, bundled short options, no `+` prefix surprises.
- [ ] **`file` operand variadic** (#N1 Critical) — declared `String` instead of `Vec<String>`.
- [ ] **`-g`/`-u` not enforced as mutually exclusive** (#N15 Minor).
- [ ] **Non-POSIX `--long` aliases** (#N16 Minor).

#### OPTIONS

| Spec opt | Status | Notes (file:line) |
|---|---|---|
| `-A` | MISSING | (#N4 Critical) Parsed at `:35-36`; never consulted. |
| `-e` (XSI) | MISSING | (#N8 Major) Parsed at `:38-39`; never consulted. |
| `-f` (XSI) | DIVERGES | (#N10 Major) Parsed at `:41-42`; section symbols *unconditionally* suppressed at `:70-72`. |
| `-g` | MISSING | (#N8 Major) Parsed at `:44-45`; never consulted. |
| `-o` (XSI) | MISSING | (#N7 Major) Parsed at `:47-48`; never consulted. |
| `-P` | MISSING | (#N3 Critical) Parsed at `:53-54`; never consulted. Output is always the unspecified default. |
| `-t format` | MISSING | (#N6 Major) Parsed at `:56-57`; value column always `{:016x}`. Default per spec when `-P` set should be `x`; impl default is `d` (which doesn't matter because the flag is unread). |
| `-u` | MISSING | (#N8 Major) Parsed at `:59-60`; never consulted. |
| `-v` | MISSING | (#N9 Major) Parsed at `:62-63`; never consulted; no sort done anywhere. |
| `-x` (XSI) | MISSING | (#N7 Major) Parsed at `:50-51`; never consulted. |

#### OPERANDS / STDIN / INPUT FILES

- [ ] **`file...` variadic** (#N1 Critical).
- [x] STDIN — "See INPUT FILES" — implementation does not read stdin; spec doesn't make it routable here. ✓
- [x] Object file / executable input via `object::File::parse` — `:113`.
- [ ] **Archive (`.a`) input not handled** (#N2 Critical).

#### ENVIRONMENT VARIABLES

| Var | Status | Notes |
|---|---|---|
| `LANG` | PARTIAL | `setlocale(LcAll, "")` at `:135` honors it for the libc-side, but no LANG-driven behavior reaches user output (only collation in #N5 would, once implemented). |
| `LC_ALL` | PARTIAL | Same. |
| `LC_COLLATE` | MISSING | (#N5 Critical) No sort is performed at all. |
| `LC_CTYPE` | PARTIAL | UTF-8 codeset bound at `:137` for gettext; symbol-name byte handling is via `object` crate (raw bytes). |
| `LC_MESSAGES` | PARTIAL | (#N14 Major) `setlocale` + `textdomain` set up; clap help is `gettext`'d but runtime diagnostics are not. |
| `NLSPATH` (XSI) | PARTIAL | Implicit via `textdomain("posixutils-rs")` + `bind_textdomain_codeset`; functional as soon as #N14 routes runtime strings through `gettext`. |

#### ASYNCHRONOUS EVENTS

- [x] Default — nm is non-interactive batch utility. `grep -nE 'SIGCONT|SIGWINCH|signal_hook|libc::signal' dev/nm.rs` → 0 matches.

#### STDOUT

| Spec format | Status | Notes |
|---|---|---|
| Default (unspecified, sorted by name) | PARTIAL | Unspecified format is fine per spec; default sort is missing (#N5 Critical). |
| `-P` default (`-t x` fallback) `"%s%s %s %x %x\n"` | MISSING | (#N3 Critical). |
| `-P -t d` `"%s%s %s %d %d\n"` | MISSING | (#N3, #N6). |
| `-P -t o` `"%s%s %s %o %o\n"` | MISSING | (#N3, #N6). |
| Per-file header `"%s:\n"` (multi-file, no `-A`) | MISSING | (#N11 Major). |
| Per-archive-member header `"%s[%s]:\n"` | MISSING | (#N2, #N11). |
| `-A` per-line prefix `"%s: "` / `"%s[%s]: "` | MISSING | (#N4 Critical). |
| Symbol type letters `A/a/B/b/D/d/T/t/U` | CONFORMS-ish | `:74-93` covers `U` and uppercase/lowercase via `is_global`; emits `C` for common, `r` for read-only data (spec-permitted impl extensions per 108770). Mapping is reasonable. |
| Value column | PARTIAL | (#N6 Major) Always `{:016x}`; ignores `-t`. |
| Size column | MISSING | (#N13 Major). |

#### STDERR

- [ ] **Diagnostics go to stdout via `println!`** (#N12 Major).

#### EXIT STATUS / CONSEQUENCES OF ERRORS

- [x] 0 on success — `main` returns `Ok(())` → exit 0.
- [x] Non-zero on error — `?` propagates `Box<dyn Error>` from `show_object_file`; Rust `Termination` exits non-zero.
- [x] CONSEQUENCES OF ERRORS = Default — no special policy required.

### Test coverage signal

There are **no nm tests**. `dev/tests/dev-tests.rs` has no `test_nm_*` entries and no fixtures. Every option, every output format, and every operand pattern is untested.

Once issues land, the test plan should include:
- [ ] `-P` output matches the three spec formats byte-for-byte against a fixture object file.
- [ ] Default sort by name (and a regression that proves it sorts).
- [ ] `-v` sort by value.
- [ ] `-g` filtering retains only globals; `-u` filtering retains only undefined; `-e` retains external + static.
- [ ] `-A` prefix on each line; `-A` with an archive emits `"%s[%s]: "`.
- [ ] Multi-file invocation emits `"%s:\n"` headers; single library emits `"%s[%s]:\n"` per member.
- [ ] Archive (`.a`) input parses and emits per-member output.
- [ ] Missing-file / unparseable-file diagnostics go to **stderr** and exit non-zero.
- [ ] `-f` enables emission of `.text`/`.data`/`.bss` section symbols.

### Suggested PR groupings

- **PR A — "Accept `file...`; route diagnostics to stderr"**: #N1, #N12. Smallest unit that makes the CLI usable; trivial.
- **PR B — "Implement `-P` portable format end-to-end"**: #N3, #N6, #N7, #N13. All four touch `print_symbol`'s output path; ship together with `-t`/`-o`/`-x` and the size column.
- **PR C — "Implement `-A` and per-file headers"**: #N4, #N11. Thread the current filename (and archive-member name once #N2 lands) into `print_symbol`.
- **PR D — "Archive (.a) input"**: #N2. Largest single change — branch on archive magic, iterate members.
- **PR E — "Default sort + `-v` sort"**: #N5, #N9. Collect-then-sort. Use `strcoll(3)` for `LC_COLLATE` honor.
- **PR F — "Symbol-class filtering"**: #N8, #N15. `-g`/`-u`/`-e` plus mutual-exclusion enforcement.
- **PR G — "`-f` and section-symbol gating"**: #N10. Replace the unconditional `SymbolKind::Section` early-return.
- **PR H — "i18n on runtime diagnostics"**: #N14, plus #N17 newline-in-pathname check.
- **PR I — "CLI hygiene"**: #N16. Drop `--long` aliases (or hide).

---

## `strings`

**Implementation:** `dev/strings.rs` (205 lines, single file)
**Tests:** `dev/tests/dev-tests.rs` strings_test group (~10 tests, fixtures in `dev/tests/strings/`)
**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 §3, pp. 3448–3450
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/strings.md`
**Date:** 2026-06-02

### TL;DR

A compact and largely working implementation: `-a`, `-t d/o/x`, and `-n` all parse and take effect, object files are auto-detected and their data sections scanned, and tests cover ASCII / UTF-8 / object-file / offset-format paths. Two real bugs land in the Critical bucket: (1) stdin is not used when no `file` operand is given — the empty `Vec<OsString>` simply iterates zero times — and (2) the printable-character predicate includes `\n` (and the rest of `is_whitespace()`), so the spec's "terminated by a `<newline>` or NUL character" rule is violated and multi-line runs are collapsed into one big "string" rather than split. The `-t` output format also adds GNU-style 7-column padding that the spec doesn't request.

### Priority issues

> **Resolved 2026-06-05.** #S1 (stdin with no operand), #S3 (bare POSIX
> `-t` format — offset fixtures regenerated), and #S5 (`-n` requires a
> positive integer) are closed (branch `dev-utils-posix-conformance`). #S4 is
> closed-by-refactor: the printable decision now flows through
> `plib::locale::isprint` (libc, honoring `LC_CTYPE`), so the old LC_* env
> substring heuristic is gone. #S2/#S6/#S7 were already closed. #S8 (the `-`
> operand) is a conforming note.

#### Critical

- [x] **#S1 — STDIN not read when no `file` operand is specified.** `dev/strings.rs:41-46` declares `input_files: Vec<OsString>` with no default; `main` (`:194-202`) just does `for file in args.input_files { … }`, so an argv-less invocation produces no output and no error. POSIX OPERANDS 115878-115881: "If no file operand is specified, the standard input shall be used." Fix: when `input_files.is_empty()`, read `io::stdin()` into a `Vec<u8>` and run `print_strings` on it via the same dispatch as `print_file`.
- [x] **#S2 — `<newline>` (and other whitespace control chars) are treated as printable and embedded into strings.** ✓ closed by switching to `plib::locale::isprint` (2026-06-03): libc `isprint('\n')` returns false, so `\n` now terminates strings as POSIX mandates. Both `read_printable_char_utf8` (`dev/strings.rs:103-107`) and `read_printable_ascii_char` (`:110-117`) accept any `char::is_whitespace()` byte. `is_whitespace()` returns true for `'\n'`, `'\r'`, `'\t'`, `'\x0B'`, `'\x0C'`, and a long set of Unicode separators. POSIX DESCRIPTION 115860-115861: "A printable string is any sequence of four (by default) or more printable characters terminated by a `<newline>` or NUL character." Today, a binary containing `b"abcd\nefgh\nijkl"` is reported as one string `"abcd\nefgh\nijkl"` rather than three. Spaces and horizontal tabs (`' '`, `'\t'`) typically *are* printable by `isprint(3)`, but `\n` and `\r` are not, and `\n` is explicitly the spec's terminator. Fix: terminate on `\n` and `\0` (and any other locale-defined non-printable control char); keep ASCII space and `\t` accepted; consider routing through libc `isprint_l(3)` for true `LC_CTYPE` honor. None of the existing fixtures contain embedded newlines, which is why this passes tests.

#### Major

- (none beyond the two Critical above — the remaining gaps are minor.)

#### Minor

- [x] **#S3 — `-t d`/`-t o`/`-t x` output uses 7-column width padding instead of the spec's plain `%d`/`%o`/`%x`.** `dev/strings.rs:122-133` uses `{:7}`, `{:7o}`, `{:7x}`. Spec STDOUT 115906-115911 mandates exactly `"%o %s"`, `"%x %s"`, `"%d %s"`. The source comment ("matches GNU strings") explicitly tags this as a non-POSIX deviation. Fix: drop the width specifier (`{}` / `{:o}` / `{:x}`). Likely breaks the three `with_*_offset.correct.txt` fixtures — regenerate them with `od`-equivalent baseline output.
- [x] **#S4 — Character-set detection is a string-match heuristic on `LC_ALL`/`LC_CTYPE`/`LANG` env vars instead of using the resolved locale.** `dev/strings.rs:55-82` (`CharacterSet::from_env`). `main` calls `setlocale(LcAll, "")` at `:186` but `from_env` ignores that and re-reads the env directly, looking for the literal substring `"UTF-8"`. Doesn't match `"en_US.utf8"` (lowercase, no dash) or `"C.UTF8"` (no dash). Spec ENVIRONMENT VARIABLES 115887-115898 and RATIONALE 115940-115941 ("strings as defined by the current locale") want a true locale-driven decision. Fix: call `nl_langinfo(CODESET)` via libc after `setlocale`, or compare in a normalized form.
- [x] **#S5 — `-n number` accepts zero (and any usize) but spec requires "positive integer."** `dev/strings.rs:34-35` declares `minimum_string_length: usize` with `default_value_t = 4` and no range check. `strings -n 0 file` is currently a no-op-rich path that prints every empty position. Spec OPTIONS 115870-115871. Fix: `value_parser = clap::value_parser!(usize).range(1..)`.
- [x] **#S6 — `isprint(3)`-equivalent decision is not locale-aware.** ✓ closed by `plib::locale::isprint` (2026-06-03), which routes ASCII through libc `isprint` and non-ASCII through libc `iswprint`. `dev/strings.rs:103, :112`. Uses Rust's `char::is_control`/`is_ascii_graphic`/`is_whitespace` which apply Unicode property tables, not the `LC_CTYPE` locale. POSIX RATIONALE 115940-115941 explicitly calls this out: "the ISO C standard function isprint() is restricted to a domain of unsigned char. This volume of POSIX.1-2024 requires implementations to write strings as defined by the current locale." Fix: call `isprint_l(3)` (or `iswprint_l(3)` per code point) via libc after `setlocale`. Becomes more important once #S2 is fixed.
- [x] **#S7 — First failing file aborts the run; runtime diagnostics not gettext'd.** ✓ partially closed (2026-06-03): `process_files` now logs per-file errors via `plib::diag::error` and continues; `main` exits non-zero via `plib::diag::exit_status()`. String-level `gettext()` translation deferred. `dev/strings.rs:160-183, 194-202`. `print_file` returns `Err(...)` on `fs::read` failure, which propagates to `main` via `?` and aborts the loop, so subsequent file operands are silently dropped. Most historical `strings` implementations log a diagnostic on stderr and continue. Diagnostic strings come from Rust's default error display, with no `gettext` routing despite `setlocale` + `textdomain` being wired. Fix: catch the error, write `"strings: <path>: <error>\n"` via `gettext`-routed `eprintln!`, continue with the next file, and set a "saw error" flag for the exit code.
- [ ] **#S8 — `-` argument is opened as a literal pathname rather than treated specially.** Spec DESCRIPTION 115863: "If any argument is '−', the results are unspecified." Issue 8 Defect 1599 widened "first argument" to "any argument," explicitly leaving the behavior up to the implementation, so today's "try to open file `-`" is conforming — just worth noting. Fix (optional): document the choice, or treat `-` as stdin to match user expectation.

### Detailed conformance matrix

#### SYNOPSIS / argv parsing

- [x] Options `[-a] [-t format] [-n number]` accepted — `dev/strings.rs:27-36`.
- [x] Variadic `file...` operand — `:45`.
- [x] `--` end-of-options and bundled short options handled by clap.
- [x] `-` is unspecified per spec; impl treats as filename (acceptable). (#S8 noted.)
- [ ] **`-n 0` accepted; spec requires positive** (#S5 Minor).

#### OPTIONS

| Spec opt | Status | Notes (file:line) |
|---|---|---|
| `-a` | CONFORMS | `:28-29` parsed; `print_file:166-169` scans the whole file when set. |
| `-n number` | PARTIAL | `:34-35` honored. (#S5 Minor) accepts 0. |
| `-t format` | PARTIAL | `:31-32` honored via `OffsetFormat::{D,O,X}`. (#S3 Minor) extra width padding deviates from spec format. |

#### OPERANDS / STDIN / INPUT FILES

- [x] `file...` operands accepted variadic — `:45`.
- [ ] **No-file → stdin** (#S1 Critical).
- [x] `fs::read` accepts any regular file — `:164`.

#### ENVIRONMENT VARIABLES

| Var | Status | Notes |
|---|---|---|
| `LANG` | PARTIAL | (#S4 Minor) Re-read raw at `:74`; substring `"UTF-8"` match only. `setlocale` separately honors it at `:186` but only for libc/gettext. |
| `LC_ALL` | PARTIAL | (#S4 Minor) Same. |
| `LC_CTYPE` | PARTIAL | (#S4/#S6 Minor) Substring-matched at `:72`; not used for the actual `isprint` decision. |
| `LC_MESSAGES` | PARTIAL | `setlocale` + `textdomain` set up at `:186-188` so clap help is `gettext`'d; runtime diagnostics aren't (#S7). |
| `NLSPATH` (XSI) | PARTIAL | Implicit via `textdomain`/`bind_textdomain_codeset`; functional once runtime strings route through `gettext`. |

#### ASYNCHRONOUS EVENTS

- [x] Default — strings is non-interactive batch utility. `grep -nE 'SIGCONT|SIGWINCH|signal_hook|libc::signal' dev/strings.rs` → 0 matches.

#### STDOUT

| Spec format | Status | Notes |
|---|---|---|
| Default `"%s"` | CONFORMS | `:132`. |
| `-t d` `"%d %s"` | DIVERGES | (#S3 Minor) `:123-125` emits `"{:7} {}"` — width-padded. |
| `-t o` `"%o %s"` | DIVERGES | (#S3 Minor) `:126-128` emits `"{:7o} {}"`. |
| `-t x` `"%x %s"` | DIVERGES | (#S3 Minor) `:129-131` emits `"{:7x} {}"`. |
| Strings terminated by `\n` or NUL | DIVERGES | (#S2 Critical) `\n` is embedded into strings rather than terminating them. |
| One string per line | CONFORMS | `println!` per matched run — once #S2 lands. |

#### STDERR

- [x] Standard error used only for diagnostics — `?` propagates via `Box<dyn Error>` and Rust runtime prints to stderr.
- [ ] **Diagnostic format implementation-defined; not gettext'd; aborts on first error** (#S7 Minor).

#### EXIT STATUS / CONSEQUENCES OF ERRORS

- [x] 0 on success — `main` returns `Ok(())` → exit 0.
- [x] Non-zero on error — `?` propagation; Rust `Termination` exits non-zero.
- [x] CONSEQUENCES OF ERRORS = Default — no special policy required.

### Test coverage signal

Existing tests (per `dev/tests/dev-tests.rs:526-`):
- `test_strings_empty_file` — empty input.
- `test_strings_print_one` / `_multiple` — basic ASCII fixtures.
- `test_strings_utf8_file` — UTF-8 fixture.
- `test_strings_object_file` — object-file `.text`/`.data` scanning.
- `test_strings_print_shorter_than_default_length` (`-n 2`) and `_longer_than_default_length` (`-n 7`).
- `test_strings_print_with_decimal_offset` / `_hex_offset` / `_octal_offset` — `-t d`/`-t x`/`-t o`.

Gaps that map to findings:
- [ ] **No test for no-operand stdin path** (#S1 Critical).
- [ ] **No test with embedded `\n` proving it terminates** (#S2 Critical).
- [ ] **No test pinning the bare `%d`/`%o`/`%x` POSIX format** (#S3 Minor) — current fixtures bake in the 7-column padding.
- [ ] **No locale-driven `LC_CTYPE` test** (#S4, #S6 Minor).
- [ ] **No `-n 0` rejection test** (#S5 Minor).
- [ ] **No multi-file invocation where one path is missing** (#S7 Minor).

### Suggested PR groupings

- **PR A — "Read stdin when no file operand"**: #S1. Smallest critical fix; add the stdin branch + a test.
- **PR B — "`<newline>` terminates strings"**: #S2. Tighten the printable predicate; add a fixture with embedded newlines; #S6 (locale-aware `isprint`) is a natural extension and can either ride or be its own follow-up.
- **PR C — "POSIX `-t` output format"**: #S3. Drop the width specifier; regenerate the three offset fixtures.
- **PR D — "Locale-driven codeset detection + isprint"**: #S4, #S6. Replace the env-var heuristic with `nl_langinfo(CODESET)`; route `isprint` through libc.
- **PR E — "`-n` positive-integer validation"**: #S5. One-line clap fix + test.
- **PR F — "Multi-file error continuation + gettext diagnostics"**: #S7. Loop continuation + `eprintln!(gettext(...))`.
- **PR G — "Document `-` operand choice"**: #S8. Optional; either treat `-` as stdin to match user expectation or add a one-line note to `--help`.

---

## `strip`

**Implementation:** `dev/strip.rs` (155 lines, single file)
**Tests:** `dev/tests/dev-tests.rs` strip group (5 tests, fixtures in `dev/tests/strip/`)
**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 §3, pp. 3451–3452
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/strip.md`
**Date:** 2026-06-02

### TL;DR

A small, ELF-only implementation that handles the headline case (delete debug sections, symbol table, and relocations from a relocatable / executable ELF) and the XSI archive case (iterate `.a` members and re-pack stripped versions). The audit surfaces two Critical issues — non-ELF archive members are silently *dropped* from the output (data loss), and per-file errors are logged but never propagate into a non-zero exit status — plus four Major issues around strip aggressiveness on `.o` files (relocations gone → unlinkable), non-ELF/non-archive formats unsupported on macOS, non-atomic in-place rewrite, and stale archive symbol-table after stripping. Locale handling is set up (`setlocale` + `textdomain` + `bind_textdomain_codeset`) but no runtime diagnostic string is routed through `gettext`.

### Priority issues

> **Resolved 2026-06-05.** #ST1 (preserve non-ELF archive members), #ST4
> (keep relocations + symbol table on `ET_REL` objects so they stay
> linkable), #ST7 (require ≥ 1 operand), #ST3 (clear rejection of unsupported
> formats — real **Mach-O stripping is DEFERRED**, documented), and #ST8
> (gettext diagnostics) are closed (branch `dev-utils-posix-conformance`).
> #ST2/#ST5/#ST6 were already closed. #ST9 is a harmless plumbing note.

#### Critical

- [x] **#ST1 — Non-ELF members are silently dropped from rewritten archives (data loss).** `dev/strip.rs:75-99` (`strip_archive`). The loop appends to a fresh `ar::Builder` *only* when `is_elf(&data)` is true; the `else` branch emits a warning and never calls `stripped_archive.append`. The output archive is then written back to disk via `strip_file` (`:130`), permanently destroying any non-ELF member (e.g., the System V `/` symbol-table member, a `//` long-name member, or any non-object that ar legitimately allows per POSIX RATIONALE 84538-84542). Fix: in the `else` branch, append the unmodified member to `stripped_archive`; reserve hard-skipping for ar's own special members and document the policy.
- [x] **#ST2 — Per-file errors are logged but the process always exits 0.** ✓ closed by cross-cutting plib::diag wiring (2026-06-03): every error path in `strip_file` records via `plib::diag::error`; `main` calls `process::exit(diag::exit_status())`. `dev/strip.rs:109-142` (`strip_file`) prints `strip: …` diagnostics via `eprintln!` for read errors (`:113`), unrecognized format (`:122`), archive errors (`:139`), and write errors (`:131`) — then returns `()`. `main` (`:144-155`) iterates files and unconditionally returns `Ok(())`. POSIX EXIT STATUS 116010-116013: "0 Successful completion. >0 An error occurred." CONSEQUENCES OF ERRORS = Default, which per XCU 1.4 mandates a non-zero exit status on any error. Today `strip nonexistent.o realfile.o; echo $?` prints `0` even though one file failed. Fix: return `Result<(), …>` (or an error count) from `strip_file`; in `main`, after the loop, exit non-zero when any file failed.

#### Major

- [x] **#ST3 — Only ELF and ar archives recognized; Mach-O / COFF / PE / XCOFF rejected.** `dev/strip.rs:101-107, 117-127` (`is_elf`, `is_archive`, `strip_file` dispatch). Spec INPUT FILES 115983-115985: "files shall be in the form of strippable files successfully produced by any compiler defined by this volume of POSIX.1-2024." On macOS — a project-supported platform per `CLAUDE.md` — that's Mach-O, which today triggers `"strip: <path>: file format not recognized"` and is left untouched. Fix: detect Mach-O magic (`MH_MAGIC`/`MH_CIGAM`/`MH_MAGIC_64`/`MH_CIGAM_64`/`FAT_MAGIC`) and dispatch to a Mach-O strip path (the `object` crate has Mach-O read support; a write path may need a separate builder). At minimum, exit non-zero and document the supported-format list.
- [x] **#ST4 — Relocation sections (SHT_REL / SHT_RELA) unconditionally stripped from relocatable `.o` files, breaking subsequent linking.** `dev/strip.rs:42-45` (`strip_section`). Spec DESCRIPTION 115972: "The effect of strip on object and executable files shall be similar to the use of the −s option to c17." `c17 -s` strips a final executable's symbol table but does *not* render an object file unlinkable; historical strip distinguishes the two by the ELF type (`ET_REL` vs `ET_EXEC`/`ET_DYN`). Today the same aggressive removal hits both, and `test_strip_remove_all_relocations` (`dev/tests/dev-tests.rs:491`) actually pins the regression in. Fix: skip relocation/symtab stripping when `elf::Header::e_type == ET_REL`; restrict aggressive section deletion to executables and shared libraries.
- [x] **#ST5 — Archive symbol table is not regenerated after member stripping; the output archive is stale for the link editor.** ✓ closed by cross-cutting plib::archive helper (2026-06-03): `strip_archive` now rebuilds the `"/"` symbol-table member via `plib::archive::write_sysv_symbol_table` after stripping all members, using offsets computed from the freshly-laid-out archive. `dev/strip.rs:75-99`. POSIX spec 115973-115974: "The effect of strip on an archive of object files shall be similar to the use of the −s option to c17 for each object file in the archive." The companion `ar` utility (per `dev/ar.rs`) generates a `/` symbol-table member when it writes an archive, and POSIX 84371-84376 says the link editor uses it for random access. After stripping members, the offsets and the symbol set itself have changed, so any preserved table is wrong; the `ar::Builder` used here does not synthesize a fresh one. Fix: after appending stripped members, re-derive symbol → member-offset mapping (the same logic in `dev/ar.rs:294-342`) and write a new symbol-table member at the head of the output.
- [x] **#ST6 — In-place file rewrite is non-atomic; a crash mid-write leaves the binary truncated/corrupted.** ✓ closed by cross-cutting plib::io::write_atomic (2026-06-03): `strip_file` now writes via tempfile + `rename(2)`. `dev/strip.rs:130` (`std::fs::write(file, stripped_contents)`) opens with `O_WRONLY|O_CREAT|O_TRUNC`, truncating the original before any new bytes are written. POSIX doesn't strictly mandate atomicity, but the file being rewritten is typically the system's `/usr/bin/foo` or a build artifact — a partial write is destructive. Fix: write to `file.with_extension(".strip.tmp")` in the same directory (so `rename(2)` is atomic), `fsync` the temp, then `rename` it over the original. (Existing file mode is preserved by the current `fs::write` against an existing path because `O_CREAT` ignores the mode arg when the file exists, so executable bits survive — no separate fix needed there, but the temp-file path must explicitly carry the original's mode forward.)

#### Minor

- [x] **#ST7 — Empty operand list silently succeeds; spec mandates `file...` (≥ 1).** `dev/strip.rs:25` declares `input_files: Vec<OsString>` with no `num_args = 1..` constraint, so `strip` with no arguments exits 0 with no work done. POSIX SYNOPSIS 115966 makes the operand required (the variadic `...` allows ≥ 1, not ≥ 0). Fix: `#[arg(num_args = 1.., required = true)]`.
- [x] **#ST8 — Runtime diagnostic strings hardcoded English; `gettext` set up but unused at runtime.** `dev/strip.rs:91, 113, 122, 131, 139`. `setlocale`/`textdomain`/`bind_textdomain_codeset` are called at `:145-147` so the framework is in place — only the clap help text is decorated (`:23`). Spec ENVIRONMENT VARIABLES 115996-115998 + NLSPATH 115999. Fix: wrap each runtime string in `gettext(...)`.
- [ ] **#ST9 — `LC_*` chain not actively consulted beyond `setlocale`.** Strip's spec-listed env vars (`LANG`/`LC_ALL`/`LC_CTYPE`/`LC_MESSAGES`/`NLSPATH`) are all routed through libc via `setlocale(LC_ALL, "")`, which is correct for any libc-mediated decision — but the implementation makes no such decisions today (no `isprint`, no collation, no message catalog use). The plumbing is harmless and forward-compatible with #ST8.

### Detailed conformance matrix

#### SYNOPSIS / argv parsing

- [x] Variadic `file...` accepted — `dev/strip.rs:25`.
- [x] `--` end-of-options handled by clap.
- [x] No options to consider (spec OPTIONS 115975-115976: "None.").
- [ ] **Empty operand list not rejected** (#ST7 Minor).

#### OPTIONS

- N/A — spec explicitly defines no options.

#### OPERANDS / STDIN / INPUT FILES

- [x] `file` operand routed to `strip_file` — `:151-153`.
- [x] STDIN "Not used" — `grep -n 'stdin' dev/strip.rs` → 0 matches.
- [x] ELF object / executable accepted — `:117-118`.
- [x] ELF archive accepted (XSI) — `:119-120`.
- [ ] **Other strippable formats (Mach-O, COFF, PE, XCOFF) rejected** (#ST3 Major).

#### ENVIRONMENT VARIABLES

| Var | Status | Notes |
|---|---|---|
| `LANG` | PARTIAL | `setlocale(LcAll, "")` at `:145` honors via libc; no in-process use yet. |
| `LC_ALL` | PARTIAL | Same. |
| `LC_CTYPE` | PARTIAL | Same; no byte-vs-char decisions made. |
| `LC_MESSAGES` | PARTIAL | (#ST8 Minor) `textdomain`/`bind_textdomain_codeset` set up at `:146-147`; runtime diagnostic strings not gettext'd. |
| `NLSPATH` (XSI) | PARTIAL | Implicit via `textdomain`; effective once #ST8 lands. |

#### ASYNCHRONOUS EVENTS

- [x] Default — strip is non-interactive batch utility. `grep -nE 'SIGCONT|SIGWINCH|signal_hook|libc::signal' dev/strip.rs` → 0 matches.

#### STDOUT / STDERR

- [x] STDOUT "Not used" — no `println!`/`stdout()` in the source.
- [x] Diagnostics routed to stderr via `eprintln!` — `:91, 113, 122, 131, 139`.
- [ ] **Diagnostic strings not gettext-routed** (#ST8 Minor).

#### OUTPUT FILES

- [x] Output is "strippable files of unspecified format" — ELF members preserved as ELF, archive members re-packed as ar.
- [ ] **Non-ELF archive members silently dropped from output** (#ST1 Critical).
- [ ] **Archive symbol table not regenerated** (#ST5 Major).
- [ ] **Non-atomic in-place rewrite** (#ST6 Major).

#### What is stripped (per `strip_section`, `dev/strip.rs:39-54`)

| Spec / convention | Status | Notes |
|---|---|---|
| Symbol table (`.symtab` + symbol-name `STRING` section) | CONFORMS-ish | `:49-53` — appropriate for executables; (#ST4 Major) too aggressive on `.o`. |
| Debug sections (`.debug*`, `.zdebug*`, `.gnu.linkonce.wi.*`, `.gnu.debuglto_.debug_*`, `.line`, `.stab*`, `.gdb_index`) | CONFORMS | `:28-37` mirrors GNU binutils list. |
| Group sections (`SHT_GROUP`) | PARTIAL | (#ST4) over-aggressive on relocatables. |
| Relocations (`SHT_REL`, `SHT_RELA`) | PARTIAL | (#ST4 Major) breaks linking when applied to `.o`. |
| Dynamic linking sections (`.dynsym`, `.dynstr`, `.gnu.hash`, etc.) | PRESERVED | Not in `strip_section` — correct for runtime correctness. |

#### EXIT STATUS / CONSEQUENCES OF ERRORS

- [x] 0 on success — `main` returns `Ok(())`.
- [ ] **Non-zero on error not honored** (#ST2 Critical) — `strip_file` swallows errors after logging them; `main` always succeeds.

### Test coverage signal

Existing tests (`dev/tests/dev-tests.rs:454-`):
- `test_strip_stripped_elf_is_valid_elf` — output of stripping an ELF is itself a valid ELF.
- `test_strip_stripped_archive_contains_valid_elf_members` — archive members survive stripping.
- `test_strip_remove_all_non_section_symbols` — symbol table is emptied.
- `test_strip_remove_all_relocations` — *locks in* the over-aggressive relocation removal (#ST4).
- `test_strip_removes_all_debug_sections` — debug sections gone.

Gaps that map to findings:
- [ ] **No test for non-ELF archive members being preserved** (#ST1) — a fixture with a mixed-content archive would have caught the data loss.
- [ ] **No test for non-zero exit on read/format error** (#ST2). `strip /nonexistent` should exit non-zero.
- [ ] **No test that a stripped `.o` is still linkable** (#ST4) — would fail today.
- [ ] **No test that the archive symbol table after stripping points to correct offsets** (#ST5).
- [ ] **No test for atomic-rewrite / crash-safety** (#ST6) — hard to write but the temp-file approach can be sanity-checked via behavior under a forced error.
- [ ] **No test for empty operand list** (#ST7).
- [ ] **No Mach-O / non-ELF test** (#ST3) — would require macOS CI.

### Suggested PR groupings

- **PR A — "Preserve non-ELF archive members + non-zero exit on error"**: #ST1, #ST2. The two Critical bugs share `strip_file` / `strip_archive` and need joint test fixtures.
- **PR B — "Don't strip relocations from `ET_REL`"**: #ST4. Gate aggressive stripping on ELF type; replace `test_strip_remove_all_relocations` with one that targets `ET_EXEC` only and adds a `.o` linkability check.
- **PR C — "Regenerate archive symbol table after stripping"**: #ST5. Largely reuses `dev/ar.rs:294-342` — consider extracting a shared helper between `ar` and `strip`.
- **PR D — "Atomic in-place rewrite via temp + rename"**: #ST6. Wire the temp-file path; carry forward the original mode/uid/gid via `fchmod(2)`/`fchown(2)` on the temp before rename.
- **PR E — "Mach-O support (or honest rejection)"**: #ST3. macOS-conditional; alternatively just exit non-zero with a clearer "format not yet supported on this platform" message.
- **PR F — "CLI / i18n hygiene"**: #ST7, #ST8, #ST9. Require ≥ 1 operand, route runtime diagnostics through `gettext`.
