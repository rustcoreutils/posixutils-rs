# POSIX.1-2024 Conformance Audit — `man`

**Implementation:** `man/` crate (~14.2 kloc): `man.rs` (1,171), `man_util/formatter.rs` (7,573, mdoc AST→terminal renderer), `man_util/roff/` (1,633, the roff request/escape front-end + expression evaluator), `man_util/parse/` (1,577, the hand-written mdoc/man parsers), `man_util/man7.rs` (640, the man(7) renderer), `man_util/term/` (462, the shared terminal backend), `man_util/mdoc_macro/` (411), `man_util/preproc/` (372, tbl/eqn), `man_util/parser.rs` (289, a facade over `parse/`), `man_util/config.rs` (78).
> The figures above replace the original header, which described an engine that no longer exists: `parser.rs` was 12,054 lines of pest-driven parsing and the grammar lived in `man_util/mdoc.pest`. Both are gone (see the engine-rewrite note below); `parser.rs` is now a 289-line facade with no `.unwrap()` at all.

**Tests:** 304 across the crate — 146 formatter snapshot tests, the `man_util` unit suites (roff, man7, tbl, eqn, term, parse), and `man/tests/man/` (integration, including a 20-page malformed-input corpus). The original header's "599 snapshot tests" is not supported by anything in the tree.
**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 §3 `man`, pp. 3163–3167 (User Portability Utilities option, marked `UP`).
**Reference:** No sliced spec tree was available; the spec was read from the mega-PDF `~/tmp/POSIX.2024.pdf` (internal pages 106802–106948). Mirrors the `m4`/`make` audits' PDF-based method. Engine-fitness comparisons are against OpenBSD/mandoc rendering of `mdoc(7)`/`man(7)`/`roff(7)`.
**Date:** 2026-06-12
**Scope (per maintainer):** POSIX contract **plus** a full fitness review of the mdoc/roff engine (the maintainer notes the groff-style engine was implemented *implicitly* to satisfy POSIX `man`), behaviorally verified.
**Verification:** Critical and most Major findings were **behaviorally verified** against the built `target/release/man` binary using `man -c -l <file>` (format a local page to stdout, bypassing the pager) on crafted mdoc/man(7) inputs. Evidence is inline. Several agent-proposed findings were **refuted** by behavioral testing and are recorded at the bottom rather than silently dropped. No code was modified.

## TL;DR

> **Status (2026-08-03): every finding in this document is closed.** The 2026-06-12 pass remediated #1–#8 and #10–#16 across five commits. The one item it left open, **#9**, is closed by the 2026-08-03 pass, which also closed the two halves of **#14** (`gettext` diagnostics, and `NLSPATH` — fixed in `gettext-rs` for the whole workspace rather than in `man` alone) and the wide/CJK width row. That pass re-audited the rebuilt engine, which no earlier pass had examined, and recorded fourteen new findings **#M1–#M14** — three Critical, one a file-disclosure vulnerability — in their own section below. Remaining deliberate gaps: full `LC_CTYPE`-charset decoding, and tbl spanning/box drawing.

> **Engine rewrite (2026-06-12, follow-on):** the engine was subsequently rebuilt toward the mandoc architecture — a single terminal backend (`man_util/term/`) driven by both renderers; a full **roff front-end** (`man_util/roff/`) closing the "out of scope" gaps above: number/string registers, `.if`/`.ie`/`.el`/`.while` conditionals and loops, `.de`/`.am` user macros, `.so` includes, `.ig`, diversions, an expression evaluator, and traps/environments as terminal no-ops; a **hand-written recursive-descent parser** (`man_util/parse/mdoc.rs`) that **replaced pest entirely** (grammar, deps, and the exponential-backtracking nesting guard from #2 are gone — deep nesting is now handled in linear time without a cap); and **tbl/eqn preprocessors** (`man_util/preproc/`). The parser is total (the #1/#2 crash classes are structurally impossible). The hand-written parser is validated end-to-end by the 145 byte-identical formatter snapshot tests plus the integration suite.

Against the (deliberately minimal) POSIX `man` contract — one option `-k`, a handful of env vars, STDIN-not-used, exit 0/>0, implementation-defined output — the tool largely **conforms**: `-k` exists, STDIN is unused, exit status is 0/>0, `PAGER` is honored, `setlocale` is called, and every `.Sh` section (SYNOPSIS/OPTIONS/ENVIRONMENT/EXIT STATUS) of an mdoc page reaches output. The risks are concentrated in the *engine* the maintainer built to render pages. **A malformed page can crash the process** (`.Xr name` with a missing section number panics; deeply nested macros overflow the stack), and — most importantly for the golden path — **the parser only understands `mdoc(7)`; a legacy `man(7)`/roff page (`.TH`/`.SH`/`.B`/`.TP`, which is what most Linux pages are) renders as an empty page with header and footer only, and still exits 0.** Beyond that: no bold/italic/underline is ever emitted (all SGR is commented out), inline `\fB…\fR` roff font escapes pass through as literal text, the pager is invoked even when stdout is not a terminal, width is capped at 78 and never honors a wider terminal, and `-k`'s native fallback does literal-substring matching rather than the spec's `grep -Ei` (ERE).

## Findings from the 2026-08-03 closeout pass (`#M1`–`#M14`)

The engine described above was rebuilt after the original audit, and the rebuilt
subsystems — the roff front-end, the man(7) renderer, and the tbl/eqn
preprocessors — had never been audited. Re-auditing them, and sweeping the 1,777
pages in `/usr/share/man/man1` through the built binary, found thirteen defects
that no finding list contained (`#M14` was found on 2026-08-04, following it). All were reproduced against the binary before
being fixed, and every fix was confirmed to fail against the pre-fix code.

The sweep is the honest headline. Before this pass, of 1,777 real pages:
**195 leaked raw roff source** into the terminal with exit status 0, **539
rendered a line wider than the terminal**, and one (`sccs(1)`) panicked. After:
**4**, **352**, and none. What remains is dominated by tbl `T{`…`T}` text
blocks, which `preproc/tbl.rs` documents as unmodeled — a real gap, recorded
here rather than silently closed.

| # | Severity | Summary |
|---|---|---|
| **#M1** | **Critical** | `.ds A \*[A]` plus `\*A` — two lines — overflowed the stack and aborted the process (SIGABRT, exit 134). `Roff::interpolate` recursed into every string-register expansion with no limit, despite its own comment claiming it resolved them "once". Also reachable via a mutual `.ds` pair and via `\w'…'`. This runs *before* language detection, so every entry path was affected. Fixed with an in-progress name set (which is what breaks a cycle) plus a depth cap (for the acyclic-but-deep case). |
| **#M2** | **Critical** | `is_man7` looked at the **first macro only**, so any roff preamble (`.nh`, `.pc`, `.ig`, `.ds`, `.nr`, or leading text) sent a man(7) page to the mdoc parser, which knows none of its macros and printed the page's source with exit status 0. This is finding **#3's original symptom, still live** despite #3 being ticked fixed. Detection now runs to whichever of `.TH`/`.SH`/`.SS` or `.Dd`/`.Dt`/`.Os`/`.Sh` appears first, as mandoc does. |
| **#M3** | **Critical** | `.TP`/`.IP` set the body indent from the *already-shifted* margin and `.PP` never reset it, so every entry in a GNU OPTIONS section sat one step further right than the last: `ls(1)` rendered a **396-column line** into a 78-column terminal. A second ratchet came from `.RE` restoring the paragraph base from the terminal margin, which inside a `.TP` body is the tag's body indent — `screen(1)` uses that shape ~200 times and reached **880 columns**. |
| **#M4** | **High (security)** | `.so /etc/passwd` read the password file and rendered it to the terminal with exit status 0: `load_so` passed the target straight to `PathBuf::from` before trying the man roots. A page is untrusted input — from a package, a shared `MANPATH`, or `man -l` on a download — and `.so` is not a general file-read primitive. Now restricted to relative targets with no `..` component, as mandoc requires. |
| **#M5** | High | A page that sources itself, or a pair that source each other, never terminated in practice: inclusion is flattened onto the line queue, so no depth limit applied and it ran until the two-million-line budget tripped — **224 seconds and 8 MB of output** from two short files. Every include also costs a `fork`+`exec` of `cat`/`zcat`. |
| **#M6** | High | A `.ds` doubling chain (the billion-laughs shape) produced **400 MB of output and 2 GB resident**, exit 0, from about thirty input lines. It is acyclic and only thirty deep, so neither guard above sees it, and it all lands on a **single line**, which a line budget cannot see. Fixed with a byte budget, and with an expansion allowance inside `interpolate` itself — the chain reaches full size before anything is emitted. |
| **#M7** | High | `roff/expr.rs`'s `term()` and `expr()` recurse per nesting level with no cap: `.if ((((…1` overflowed the stack. Its arithmetic also panicked outright under overflow checks (`9223372036854775807+1`); it now saturates, and division uses `checked_div`/`checked_rem` to cover `i64::MIN / -1`. |
| **#M8** | High | The `.while` line budget was **per-iteration**, so the 100,000-iteration cap multiplied a two-million-line allowance. Both counters now live on the interpreter, so macro expansion, `.while` bodies and `.so` inclusion share one allowance. |
| #M9 | Major | Seven input-reachable `unreachable!()` arms and two arithmetic underflows in the renderer. Folded into **#9** above, which is where the finding belongs. |
| #M10 | Medium | A conditional's numeric expression ended at the first whitespace *anywhere*, so `.if (\n(rF:(\n(.g==0)) \{\` — the standard groff idiom opening every pod2man page — evaluated `(0`, which is truthy, and printed the rest as body text. |
| #M11 | Medium | Macro bodies were stored without roff **copy mode**, so `\\` never reduced to `\`; `substitute_args` then emitted the leading backslash of `\\$1` before every expanded argument. Doubling escapes is the normal way to write a macro body, so this affected essentially every `.de`. `\n(.$` (the argument count) was also unsupported. |
| #M12 | Medium | `\,` and `\/` (italic corrections) leaked as literal text. help2man emits them on every GNU page. |
| **#M14** | Major | `T{`…`T}` text blocks were emitted as literal cell content, markers and all, on one unwrapped line — 80 real pages showed a bare `T{`. With it: per-row format sections concatenated into one, `.T&` treated as data, uncounted `s`/`^` span columns, and `tab (@)` with a space unrecognized. See the section below. |
| #M13 | Low | A `.TS` region with no format line silently discarded its entire content (the scan for the terminating `.` consumed the region). tbl rule rows were O(input²) — 2,000 rule rows beside one 200 KB cell cost 400 MB. Emitted lines kept trailing padding, which after `\&` resolution left 380-column runs of spaces. A trailing `\"` comment reached request arguments, so `.de CQ \" put $1 in typewriter font` took `\"` as the definition's custom end macro and swallowed the rest of the page — a pod2man idiom that blanked every perl-derived page. Tags and `.B` arguments were emitted unfilled, where groff fills them (`nvidia-smi(1)`: a 490-column `.TP` tag). |

### Corrections to this document made in the same pass

- The header described `parser.rs` as 12,054 lines of pest-driven parsing with a
  `man_util/mdoc.pest` grammar, and the crate as 21.4 kloc. All three were stale;
  the figures at the top are now measured. "599 snapshot tests" was not supported
  by anything in the tree (the real number is 146 in `formatter.rs`, 304
  crate-wide).
- `parse/mod.rs` and `man_util/mod.rs` both claimed the hand-written parser sat
  behind a `MAN_PARSER=v2` switch with "pest remains the default". No such
  variable is read anywhere and pest is gone; both comments are corrected.
- `formatter.rs` carried a 139-line `/* */`-commented `mod test_mdoc` that could
  never compile, which is what made the `rstest` dev-dependency unused. Both are
  removed.

### tbl text blocks (`#M14`, closed 2026-08-04)

`T{`…`T}` marks a cell holding prose to be filled to the column's width. It was
passed through as literal cell content, markers and all, on one unwrapped line:
**80 of the 1,777 pages printed a bare `T{` to the terminal**, man(1)'s own page
among them. Now none do.

A block cannot be measured the way an ordinary cell is, because its extent is a
*consequence* of the width rather than an input to it. groff diverts the block,
lets the formatter fill it, then reads the measured width and height back from
`\n[dl]`/`\n[dn]`; the width it fills to is `w(N)` if given, else
`L*C/(N+1)` — emitted literally as `\n[.l]*C/(N+1)`. The same rule is used
here, verified against groff at three column counts. Filling is left-aligned
rather than adjusted to both margins, matching the rest of this renderer.

Testing it against real pages surfaced four more defects, all fixed with it: a
format section is a list of **per-row** formats (concatenating systemctl(1)'s
fifteen lines made a 45-column table, so blocks filled to `L/46` — one word per
line); `.T&` was treated as data, printing `l l l` as a table row; `s`/`^` span
markers were not counted as columns; and `tab (@);` with a space was not
recognized, which is why man(1) still leaked markers.

### Known gaps, recorded rather than closed

- tbl **spanning and box drawing** are not modeled. A table of wide simple cells
  can still exceed the terminal — but so can groff's: systemctl(1) renders at
  157 columns under groff against our 156, so this is the page's own doing.
- `.nf` regions are not wrapped, which matches groff: a literal line wider than
  the terminal is likewise the page's own doing (240 of the 346 over-wide pages).
- **Correction to the earlier framing in this section:** over-wide lines were
  described as "dominated by tbl text blocks". Measuring against groff showed
  that was wrong — the count is dominated by `.nf` literals, and the widest table
  cases are ones groff renders just as wide. Line width was the wrong proxy for
  the text-block defect; the right one was marker leakage, which was 80 pages.

## Priority issues

### Critical

- [x] **#1 — A man page with `.Xr name` (no section number) panics the whole process.** ✓ fixed (Phase 1): `parse_xr` now peeks for an optional section instead of unwrapping a second argument; `format_xr` renders the bare name when the section is empty. Test `xr_missing_section_does_not_crash`. `parser.rs:2677` — `parse_xr` does `inner.next().unwrap()` for the section after consuming the name, but the grammar `xr = "Xr" ~ (ws+ ~ text_arg)+` only requires **one** argument. Verified: a page containing `.Xr grep` → `thread 'main' panicked at man/man_util/parser.rs:2677:36: called Option::unwrap() on a None value`, **exit 101**. Untrusted input (any installed page) can crash `man`. Fix: treat a missing section as `None` and render `name` alone (mandoc renders `grep` un-decorated), or emit a parse diagnostic and skip the macro.

- [x] **#2 — Deeply nested macros overflow the stack (no recursion limit).** ✓ fixed (Phase 1): the cost was exponential PEG backtracking, not just deep recursion — even ~14 nested partial macros on one line hung for seconds before overflowing. `parse_mdoc` now rejects a line with more than `MAX_NESTING_PER_LINE = 12` nesting macros (`NESTING_MACROS` set) up front with `MdocError::TooDeeplyNested`. Real pages cap at 5, so the margin is wide; rejection is instant. Test `deeply_nested_macros_rejected`. `parser.rs` `parse_element` is mutually recursive through `Rule::element` / `Rule::partial_implicit_element` / `Rule::macro_arg`, and the grammar permits unbounded nesting of partial-implicit blocks. Verified: a page with `.Aq ` repeated 20,000× on one line → `thread 'main' has overflowed its stack`, **exit 134 (SIGABRT)**. A hostile or merely pathological page DoSes the renderer. Fix: cap nesting/recursion depth and return a parse error past the limit.

- [x] **#3 — Legacy `man(7)`/roff pages render as an empty page, yet exit 0.** ✓ fixed (Phase 5): added a dedicated `man7` renderer (`man_util/man7.rs`) for the common `man(7)` macro subset (`.TH`, `.SH`, `.SS`, `.PP`/`.LP`/`.P`, `.TP`, `.IP`, `.HP`, `.RS`/`.RE`, `.B`/`.I`/`.SM`/`.SB`, `.BR`/`.RB`/`.BI`/`.IB`/`.IR`/`.RI`, `.nf`/`.fi`, `.br`, comments, inline `\f` escapes) with fill/word-wrap, `.TP`/`.IP` tag handling, headers/footers, and overstrike emphasis. `format_man_page` detects `man(7)` (first macro `.TH`/`.SH`) and routes to it; `mdoc(7)` is unchanged. The exit-0-on-empty half is also fixed: a `man(7)`-detected page producing no body returns `ManError::EmptyPage` (non-zero exit). Roff programmability (`.if`/`.ie`/`.de`/`.nr`/`.ds`/`.so`) is intentionally out of scope. Tests `man7_page_renders`, `man7_empty_page_errors`, `man_util::man7::tests::*`. The parser implements `mdoc(7)` only; there is no `man(7)` macro set (`.TH`/`.SH`/`.PP`/`.TP`/`.B`/`.BR`/`.IR` are absent — `grep -cE '"TH"|"PP"|"TP"|"BR"' parser.rs` → 0) and no roff request layer (`.if`/`.ie`/`.de`/`.ds`/`.nr`/`.so`). Verified: a standard `man(7)` page (`.TH TEST 1 … / .SH NAME / .B test / .TP …`) produced **only** the `UNTITLED … LOCAL … UNTITLED` header/footer — the entire NAME/SYNOPSIS/DESCRIPTION/EXIT STATUS body was dropped — and the process **exited 0** with no diagnostic. On a typical system whose pages are `man(7)`, `man <utility>` silently shows nothing useful while reporting success; this fails both "shall write information about each of the `name` operands" (DESCRIPTION, p. 3163) and the EXIT STATUS contract (p. 3165: `>0` on error). Fix: implement (at least a subset of) the `man(7)` macro set + roff requests, or — at minimum — detect an unrenderable/empty result and exit non-zero with a diagnostic. (Caveat: if the project intends to ship only `mdoc` pages, the empty-output-with-exit-0 half is still a bug.)

### Major

- [x] **#4 — No bold / italic / underline is ever emitted; `.Sy`, `.Em`, `.Sh`/`.Ss` headings, `.Bf` all render as plain text.** ✓ fixed (Phase 3): emphasis is emitted as nroff backspace-overstrike (`c\bc` bold, `_\bc` underline) — `.Sy`/`.Sh`/`.Ss`/`.Bf -symbolic` bold, `.Em`/`.Ar`/`.Bf -emphasis` underline. Formatting inserts zero-width style markers; a final `apply_styling` pass resolves them, and `display_width` keeps wrapping correct. Styling is enabled only for an interactive terminal (`FormattingSettings.styling = stdout.is_terminal()`), so piped/redirected output stays plain (and the 599 snapshot tests are byte-identical). Tests `sy_is_bold_overstrike`, `em_and_ar_are_underline_overstrike`. Every SGR path was commented out: `format_em` (`formatter.rs:3373`), `format_sy` (`formatter.rs:3935`), `format_bf_block` (`formatter.rs:1465`); the `_supports_bold/_italic/_underline` helpers (`formatter.rs:527–548`) are dead (`_`-prefixed, never called). Verified: `.Sy boldword` and `.Em emphword` → literal `boldword emphword` with no ESC bytes (`cat -v` shows none). mandoc (`-Tutf8`) renders `.Sy` bold and `.Em` underlined/italic; headings bold. Output is readable but loses all emphasis the page author encoded. Fix: emit SGR (or nroff backspace-overstrike) for the emphasis macros and headings, gated on tty/`terminfo`.

- [x] **#5 — roff font escapes `\fB` / `\fI` / `\fR` / `\f[..]` pass through as literal text.** ✓ fixed (Phase 3): `replace_escapes` now runs `replace_font_escapes`, which maps `\fB`/`\f(CB`→bold, `\fI`/`\f(CI`→underline, `\fR`/`\fP`/unknown→reset (via the same style markers as #4). The escapes are always removed (no literal leak even when styling is off) and drive emphasis when on. Tests `font_escapes_styled_when_on`, `font_escapes_stripped_when_off`. They were absent from the `substitutions()` escape table (`formatter.rs:39–443`) and from the grammar. Verified: `This is \fBbold\fR text.` renders verbatim as `This is \fBbold\fR text.`. Inline font changes are extremely common in real pages (including `man(7)` ones); the raw escapes leak into the displayed output. Fix: handle `\f` font-selection escapes in the roff/escape layer (toggle the same emphasis machinery as #4, or strip when styling is unavailable).

- [x] **#6 — `PAGER` is piped through even when standard output is not a terminal.** ✓ fixed (Phase 2): `display_pager` now writes directly to stdout when `-c` is set **or** `io::stdout().is_terminal()` is false; the pager is spawned only for an interactive terminal. Test `pager_not_invoked_when_piped`. `display_pager` (`man.rs:469–483`) always spawns `$PAGER` (default `more -s`) unless `-c` is given; it never checks `io::stdout().is_terminal()`. POSIX (p. 3164): "**When standard output is a terminal device**, the reference page output shall be piped through the command." Verified: `man -l page | cat` (stdout not a tty) still invokes the pager (a marker `PAGER` script fired). With the default `more` this is benign (it copies when not a tty), but a custom `PAGER` that always paginates/clears corrupts non-interactive use (`man x | grep y`). Fix: pipe through `PAGER` only when `stdout.is_terminal()`; otherwise write directly to stdout.

- [x] **#7 — Terminal width is capped at 78 and never honors a wider terminal (fitness; POSIX does not mandate `COLUMNS`).** ✓ fixed (Phase 2): `get_pager_settings` now derives the width from the available column count with precedence `COLUMNS` > config `width` > terminal ioctl (`ws_col`) > default 78 (one-column right margin kept), so wide terminals are honored and `COLUMNS` works. Test `columns_env_sets_width`. `get_pager_settings` (`man.rs:403–410`) only *reduces* the default `width: 78` (`man.rs:272`) when `ws_col < 79`; a 120-column terminal still renders at 78, and `COLUMNS` is read nowhere. Verified: piped output and `COLUMNS=40` both produce a max line length of 78. mandoc fills to the terminal width (and honors `-O width`/`COLUMNS`). Not a POSIX violation (the `man` ENV VARS list omits `COLUMNS`/`LINES` and the format is implementation-defined), but a real usability gap on wide terminals. Fix: use the ioctl `ws_col` as the width (clamped sanely) and/or honor `COLUMNS`.

- [x] **#8 — `-k` native keyword search does literal-substring matching, not the spec's `grep -Ei` (ERE).** ✓ fixed (Phase 4): `native_keyword_search` compiles each keyword once as a case-insensitive regex (`RegexBuilder … case_insensitive(true)`) and matches names/description with `is_match`, falling back to a literal substring match if the keyword is not valid regex syntax. Test `apropos_regex_keyword_does_not_crash`. `native_keyword_search` (`man.rs:622–628`) matches with `String::contains` (case-insensitive), so ERE metacharacters in a keyword are treated literally; `man -k '^foo$'` cannot work as the spec's illustrative `grep -Ei '^foo$'` would. (The external-`apropos` path, `man.rs:858–878`, delegates to the system database and is fine where present.) POSIX (p. 3163): the search "shall produce results that are the equivalent of the output of … `grep -Ei`". Fix: compile each keyword as a case-insensitive ERE in the native path.

- [x] **#9 — Large, mostly-unguarded panic surface in the parser.** ✓ fixed (2026-08-03, man/ closeout Phase 5). **The finding's premise had gone stale**: it counted 544 `.unwrap()` calls (41 of them `next().unwrap()`) in a 12,054-line pest-driven `parser.rs`. That file no longer exists — `parser.rs` is a 289-line facade with **zero** unwraps, and the live parser (`parse/mdoc.rs`) has 25, none of which a 20-page malformed-mdoc battery could reach. So the per-site sweep the finding asked for was re-scoped to where the surface actually moved: the renderer.

  Seven `unreachable!()` arms and two arithmetic underflows in `formatter.rs` **were** reachable from page content. `sccs(1)`, as shipped on a stock system, aborted the renderer (exit 101): it puts plain text inside `.Rs`, which `format_rs_block` declared impossible. Also reachable: `.Rs` holding a non-`%X` macro; `.Fn foo Ar bar` (a macro as the first argument); `.Dd`/`.In`/`.Lb` with a non-text argument; `.Bl -column` with no column widths (`col_count - 1` underflows); and a nested `.Bl` inside `.Bk` on a narrow terminal (`width - indent` underflows). The two underflows only trap under overflow checks — in release they wrapped to an enormous width and silently produced garbage, which is worse than the panic.

  Each site now formats what it was given, following the pattern and the comment already present three times in this file — *"a renderer must be total"*. The reason these survived is recorded as a coverage gap, not a code one: the entire 104-page corpus is well-formed. `man/test_files/malformed/` (20 pages, several taken from real system pages rather than invented) and `malformed_pages_do_not_crash` now render every hostile shape in a subprocess and require only that `man` terminates without crashing.

### Minor

- [x] **#10 — Non-UTF-8 (e.g. Latin-1) page content is a hard error, not transliterated.** ✓ fixed (Phase 4): `format_man_page` now decodes invalid-UTF-8 input byte-for-byte into the Latin-1 Unicode block instead of erroring, so such pages render (test `non_utf8_page_renders`). `format_man_page` (`man.rs:446`) does `String::from_utf8(man_bytes)?`. Verified: a page with Latin-1 `café` bytes → `man: parsing error: invalid utf-8 sequence … (exit 1)`. No crash (good), but older/localized pages that mandoc renders fail outright. Fix: decode per `LC_CTYPE` (or lossily) instead of rejecting.

- [x] **#11 — `.It` outside a `.Bl` is silently dropped.** ✓ fixed (Phase 4): `prepare_document` tracks `.Bl`/`.El` nesting and rewrites a depth-0 (stray) `.It` into a plain text line so it renders (the grammar only recognized `.It` inside a list); `format_it_block` also renders content if reached. Test `stray_it_renders`. `format_it_block` returns `String::new()` for a stray `.It` (`formatter.rs:~2305`). Verified: `.It orphan item` under a section produced no output. mandoc emits a diagnostic and renders the text. Fix: render the item text (and/or warn).

- [x] **#12 — `ws_col - 1` underflows when a tty reports `ws_col == 0`.** ✓ fixed (Phase 2): width derivation goes through `apply_terminal_width`, which leaves the default in place for a column count `< 2` instead of underflowing. Test `columns_zero_does_not_underflow`. `man.rs:405` computes `(winsize.ws_col - 1) as usize` inside `if winsize.ws_col < 79`; a terminal reporting `ws_col == 0` underflows the `u16` (panic in debug; `65535` width in release). Static (needs a 0-column tty). Fix: guard `ws_col >= 1` (or `saturating_sub`).

- [x] **#13 — `.Bl -width` values above 20 are silently clamped.** ✓ fixed (Phase 2): the `MAX_INDENT = 20` cap was removed; `get_width_indent` now honors the declared width, bounded only by the page width (minus a small text remainder). Test `bl_width_above_20_is_honored`. `formatter.rs:~1336` discards larger declared widths, so wide tag lists mis-indent versus mandoc. Static. Fix: honor the declared width (bounded by terminal width).

- [x] **#14 — Diagnostics are largely un-internationalized; `NLSPATH` (XSI) is not handled.** ⚠ partially addressed (Phase 4): the `-k` "nothing appropriate" diagnostic is now routed through `gettext`, joining the already-`gettext`'d clap help and `setlocale` setup. The `thiserror`-generated `ManError` messages and `NLSPATH` (XSI catalog lookup) remain English/unhandled — a fuller i18n pass is still open. `setlocale(LC_ALL, "")` and `gettext` are set up (`man.rs:923–925`), and clap help strings are wrapped, but runtime diagnostics (`ManError` `#[error("…")]` text, `man.rs:144–195`; the `nothing appropriate` message, `man.rs:886`) are raw English. POSIX lists `LC_MESSAGES` and (XSI) `NLSPATH` as affecting `man`. Fix: route diagnostics through `gettext`; honor `NLSPATH` for catalogs.

- [x] **#15 — `.Tg` is silently stripped before parsing.** ✓ fixed (Phase 4): the wholesale `.Tg`-line strip in `prepare_document` was removed; `.Tg` now flows through the grammar/`parse_tg`/`format_tg` (which renders nothing, the correct terminal result) without discarding the line. Test `tg_line_is_harmless`. `prepare_document` removes `.Tg` lines (`parser.rs:183`) even though a `tg` grammar rule and `parse_tg` exist; tag metadata is lost. Minor/cosmetic. Fix: route `.Tg` through its handler (or document the omission).

- [x] **#16 — `man a b c` aborts at the first not-found name instead of continuing.** ✓ fixed (Phase 4): the per-name loop now reports a failing operand and continues with the rest, setting a non-zero overall exit instead of propagating the first error with `?`. Test `missing_name_does_not_abort_batch`. The per-name loop (`man.rs:899–907`) propagates the first `PageNotFound` with `?`, so later operands are never attempted. POSIX CONSEQUENCES OF ERRORS is "Default" (so stopping is permissible), but historical `man` continues and reports a non-zero exit at the end. Fix: continue past a missing operand, set `no_errors = false`, exit non-zero.

## Detailed conformance matrix

### SYNOPSIS / Options (spec: `man [-k] name...`, XBD 12.2)
- [x] `-k` CONFORMS — present as `apropos` (`man.rs:82–86`); uses system `apropos` when available, else a native scan. Keyword semantics diverge from `grep -Ei` (Minor #8). The POSIX-mandated option **is** supported.
- [x] XBD 12.2 option parsing CONFORMS — clap handles `--`, bundling, unknown-option errors.
- Extensions present (non-POSIX, no conflict — informational): `-a/--all`, `-C` (config file), `-c/--copy`, `-f/--whatis`, `-h` (synopsis-only), `-l/--local-file`, `-M`/`-m` (path override/augment), `-S` (architecture), `-s` (section), `-w` (list pathnames), `--help`. Per audit scope these are noted, not flagged.

### Operands / STDIN
- [x] `name` operand CONFORMS — one or more names accepted (`man.rs:140`, `man.rs:899`).
- [x] **STDIN: Not used** CONFORMS — the only `stdin` use (`man.rs:332–344`) is writing the formatted page *into the pager subprocess*, not reading commands/content. Matches spec (p. 3163).
- [x] `name` not a standard utility, `-k` absent → "results unspecified": the tool searches the man path and errors `PageNotFound` (exit 1) if not found — within "unspecified", acceptable.

### Environment variables (spec p. 3163–3164)
- [x] `LANG` PARTIAL→CONFORMS — `setlocale(LC_ALL, "")` (`man.rs:923`) lets libc honor `LANG` precedence.
- [x] `LC_ALL` CONFORMS — via `setlocale(LC_ALL, "")`.
- [x] `LC_CTYPE` PARTIAL→improved — `setlocale` covers it and non-UTF-8 input now falls back to Latin-1 decoding (#10); a fully `LC_CTYPE`-charset-driven decode is still not implemented.
- [x] **`LC_MESSAGES` CONFORMS (Minor #14)** — every `ManError` diagnostic now routes through `gettext`, joining the clap help strings and the `-k` message. `thiserror`'s `#[error("…")]` is a compile-time literal and cannot be translated, so the derive was replaced with a hand-written `Display` (test `error_messages_are_unchanged_and_translatable` pins every message).
- [x] **`NLSPATH` (XSI) CONFORMS (Minor #14)** — consulted, ahead of `bindtextdomain`, `TEXTDOMAINDIR` and the system locale directories, with `%N`/`%L`/`%l`/`%t`/`%c` substitution. Fixed in `gettext-rs` rather than in `man`: that shim was an identity passthrough for the whole workspace, so no utility could be translated however many strings were wrapped, and wiring `man` alone to a catalog would have made it the only utility that translates anything. The `.mo` reader is total — a catalog is untrusted input — and behavior is unchanged where no catalog exists.
- [x] `PAGER` CONFORMS-with-caveat — honored, default `more` (`man.rs:476`); but piped unconditionally (Major #6).
- Non-POSIX: `MANPATH`/`MACHINE` are read/set (`man.rs:782–793`); the spec RATIONALE explicitly leaves `MANPATH` out of POSIX. Informational.

### Asynchronous events
- [x] CONFORMS — spec says "Default"; no custom signal handling is required and none is installed.

### STDOUT / STDERR
- [x] STDOUT format CONFORMS — POSIX makes it implementation-defined; the tool emits a formatted page. For an mdoc page the mandated content (syntax/options/operands/env/exit) is preserved (see section-preservation below).
- [x] STDERR CONFORMS — diagnostics go to stderr (`man.rs:886`, `933`, `946`); informational `-k` lines go to stdout (`man.rs:891`).
- [x] **PAGER routing CONFORMS (Major #6)** — `man.rs:463` and `man.rs:612` gate the pager on `stdout().is_terminal()`. Test `pager_not_invoked_when_piped`.

### Output files
- [x] CONFORMS — none (spec: "None").

### Extended description / EXIT STATUS / Consequences of errors
- [x] EXTENDED DESCRIPTION CONFORMS — spec: "None".
- [x] EXIT STATUS CONFORMS on the error path — `main` returns 0 on success, 1 on `Ok(false)`/`Err` (`man.rs:939–951`).
- [x] EXIT STATUS on the empty-render path (Critical #3 fixed) — an unrenderable `man(7)` page now exits non-zero (`ManError::EmptyPage`).
- [x] CONSEQUENCES OF ERRORS (Minor #16 fixed) — a missing operand is reported and the batch continues; exit status is non-zero.
- [x] FUTURE DIRECTIONS (newline-in-pathname → error): N/A — encouraged, not required; not implemented.

### Section preservation (POSIX content requirement, Issue 8)
- [x] CONFORMS for mdoc pages — `format_mdoc` (`formatter.rs:678`) iterates **all** top-level elements with no section allowlist/blocklist. Verified: a page with NAME/SYNOPSIS/DESCRIPTION/ENVIRONMENT/"EXIT STATUS" rendered all five headings. The Issue-8 requirement that `man <util>` show syntax/options/operands/**environment variables**/**exit status** is a property of the page *content* (system-supplied), and the formatter does not drop those sections. (The `-h` path intentionally shows SYNOPSIS only — `formatter.rs:647–657` — which is an extension, not the default.)

### Engine fitness — parser (`parser.rs`, mdoc/roff)
- [x] mdoc macro coverage is broad — prologue (`Dd`/`Dt`/`Os`), sections (`Sh`/`Ss`/`Sx`/`Nd`), full/partial blocks (`Bd`/`Bl`/`It`/`Rs`, `Aq`/`Bq`/`Op`/`Fo`/`Oo`/…), in-line semantic macros (`Fl`/`Ar`/`Cm`/`Ic`/`Nm`/`Pa`/`Va`/`Fn`/`Ft`/`Em`/`Sy`/`Dv`/`Er`/`Ev`/`Xr`/…), and text-production (`Ex`/`Rv`/`St`/`At`/`Bx`/`Nx`/`Ox`/`Dx`/`Lb`) are implemented (PEG grammar `mdoc.pest`; dispatch `parser.rs:~360–2700`).
- [x] `man(7)` macro subset implemented (Critical #3 fixed) — `man_util/man7.rs`.
- [x] **roff request layer IMPLEMENTED** (engine rewrite) — `.ds`/`.as`/`.nr`/`.if`/`.ie`/`.el`/`.while`/`.de`/`.am`/`.so`/`.ig`/diversions/expr in `man_util/roff/`; `.TS`/`.EQ` preprocessed by `man_util/preproc/`.
- [x] **`\f` font escapes CONFORM (Major #5)** — `replace_font_escapes` maps `\fB`/`\fI`/`\fR`/`\fP`/`\f(CB`/`\f(CI` onto the style markers and always removes them. `.ds`-defined strings are handled by the roff front-end (`\*X`, `\*(xx`, `\*[name]`), with a cycle guard and an expansion-size cap. `\,` and `\/` (italic corrections) are now dropped too — help2man emits them on every GNU page, so they had been visible as stray backslashes across most of /usr/share/man.
- [x] **Panic surface (Major #9, Critical #1/#2)** — see #9 above: the parser class is structurally gone, and the renderer's reachable panics are fixed and covered by a malformed-input corpus.
- [x] UTF-8 byte/char handling in the parser is sound (slicing via `char_indices()`/`len_utf8()`); no byte-index hazards found there.

### Engine fitness — formatter (`formatter.rs`, AST→terminal)
- [x] `.Bl` list types implemented — `-bullet`/`-dash`/`-enum`/`-item`/`-tag`/`-hang`/`-ohang`/`-inset`/`-diag`/`-column`/`-compact` (`formatter.rs:1527–1900`). `-enum` numbering is correct (starts at `1.`, verified — refutes an agent claim).
- [x] `.Bd` display modes implemented — `-filled`/`-unfilled`/`-ragged`/`-centered`/`-literal`/`-offset` (`-unfilled`/`-ragged` reflow like `-filled`; `-compact` accepted but ignored).
- [x] **Emphasis (Major #4)**, **terminal width (Major #7)** and **`.Bl -width` (Minor #13)** all conform — overstrike emphasis gated on a tty, `COLUMNS` honored (tests `columns_env_sets_width`, `columns_zero_does_not_underflow`, `bl_width_above_20_is_honored`).
- [x] **Wide/CJK characters CONFORM** — `display_width` measures terminal cells via `unicode-width`, and the wrapper, `\w'…'`, the three-part header/footer and the tbl layout all route through it. Previously a CJK paragraph wrapped at twice the terminal width (130 columns into a 78-column terminal) and combining marks wrapped early. Tests `wide_and_combining_characters_measure_in_cells`, `wide_characters_wrap_at_the_terminal_width`, `width_escape_counts_terminal_cells`.
- [x] **`unreachable!()` arms** — all seven input-reachable arms are gone (see #9). The one remaining `unreachable!()` in the file is inside a test helper.

## Test coverage signal

Existing tests render fixture mdoc files and compare output. Not covered (each is a "write a test" item):
- [x] `.Xr name` with a missing section (Critical #1) — `xr_missing_section_does_not_crash`.
- [x] Deeply nested macros / recursion depth (Critical #2) — `deeply_nested_macros_rejected`.
- [x] A `man(7)`/roff page (Critical #3) — `man7_page_renders`, `man7_empty_page_errors`.
- [x] Emphasis macros (`.Sy`/`.Em`) and `\fB…\fR` escapes (Major #4/#5) — `sy_is_bold_overstrike`, `em_and_ar_are_underline_overstrike`, `font_escapes_styled_when_on`, `font_escapes_stripped_when_off`, `italic_corrections_are_removed`.
- [x] `PAGER` not invoked when stdout is not a terminal (Major #6) — `pager_not_invoked_when_piped`.
- [x] Width honoring a wide terminal / `COLUMNS` (Major #7) — `columns_env_sets_width`, `columns_zero_does_not_underflow`.
- [x] `-k` ERE semantics (Major #8) and the native-vs-`apropos` fallback — `apropos_regex_keyword_does_not_crash`, `apropos_with_keywords`.
- [x] Non-UTF-8 page content (Minor #10) — `non_utf8_page_renders`; `.It` outside `.Bl` (Minor #11) — `stray_it_renders`.

## Suggested PR groupings

- **PR A — "Don't crash on a malformed page" (Critical #1, #2; Major #9):** make `parse_xr` tolerate a missing section, add a recursion-depth cap, and sweep input-reachable `next().unwrap()` sites. Biggest robustness win.
- **PR B — "Render the pages people actually have" (Critical #3):** add a `man(7)` macro subset + a minimal roff-request/`\f`-escape layer (overlaps Major #5); at minimum, exit non-zero with a diagnostic when rendering yields no body.
- **PR C — "Emphasis & escapes" (Major #4, #5):** emit SGR/overstrike for `.Sy`/`.Em`/headings/`.Bf`, and translate `\fB`/`\fI`/`\fR`/`\f[..]`.
- **PR D — "Pager & width fidelity" (Major #6, #7; Minor #12, #13):** pipe through `PAGER` only when stdout is a tty; use the real terminal width (and/or `COLUMNS`); fix the `ws_col==0` underflow and the `-width` clamp.
- **PR E — "Search & i18n" (Major #8; Minor #10, #14):** ERE keyword matching in the native path; `LC_CTYPE`-aware decoding; route diagnostics through `gettext`; honor `NLSPATH`.

## Appendix — agent/heuristic findings refuted by behavioral testing

Per the playbook, claims that did not survive verification are recorded, not deleted:
- **Empty/single-char `.Bd` → `formatter.rs:699` `remove(0)` panic** — REFUTED. An empty top-level `.Bd -literal`/`.Ed` (and a single-char body) both rendered with **exit 0**; `format_bd_block` always emits ≥1 character, so the unconditional `pop()`+`remove(0)` is fragile but not reached by simple input. (Left as a latent-fragility note, not a Critical.)
- **`.Nm` with no name → `formatter.rs:2345` `unwrap` panic** — REFUTED. The `unwrap` is in the `else` of `if self.formatting_state.first_name.is_none()` (`formatter.rs:2330`), so `first_name` is guaranteed `Some` there; a name-less `.Nm` (first or otherwise) rendered with exit 0.
- **`.Bl -enum` numbers from `0.`** — REFUTED. Verified output is `1.`, `2.`, `3.` (a cosmetic nit remains: no space after the period, e.g. `1.first`).
