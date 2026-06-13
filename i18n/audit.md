# POSIX.1-2024 Conformance Audit — `i18n` crate

**Crate:** `i18n/` (`posixutils-i18n`)
**Utilities:** `gencat`, `gettext`, `ngettext`, `iconv`, `locale`, `localedef`, `msgfmt`, `xgettext` (8 binaries — all POSIX.1-2024 utilities; the `gettext`/`ngettext`/`msgfmt`/`xgettext` family was added to the standard in Issue 8)
**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 (Shell & Utilities), §3.
**Spec source:** the mega-PDF `~/tmp/POSIX.2024.pdf` (no sliced tree available for this crate); per-utility sections were extracted verbatim to `~/tmp/i18n-spec/*.txt` and cross-referenced against the implementation.
**Date:** 2026-06-13

Findings marked **✓ verified** were confirmed by building the crate (`cargo build --release -p posixutils-i18n`) and running the binary; the observed behavior is quoted inline. Unmarked findings are from spec-vs-source reading only.

---

## TL;DR

The `i18n` crate provides working *implementations* (not host-libc delegations) for all eight utilities, but every one has spec-conformance gaps, and several have **Critical** defects that corrupt output or crash on common inputs:

- **`gencat` panics** (`exit 101`) on the most basic real-world workflow — merging into a catalog file that already exists with unexpected contents — and **does not process any escape sequences** (`\n`, `\t`, `\ddd`, line continuation), so message catalogs are silently miscompiled.
- **`iconv` returns exit status 0 after printing a conversion error** and after charmap-conversion failures, defeating the whole point of an exit code; its default codeset is derived from a naive `LANG` split that hard-errors under `LANG=C`.
- **`locale` returns hardcoded values** for `d_fmt`/`t_fmt`/`d_t_fmt`/`yesexpr`/`noexpr` regardless of the active locale, and is missing ~30 required `LC_MONETARY`/`LC_TIME` keywords.
- **`localedef` produces no usable locale** — it writes a one-line `LC_IDENTIFICATION` marker file and never parses the `-f charmap`, `LC_CTYPE`, or `LC_COLLATE` data at all.
- **`msgfmt` ignores `domain` directives**, so a multi-domain `.po` is merged into a single output file named after the *input* filename rather than the per-domain `.mo` files the spec mandates.
- **`gettext`/`ngettext`** append a spurious trailing `<newline>` outside `-s` mode, are missing the mandatory `-E` option, and `ngettext` cannot accept the optional leading `[textdomain]` operand from the spec synopsis.
- **`xgettext`** writes `messages.pot` instead of the spec-mandated `messages.po`, registers the exclude option as `-X` instead of `-x`, sorts output alphabetically instead of in extraction order, and (by design) parses Rust as well as C.

The good news: `.po`/`.mo` round-tripping, the gettext plural-expression evaluator (`plural.rs`), UTF-8/16/32 BOM-aware transcoding, and the gencat/po grammars' happy paths largely work. None of the Critical defects are architectural dead-ends.

---

## Cross-cutting themes

These recur across the crate and are worth fixing as themed sweeps rather than per-utility:

1. **Exit status 0 after a diagnosed error.** `iconv` (charmap path, `-s` path, default-codeset failure), `locale` (unknown keyword, `charmap` operand), and `localedef` (no-op success) all print to stderr and then exit 0. POSIX requires `>0` on error for every one of these. The root cause is `main()` returning `()` / `Ok(())` with no `had_error` flag threaded out of the worker functions. **✓ verified** on `iconv`, `locale`.

2. **Hardcoded English diagnostics; `LC_MESSAGES` is inert.** Every utility calls `setlocale(LC_ALL, "")` + `textdomain(...)` near `main()`, but the runtime diagnostic strings (`eprintln!`, `ParseError::fmt`, clap `about` strings evaluated at attribute-expansion time) are not wrapped in runtime `gettext()`. The spec's `LC_MESSAGES` entry ("affect the format and contents of diagnostic messages") is therefore unmet crate-wide. Minor, but pervasive.

3. **`NLSPATH` / `LANGUAGE` not honored.** The XSI `NLSPATH` variable (required to take precedence over `TEXTDOMAINDIR`) and the XSI `LANGUAGE` variable are not consulted by the gettext family. Several utilities list `NLSPATH` in their ENVIRONMENT VARIABLES section but never read it.

4. **`LC_CTYPE`-driven byte interpretation ignored.** `gencat`, `iconv`, `msgfmt`, and `localedef` read input as UTF-8 (`read_to_string`) and never consult `LC_CTYPE` to interpret byte sequences, so on non-UTF-8 locales multibyte text is mishandled.

5. **Real implementation, not delegation — but incomplete.** Unlike some posixutils crates that shell out to the host tool, these are genuine in-tree implementations. That is the right call, but it means the gaps (charmap parsing, locale compilation, codeset matrix) are real functional holes rather than thin wrappers. Two places *do* delegate: `locale -a` spawns the system `locale -a` when a `locale-archive` is present (`locale_lib/platform.rs`), and the C-locale machinery is reached via `localeconv()`/`setlocale()`.

---

# `gencat`

**Implementation:** `gencat.rs` (823 lines)
**Tests:** `tests/gencat/mod.rs` (129 lines)
**Spec:** POSIX.1-2024 `gencat` (pp. ~2959–2961; extract `~/tmp/i18n-spec/gencat.txt`)

## TL;DR
The message-text-source grammar is only half-implemented: escape sequences and line continuation are missing, message-collision *replacement* is actually *append*, the delete-by-number form is rejected as an error, and a pre-existing catalog with an unexpected header **panics the process**. Only one `msgfile` is accepted despite the `msgfile...` synopsis.

## Priority issues

### Critical
- [x] **GC-1 — Escape sequences not processed.** ✓ fixed (Phase 1): `process_escapes()` expands `\n \t \v \b \r \f \\ \ddd`. `\n`, `\t`, `\v`, `\b`, `\r`, `\f`, `\\`, and `\ddd` octal are stored verbatim as two characters instead of being converted. `gencat.rs:420` (text taken straight from `input.lines()` at `:335`). **✓ verified:** `1 Hello\tWorld` is stored as bytes `…48 65 6c 6c 6f 5c 74 57…` (`\t` literal `5c 74`, not a tab `09`). Fix: run a `process_escapes()` pass over the message text before storing.
- [x] **GC-2 — Process panics on a pre-existing catalog with an unexpected magic/header.** ✓ fixed (Phase 1): graceful `InvalidData` error + length guard; exit 1. `panic!("DOESNT MATCH")` at `gencat.rs:580`. **✓ verified:** `gencat junk.cat m1.msg` → `thread 'main' panicked at i18n/gencat.rs:580 … DOESNT MATCH`, `exit 101`. Fix: return a graceful `Err` → exit `>0`.
- [x] **GC-3 — Backslash-newline line continuation not implemented.** ✓ fixed (Phase 1): trailing-backslash continuation joined before tokenizing. `input.lines()` at `gencat.rs:335` splits on `\n` before any continuation join, so a line ending in `\` is mis-parsed. Fix: join continued lines before tokenizing.

### Major
- [x] **GC-4 — msgid collision appends instead of replaces.** ✓ fixed (Phase 1): `add_msg` replaces existing `msg_id` text in-place. Spec DESCRIPTION: "If set and message numbers collide, the new message text … shall replace the old." `add_msg` appends a new list node (`gencat.rs:463-485`). **✓ verified:** a source with `1 First` then `1 Second` in `$set 1` emits **both** `First` and `Second`. Fix: replace existing `msg_id` text in-place.
- [x] **GC-5 — Only one `msgfile` accepted; synopsis is `msgfile...`.** ✓ fixed (Phase 2): `msgfile: Vec<PathBuf>`, merged in order. `msgfile: PathBuf` (`gencat.rs:46`) should be `Vec<PathBuf>` processed in order. **✓ verified:** `gencat cat m1.msg m2.msg` → clap `error: unexpected argument 'm2.msg'`, `exit 2`.
- [x] **GC-6 — Delete-by-number form rejected.** ✓ fixed (Phase 1): a bare message-number line deletes that message via `delete_msg`. A message line with a number and no separator/text shall *delete* that message; instead it errors. `gencat.rs:412-413`. **✓ verified:** bare `1` line → `Error: Invalid line 3 with content 1`, `exit 1`.
- [x] **GC-7 — `$delset` breaks on a trailing comment and skips range validation.** ✓ fixed (Phase 2): first token only, `[1,NL_SETMAX]` checked. `rem.trim().parse::<u32>()` (`gencat.rs:369`) parses the whole remainder, so `$delset 2 a comment` is a parse error; no `[1, NL_SETMAX]` check. Fix: `splitn(2, …)`, validate the first token.
- [x] **GC-8 — Message-id range/order not validated.** ✓ fixed (Phase 2): `[1,NL_MSGMAX]` enforced; ascending is advisory (collision-replace is legal), noted in code. No check that `msg_id` is in `[1, NL_MSGMAX]` or ascending within a set (`gencat.rs:419`). `msg_id == 0` is silently accepted.

### Minor
- [ ] **GC-9 — Diagnostics not localized** (`ParseError::fmt`, `gencat.rs:229-262`) — `LC_MESSAGES` has no effect.
- [x] **GC-10 — `NL_SETMAX` hardcoded to 255** ✓ fixed (Phase 2): documented as matching the POSIX/glibc/macOS `<limits.h>` limit; `NL_MSGMAX` added too. (`gencat.rs:25`) rather than queried from the platform `<limits.h>`. Incidentally correct on Linux.
- [x] **GC-11 — `$quote c` uses byte length** ✓ fixed (Phase 2): now `chars().count() == 1`. (`c.len() == 1`, `gencat.rs:349`) not char count; a single multibyte quote char is rejected.
- [x] **GC-12 — `msgfile` `-` (stdin) is untested** ✓ fixed (Phase 2): added `gencat_msgfile_from_stdin` integration test.; correctness depends on `plib::io::input_stream` and is unexercised.

## Conformance matrix

### Synopsis / Options / Operands
- [x] `OPTIONS: None` CONFORMS — no options defined (`gencat.rs:41-47`); clap `--help`/`--version` are benign extensions.
- [x] **`msgfile...`** — ✓ fixed (Phase 2): multiple operands merged in order (GC-5).
- [x] `catfile -` → stdout CONFORMS (`gencat.rs:809`).
- [x] **`msgfile -` → stdin** — ✓ verified by test (GC-12).

### STDIN / INPUT FILES / STDOUT / STDERR
- [x] STDIN only when `msgfile` is `-` CONFORMS (`gencat.rs:307`).
- [x] STDOUT only when `catfile` is `-` CONFORMS (`gencat.rs:809`).
- [x] STDERR diagnostics only CONFORMS (`gencat.rs:819`).
- [x] Input read as text CONFORMS (`gencat.rs:309`), modulo LC_CTYPE (theme 4).

### Environment variables
- [ ] **`LC_CTYPE` MISSING** — source read as UTF-8, not LC_CTYPE-interpreted.
- [ ] **`LC_MESSAGES` PARTIAL** — diagnostics hardcoded English (GC-9).
- [x] `LANG`/`LC_ALL` PARTIAL — honored only via `setlocale` (`gencat.rs:786`).
- [ ] **`NLSPATH` PARTIAL** — not read for the utility's own diagnostics.

### Extended description (source-file grammar)
- [x] `$ ` comment, empty lines CONFORMS (`gencat.rs:339`).
- [x] `$set n` ascending check + `[1,NL_SETMAX]` CONFORMS (`gencat.rs:389-393`); repeated same set number silently accepted (Minor).
- [x] **`$delset`** — ✓ fixed (Phase 2) (GC-7).
- [x] `$quote c` / empty `$quote` CONFORMS (`gencat.rs:343-356`), byte-length caveat (GC-11).
- [x] **message escapes** (GC-1), **continuation** (GC-3), **delete-by-number** (GC-6), **collision replace** (GC-4) — ✓ fixed (Phase 1).

### Exit status / errors
- [x] `0` success / `>0` error CONFORMS for parse errors (`gencat.rs:817-822`) — except the GC-2 panic path.

## Test coverage — not covered
- [ ] Escape sequences, line continuation, delete-by-number, msgid collision-replace.
- [ ] Pre-existing catalog merge (would have caught GC-2).
- [ ] `msgfile -` (stdin); multiple `msgfile` operands.

---

# `gettext` and `ngettext`

**Implementations:** `gettext.rs` (150 lines), `ngettext.rs` (100 lines)
**Shared library:** `gettext_lib/{lookup,catalog,mo_file,plural,po_file}.rs`
**Tests:** `tests/gettext/mod.rs` (106), `tests/ngettext/mod.rs` (96)
**Spec:** POSIX.1-2024 `gettext, ngettext` (shared page; extract `~/tmp/i18n-spec/gettext.txt`)

> Note on delegation: both binaries `use gettextrs::…` only for their own startup self-localization. All user-facing msgid lookup goes through the in-tree `gettext_lib`. Conformance is judged against the in-tree code.

## TL;DR
Lookup, locale precedence (`LC_ALL > LC_MESSAGES > LANG`), and the plural-expression evaluator work. But output gets a spurious trailing newline outside `-s`, the mandatory `-E` option is absent, `ngettext` cannot take the optional `[textdomain]` operand, and `NLSPATH`/`LANGUAGE` are unimplemented.

## Priority issues

### Major
- [x] **GT-1 — `-E` option missing from both utilities.** ✓ fixed (Phase 9): both utilities accept `-E` (conflicts_with `-e`). The default is no escape processing (so the `-s`→`-E` default is satisfied), and `-e` opts in. Spec: "`-E` Do not process C-language escape sequences," and "if `-s` … is specified then `-E` shall be the default."
- [x] **GT-2 — Spurious trailing `<newline>` outside `-s`.** ✓ fixed (Phase 9): the non-`-s` gettext path and ngettext use `print!` (no trailing newline); gettext `-s` appends a newline only when `-n` is not given. APPLICATION USAGE: "unless `-s` … is specified without `-n`, the message(s) … are not followed by a `<newline>`."
- [x] **NG-1 — `ngettext` missing the optional `[textdomain]` operand.** ✓ fixed (Phase 9): operands are parsed from a `Vec` accepting either `msgid msgid_plural n` (3) or `textdomain msgid msgid_plural n` (4). `ngettext mail recipient recipients 1` now works.
- [x] **GT-3 — `NLSPATH` not implemented.** ✓ fixed (Phase 9): `find_mo_file` consults `NLSPATH` first (taking precedence over `TEXTDOMAINDIR`/defaults), expanding `%N`/`%L`/`%l`/`%t`/`%c`/`%%` per locale variant.
- [x] **GT-4 — NULL-textdomain path diverges.** ✓ fixed (Phase 9): domain resolution returns `None` when none of operand/`-d`/`TEXTDOMAIN` yields a non-empty name; gettext then returns the (escape-processed) msgid directly with no lookup, and ngettext selects the Germanic plural form without a fabricated-domain lookup.
- [x] **GT-5 — `mo_file.rs::get_plural_index` ignores the parsed plural expression.** ✓ fixed (Phase 9): it now parses and evaluates the catalog's `Plural-Forms` expression via `plural.rs` (clamped to `nplurals`), falling back to the Germanic rule only when parsing fails. Unit-tested with a Polish 3-form expression.

### Minor
- [x] **GT-6 — `LANGUAGE` (XSI) not consulted.** ✓ fixed (Phase 9): when set and the locale is not C/POSIX, `LANGUAGE`'s colon-separated entries are tried in priority order (effective locale last).
- [x] **GT-7 — gettext non-`-s` mode accepts multiple msgids.** ✓ fixed (Phase 9): the non-`-s` form takes `[textdomain] msgid` (one msgid); more than two operands is an error.
- [x] **GT-8 — Octal escape requires a leading zero.** ✓ fixed (Phase 9): octal is `\NNN` (1–3 digits, no leading zero required) and `\0`/`\x00` now produce a NUL instead of being dropped.
- [x] **GT-9 — `-e` not applied to the fallback (catalog-miss) msgid.** ✓ fixed (Phase 9): escapes are applied to the operand up front, and that processed string is both the lookup key and the fallback; the translation itself is no longer escape-processed.
- [x] **GT-10 — Existing tests encode the non-conformant trailing newline.** ✓ fixed (Phase 9): the gettext/ngettext integration tests were updated to expect no trailing newline (and new tests added for `-E`, the ngettext `[textdomain]` operand, octal escapes, and plural evaluation).

## Conformance matrix

### Options
- [x] `-d`, `-n`, `-s` (gettext) CONFORMS (`gettext.rs:29-39`).
- [x] `-d` (ngettext) CONFORMS; `-n`/`-s` correctly absent for ngettext.
- [x] **`-E` CONFORMS** (GT-1, Phase 9); **`-e` CONFORMS** (GT-8/GT-9, Phase 9).

### Operands / output
- [x] gettext `[textdomain] msgid` disambiguation CONFORMS; non-`-s` single-msgid enforced (GT-7, Phase 9).
- [x] **ngettext `[textdomain]` CONFORMS** (NG-1, Phase 9); count parsed via Rust `parse::<u64>` vs `strtoul` (negligible).
- [x] **trailing newline CONFORMS** (GT-2, Phase 9); `-s` space-separation CONFORMS.

### Environment variables
- [x] `LC_ALL`/`LC_MESSAGES`/`LANG` precedence CONFORMS (`lookup.rs:83-107`).
- [x] `TEXTDOMAIN` CONFORMS; `TEXTDOMAINDIR` CONFORMS (NLSPATH now takes precedence).
- [x] **`NLSPATH` CONFORMS** (GT-3, Phase 9); **`LANGUAGE` CONFORMS** (GT-6, Phase 9).

### Plural / exit status
- [x] Plural-expression parse+evaluate via `plural.rs`/`catalog.rs` CONFORMS.
- [x] **`mo_file.rs` plural index CONFORMS** (GT-5, Phase 9 — evaluates the parsed expression).
- [x] Exit `0`/`>0` CONFORMS (`gettext.rs:70`, `ngettext.rs:65`).

## Test coverage — not covered
- [x] `-E`; ngettext `[textdomain]` operand; octal/NUL escapes; non-Germanic plural counts — now covered by added tests (Phase 9). Real `.mo` translation lookup via NLSPATH/LANGUAGE paths remains exercised only manually.

---

# `iconv`

**Implementation:** `iconv.rs` (544 lines)
**Library:** `iconv_lib/{ascii,utf_8,utf_16,utf_32}.rs`
**Tests:** `tests/iconv/mod.rs` (1085 lines)
**Spec:** POSIX.1-2024 `iconv` (extract `~/tmp/i18n-spec/iconv.txt`)

## TL;DR
UTF-8/16/32 + ASCII transcoding with BOM detection works and is well-tested. But the error model is broken: multiple paths **exit 0 after a diagnosed error** or call `exit(1)` from inside library iterators; the default-codeset logic only parses `LANG` and hard-fails on `LANG=C`; `-l` prints names the parser won't accept; and the `-s` flag truncates the stream instead of merely silencing stderr.

## Priority issues

### Critical
- [x] **IC-1 — Exit status 0 after a conversion/codeset error.** ✓ fixed (Phase 3): a shared `had_error: Rc<Cell<bool>>` is threaded into every codec (`to_ucs4`/`from_ucs4`) and into `encoding_conversion`/`charmap_conversion`; `main` exits `1` if it is set after processing all operands. An unconvertible character without `-c` now exits `1` (matching GNU). `main()` returns `Ok(())` even when conversion failed. **✓ verified:** `printf A | LANG=C iconv` → `Error: Could not find a codeset from your locale`, **`exit 0`**. Also the whole `charmap_conversion` path reports via `eprintln!` and returns `()` (`iconv.rs:416-480`), and `-s` error paths `return None` (`ascii.rs:31`, `utf_16.rs:84-106`, `utf_32.rs:75-82`) → exit 0. Spec: ">0 An error occurred," and "absence of `-c`/`-s` shall not affect the exit status." Fix: thread a `had_error` flag to `main` and exit `>0`. (The `LANG`/codeset branch is IC-3, Phase 4.)
- [x] **IC-2 — `exit(1)` called from inside library iterators** (`ascii.rs:29`, `utf_8.rs:48-178`). ✓ fixed (Phase 3): every codec error path now sets `had_error` and returns `None` (stopping that stream) instead of calling `process::exit`; the exit status is decided once in `main`. Remaining file operands are no longer bypassed by a mid-stream process abort.

### Major
- [x] **IC-3 — Default codeset reads only `LANG`, splitting on `.`.** ✓ fixed (Phase 4): `locale_codeset()` now calls `nl_langinfo(CODESET)` (after the existing `setlocale(LC_ALL, "")`), which honors the `LC_ALL > LC_CTYPE > LANG` chain; on non-Linux/macOS or an empty result it falls back to UTF-8. `LANG=C`/`POSIX` now resolve to the codeset name `nl_langinfo` reports (`ANSI_X3.4-1968` / `US-ASCII`), which `Encodings::canonical_name` maps to ASCII, so `printf A | LANG=C iconv` succeeds (exit 0) instead of erroring. Ignores the `LC_ALL > LC_CTYPE > LANG` chain; `LANG=C`, `LANG=POSIX`, `LANG=en_US` (no `.`), or unset all hit the error/exit branch (`iconv.rs:494-512`). **✓ verified** (see IC-1). Fix: `nl_langinfo(CODESET)` after `setlocale`.
- [x] **IC-4 — `-s` truncates the stream.** ✓ fixed (Phase 3): `-s` (`suppress_error`) now gates only the stderr message; it no longer governs stream/exit behavior. Stream termination on an unconvertible character is decided solely by the absence of `-c`, and the error is recorded via `had_error` regardless of `-s`. So `-s` without `-c` exits `1` with empty stderr (was: silently `return None` → exit 0). Spec: `-s` "Suppress any messages" (stderr only). The impl uses `return None` as a stream terminator when `-s` is set without `-c` (`ascii.rs:31`, `utf_16.rs:84-106`). Fix: continue after a suppressed error; never use iteration end as an error signal.
- [x] **IC-5 — `-l` lists names the parser rejects.** ✓ fixed (Phase 4): `list_encodings()` now prints the serialized hyphen form via `Display` (`UTF-8`, `UTF-16LE`), and `Encodings::canonical_name` normalizes `-f`/`-t` input case-insensitively and accepts common aliases (`utf8`, `US-ASCII`, `ANSI_X3.4-1968`, `UCS-2`/`UCS-4`, …). The listed names now round-trip. **✓ verified:** `iconv -l` prints `UTF_8`, `UTF_16LE` (underscores, Debug format) but `-f`/`-t` require `UTF-8`/`UTF-16LE` (hyphens), and names are case-sensitive (`iconv -f utf-8 …` → `Error: Unknown encoding: utf-8`). `iconv.rs:178-181`, `:165-175`. Fix: print the serialized (hyphen) form; normalize/upper-case input and accept common aliases.
- [x] **IC-6 — Generic `UTF-16`/`UTF-32` output writes no BOM** (`utf_16.rs:143-151`, `utf_32.rs:98-108`). ✓ fixed (Phase 4): `from_ucs4` prepends a U+FEFF BOM (in the resolved host endianness) when the requested variant is the generic `UTF-16`/`UTF-32`; the explicit LE/BE forms still write none. Verified by round-trip (generic output re-decodes with the BOM consumed).

### Minor
- [x] **IC-7 — Charmap `mb_cur_max` defaults to 0** (`iconv.rs:186`). ✓ fixed (Phase 4): `parse_charmap` now defaults `mb_cur_max` to 1 when the charmap omits it, so the flush guard is no longer always true.
- [x] **IC-8 — Per-byte stdout flush** in the encoding path (`iconv.rs:410-413`). ✓ fixed (Phase 4): `encoding_conversion` writes through a `BufWriter` and flushes once at end instead of flushing after every byte.
- [ ] **IC-9 — Hardcoded English diagnostics** in `iconv_lib` (`ascii.rs:28`, etc.); `LC_MESSAGES` inert. _(Deferred to Phase 12: crate-wide gettext diagnostics sweep.)_
- [x] **IC-10 — Typo `supress_error`** (one `s`) in `encoding_conversion` signature (`iconv.rs:362`). ✓ fixed (Phase 4): renamed to `suppress_error`.
- [x] **IC-11 — `from_ucs4` silent drop:** ✓ fixed (Phase 3, folded into the error model): the surrogate / out-of-range branches in `utf_8::from_ucs4` and `utf_16::from_ucs4` no longer return a silent `Some(vec![])`. With `!omit_invalid` they now set `had_error` (so exit `>0`) and report unless `-s`; with `-c` they omit. (`utf_16::from_ucs4` also had inverted omit/suppress logic, fixed here.)

## Conformance matrix

### Synopsis / Options / Operands
- [x] `-f`/`-t` (both charmap-file and codeset forms) CONFORMS — slash-detection dispatch (`iconv.rs:348-354`).
- [x] **`-c` CONFORMS** — exit-status interaction fixed (IC-1/IC-2, Phase 3): `-c` omits and succeeds; absence of `-c` on an unconvertible character exits `>0`.
- [x] **`-s` CONFORMS** (IC-4, Phase 3) — gates only the stderr message, not stream/exit. **`-l` CONFORMS** (IC-5, Phase 4) — prints round-trippable hyphen names.
- [x] `file...`, `-` → stdin, missing-operand → stdin CONFORMS (`iconv.rs:517-523`). **✓ verified** by the existing `-` tests.

### Environment variables
- [x] **`LC_ALL`/`LC_CTYPE`/`LANG` CONFORMS** for default codeset (IC-3, Phase 4) — resolved via `nl_langinfo(CODESET)`, which honors the full chain.
- [ ] **`LC_MESSAGES` PARTIAL** (IC-9, deferred to Phase 12). `NLSPATH` N/A (gettextrs equivalent).

### Conversion engine
- [x] ASCII ↔ UTF-8/16/32 (+ LE/BE) full matrix CONFORMS; UTF-16/32 **input** BOM handling CONFORMS (`utf_16.rs:46-65`, `utf_32.rs:44-63`).
- [x] **UTF-16/32 output BOM CONFORMS** (IC-6, Phase 4) — generic forms emit a leading U+FEFF; explicit LE/BE do not. No ISO-8859-x / locale-derived codesets (implementation-defined per spec, but a practical gap). Mixed charmap+codeset correctly rejected (`iconv.rs:534-540`).

### Exit status
- [x] **CONFORMS** (IC-1/IC-2/IC-4, Phase 3) — a conversion error now yields exit `>0` via the shared `had_error` flag; `-c`/`-s` no longer wrongly force exit 0. (Default-codeset error path is IC-3, Phase 4.)

## Test coverage — not covered
- [x] Exit status on unconvertible input (with/without `-c`/`-s`); `-l` round-trip (alias/case); generic UTF-16/32 output BOM — now covered by added integration tests (Phases 3–4). `LANG=C` default-codeset path verified manually (locale-dependent, not asserted in CI).

---

# `locale`

**Implementation:** `locale.rs` (315 lines)
**Library:** `locale_lib/{env,platform,types}.rs`
**Tests:** `tests/locale/mod.rs` (129 lines)
**Spec:** POSIX.1-2024 `locale` (extract `~/tmp/i18n-spec/locale.txt`)

## TL;DR
The no-operand default output, the `LC_ALL > LC_* > LANG` precedence, and `localeconv()`-backed numeric/monetary basics work. But `LC_TIME` and `LC_MESSAGES` keywords are **hardcoded constants** that ignore the active locale, ~30 required keywords are missing, numeric keywords are wrongly double-quoted, the reserved `charmap` operand is unhandled, and error paths exit 0.

## Priority issues

### Critical
- [x] **LO-1 — `LC_TIME`/`LC_MESSAGES` keyword values are hardcoded.** ✓ fixed (Phase 5): `d_t_fmt`/`d_fmt`/`t_fmt`/`yesexpr`/`noexpr` now read `nl_langinfo(D_T_FMT/D_FMT/T_FMT/YESEXPR/NOEXPR)` (falling back to the prior constants off-Linux/macOS). **Root-cause fix:** the C global locale was never actually switched — `gettextrs::setlocale(LcAll, "")` did not apply, so `localeconv`/`nl_langinfo` always saw the C locale. Added an explicit `libc::setlocale(LC_ALL, "")` (also in iconv, which makes IC-3 actually work). Verified `LC_ALL=en_US.UTF-8 locale d_fmt`→`%m/%d/%Y`, `yesexpr`→`^[+1yY]`, matching glibc. Fix: `nl_langinfo(D_FMT/T_FMT/D_T_FMT/YESEXPR/NOEXPR)`.
- [x] **LO-2 — Category operand without `-k` prints `CATEGORY=locale_name`** instead of one keyword value per line. ✓ fixed (Phase 5): `print_category_info` now iterates the category's exposed keywords and writes each value (`%s\n`) without `-k`, or `name="value"` pairs with `-k`. `LC_CTYPE`/`LC_COLLATE` (no exposed keywords) now produce no output instead of a malformed line. Diverges from STDOUT format items 2/3.

### Major
- [x] **LO-3 — Error paths exit 0.** ✓ fixed (Phase 5): an unknown name is diagnosed (`locale: unknown name "X"`) and `main` exits 1; the `charmap` operand is now handled (LO-4). **✓ verified:** `locale charmap` → `locale: unknown keyword: charmap`, **`exit 0`**; `locale frac_digits` → `unknown keyword`. Spec: `>0` on error. `locale.rs:183-188`.
- [x] **LO-4 — Reserved `charmap` / `code_set_name` operand unhandled.** ✓ fixed (Phase 5): a `charmap` operand prints `nl_langinfo(CODESET)` (value, or `charmap="..."` with `-k`). Fix: add a `charmap` arm via `nl_langinfo(CODESET)`.
- [x] **LO-5 — Numeric keywords double-quoted.** ✓ fixed (Phase 6): keyword values now carry a `KwVal` kind (`Str`/`List`/`Num`); numeric keywords render as `%s=%d` (unquoted), strings/lists as `%s="%s"`. `locale -k grouping`→`grouping=-1`. Spec: numeric keyword → `%s=%d` (unquoted).
- [x] **LO-6 — ~30 required keywords missing from dispatch.** ✓ fixed (Phase 6): all 21 POSIX `LC_MONETARY` keywords (incl. the `int_*` set) are read from `localeconv`, and `LC_TIME` `abday`/`day`/`abmon`/`mon`/`am_pm`/`t_fmt_ampm` from `nl_langinfo`. Verified byte-for-byte against glibc for the C and en_US.UTF-8 locales. `types.rs` defines the keyword arrays but the query path never consults them.
- [x] **LO-7 — `-a`/`-m` not mutually exclusive.** ✓ fixed (Phase 6): clap `conflicts_with_all` makes `-a` exclusive with `-m`/`-c`/`-k`/names, and `-m` exclusive with `-c`/`-k`/names (exit 2 on conflict).

### Minor
- [x] **LO-8 — `grouping` CHAR_MAX sentinel uses `< 127`.** ✓ fixed (Phase 6): the sentinel is compared against `libc::c_char::MAX` (correct on both signed- and unsigned-char platforms) in `char_num`/`grouping_str`, rendering "unspecified" as `-1`.
- [x] **LO-9 — No escaping** of `;`, `\`, `"`, control chars in `-k` compound values. ✓ fixed (Phase 6): `escape_value` escapes them; for compound (`List`) values each element is escaped independently so the `;` separators are preserved. Unit-tested.
- [x] **LO-10 — `-a` self-delegates** to the system `locale -a` when a `locale-archive` exists; non-Linux `-m` returns a fabricated list. ✓ addressed (Phase 6): the archive enumeration now invokes the system `locale` via an absolute path (`/usr/bin/locale`, `/bin/locale`), never a `$PATH` lookup, so it can never recurse into our own binary. (Parsing the locale-archive binary directly remains out of scope; the non-Linux `-m` fallback list is retained as a last resort.)
- [x] **LO-11 — `from_name` is case-insensitive** for category names. ✓ fixed (Phase 6): `from_name` now matches exact (upper) case, so a lowercase `lc_numeric` is treated as a (keyword) name, not a category.

## Conformance matrix

### Options / Operands
- [x] **`-a`/`-m` CONFORMS** (LO-7, Phase 6 — mutually exclusive); **`-c`/`-k` CONFORMS** (LO-5, Phase 6 — numeric unquoted).
- [x] No-operand default output (LANG/LC_*/LC_ALL with correct implied-vs-set quoting) CONFORMS (`locale.rs:105-124`, `env.rs:83-106`).
- [x] **category operand CONFORMS** (LO-2, Phase 5); **`charmap` operand CONFORMS** (LO-4, Phase 5).

### Environment variables
- [x] `LANG`/`LC_ALL`/`LC_CTYPE`/`LC_MESSAGES` + precedence CONFORMS (`env.rs:41-80`). `NLSPATH` N/A.

### Output format
- [x] `-c` category header, no-`-k` value-only format CONFORMS (`locale.rs:93-95`, `:198-200`).
- [x] **`-k` numeric quoting / escaping CONFORMS** (LO-5/LO-9, Phase 6).

### Keyword coverage / exit status
- [x] `decimal_point`, `thousands_sep`, `currency_symbol`, `int_curr_symbol`, `mon_decimal_point` CONFORMS (via `localeconv()`).
- [x] **LC_TIME/LC_MESSAGES now live** (LO-1, Phase 5 via nl_langinfo); **full POSIX LC_MONETARY + LC_TIME keyword set** (LO-6, Phase 6).
- [x] **error exit status CONFORMS** (LO-3, Phase 5) — unknown name exits 1.

## Test coverage — not covered
- [x] `charmap` operand; error exit code; live `LC_TIME`; full `LC_MONETARY` keyword set; numeric-keyword quoting; `-k` escaping — now covered by added integration tests (LC_ALL=C) and `escape_value`/`KwVal` unit tests (Phases 5–6).

---

# `localedef`

**Implementation:** `localedef.rs` (368 lines)
**Library:** `locale_lib/{env,platform,types}.rs`
**Tests:** `tests/localedef/mod.rs` (157 lines)
**Spec:** POSIX.1-2024 `localedef` (extract `~/tmp/i18n-spec/localedef.txt`)

## TL;DR
**Phase 11 (bounded) status:** the parser and CLI behavior are now spec-conformant — `-f charmap` is opened/validated, `LC_CTYPE`/`LC_COLLATE` symbolic references are parsed and checked against the charmap, `escape_char`/`comment_char`/line-continuation and `copy`/`include` directives are honored, the full POSIX keyword set is recognized, the 0/1/2/>3 exit-status table is implemented, and successfully processed categories are reported on stdout. **Deferred (LD-1):** `localedef` still writes a marker file rather than a real libc-consumable compiled locale; emitting a true compiled-locale binary (or delegating to host `localedef`) is out of scope for this pass, so `localedef` remains at README "Stage 3 — Test coverage".

Originally: `localedef` did not produce a usable locale — wrote a one-line `LC_IDENTIFICATION` marker, never opened the `-f charmap`, never parsed `LC_CTYPE`/`LC_COLLATE`, ignored `-u`, didn't resolve `copy`/`include`, mishandled `escape_char`/`comment_char` and line continuation, and collapsed the 0/1/2/3/>3 exit-status table to 0/1/4.

## Priority issues

### Critical
- [ ] **LD-1 — No usable locale is produced.** _(DEFERRED — see decision note below.)_ `write_locale` writes only `…/LC_IDENTIFICATION`; the parsed category data is not compiled to a libc-consumable binary. Producing a real compiled-locale writer (or delegating to host `localedef`) is out of scope for this audit pass; `localedef` therefore stays at README "Stage 3 — Test coverage". Fix: emit a real compiled locale or delegate to host `localedef`.
- [x] **LD-2 — `-f charmap` never read.** ✓ fixed (Phase 11, parsing): `parse_charmap_symbols` opens and validates the charmap; an unreadable charmap exits 2 (unsupported charset, no output). The symbolic names are parsed and used to validate LC_CTYPE/LC_COLLATE references. _(The charmap-driven encoding *compilation* remains part of the deferred LD-1.)_
- [x] **LD-3 — `LC_CTYPE` and `LC_COLLATE` entirely unparsed.** ✓ fixed (Phase 11, parsing): both categories are now parsed; their `<symbolic>` references are collected and validated against the charmap, emitting a warning (per spec, for these two categories) when a symbol is absent. _(Full classification/collation *compilation* is part of the deferred LD-1.)_

### Major
- [x] **LD-4 — `-c` overrides hard errors.** ✓ fixed (Phase 11): output is created only when `!has_errors`; `-c` now gates warnings only, never errors.
- [x] **LD-5 — Exit codes 2 and 3 never returned.** ✓ fixed (Phase 11): 0 = created/no warnings, 1 = created/warnings (`-c`), 2 = unsupported charset (unreadable charmap), no output, and > 3 (4) = errors — or warnings without `-c` — with no output. (3 "creation unsupported" is intentionally not used while a marker locale is still written; that relates to the deferred LD-1.)
- [x] **LD-6 — `escape_char`/`comment_char` directives ignored.** ✓ fixed (Phase 11): both directives are honored, and a line ending in the active `escape_char` is joined with the next (line continuation).
- [x] **LD-7 — `copy` not resolved; `include` not recognized.** ✓ fixed (Phase 11): `copy` is recorded for every category, and `include "file"` is recognized and inlined (depth-guarded; an unreadable include is an error). _(Resolving a `copy` against an already-installed locale needs the compiled-locale store from the deferred LD-1.)_
- [x] **LD-8 — `-u code_set_name` parsed but unused.** ✓ acceptable (Phase 11): the codeset only affects the deferred binary-encoding step (LD-1); it is accepted and otherwise inert, which is harmless for the parsing/validation this pass covers.
- [x] **LD-9 — No stdout success report.** ✓ fixed (Phase 11): on success the processed category names are written to standard output.

### Minor
- [x] **LD-10 — `LC_MONETARY` missing 9 keywords, `LC_TIME` missing 11.** ✓ fixed (Phase 11): keyword recognition is driven by the full POSIX keyword lists (`locale_lib::types::LC_*_KEYWORDS`); the per-keyword typed fields (and their dead-code underscores) are gone, and an unrecognized keyword is warned about.
- [x] **LD-11 — Warnings suppressed unless `-v`.** ✓ fixed (Phase 11): all diagnostics (warnings included) are written to standard error regardless of `-v`.
- [x] **LD-12 — `END <category>` not validated.** ✓ fixed (Phase 11): `END x` must match the open category (mismatch is an error), and a section header must be one of the known `LC_*` category names.
- [x] **LD-13 — Non-slash `name` writes to `$TMPDIR/locale/<name>`.** ✓ acceptable (Phase 11): the spec makes a non-slash `name` "interpreted in an implementation-defined manner"; the chosen location is implementation-defined and conformant. The "public" placement depends on the deferred compiled-locale writer (LD-1).

## Conformance matrix

### Options / Operands
- [x] `-i sourcefile` CONFORMS (`localedef.rs:38-40`, `:197-216`).
- [x] **`-f` parsed+validated** (LD-2, Phase 11); **`-u` accepted** (LD-8, inert pending LD-1); **`-c` CONFORMS** (LD-4, Phase 11); `-v` is a non-POSIX extension (benign).
- [x] **`name` implementation-defined** (LD-13, acceptable).

### Input files (category grammar)
- [x] **charmap parsed+validated** (LD-2, Phase 11); **LC_CTYPE/LC_COLLATE parsed** (LD-3, Phase 11). _(Encoding/collation compilation deferred with LD-1.)_
- [x] **LC_MONETARY/LC_TIME/LC_NUMERIC/LC_MESSAGES keyword recognition complete** (LD-10, Phase 11 via `LC_*_KEYWORDS`); category data parsed but not yet compiled (LD-1).
- [x] **`copy` recorded / `include` inlined** (LD-7, Phase 11); **`escape_char`/`comment_char`/continuation CONFORMS** (LD-6, Phase 11).

### Environment variables
- [x] `LC_COLLATE`/`LC_CTYPE` "POSIX locale used regardless" — CONFORMS in effect (parser is byte-oriented); `LANG`/`LC_ALL`/`LC_MESSAGES` honored via `setlocale`. `NLSPATH` N/A.

### Exit status / errors / stdout
- [x] **exit table CONFORMS** (LD-5, Phase 11 — 0/1/2/>3); **`-c` over errors CONFORMS** (LD-4, Phase 11); **stdout report present** (LD-9, Phase 11).

## Test coverage — not covered
- [x] charmap-unreadable exit 2; LC_CTYPE/LC_COLLATE symbol validation; mismatched-`END` error; unknown-keyword warning; stdout category report — now covered by added integration tests (Phase 11). A check that the output is a *usable* compiled locale remains out of scope (deferred LD-1).

---

# `msgfmt`

**Implementation:** `msgfmt.rs` (410 lines)
**Library:** `gettext_lib/{po_file,mo_file,plural,catalog}.rs`
**Tests:** `tests/msgfmt/mod.rs` (194 lines)
**Spec:** POSIX.1-2024 `msgfmt` (extract `~/tmp/i18n-spec/msgfmt.txt`)

## TL;DR
Single-domain `.po` → `.mo` compilation, multi-line string concatenation, fuzzy skipping, and `-D`/`-o` work. But `domain` directives are silently dropped, so multi-domain `.po` files are merged into one output named after the *input file*, not the per-domain `.mo` files the spec requires; `-S` has a dead-code bug; and `-c`/`-v` validation diagnostics are emitted as warnings that never affect the exit status.

## Priority issues

### Critical
- [x] **MF-1 — `domain` directives ignored → wrong output files.** ✓ fixed (Phase 7): the parser now recognizes `domain domainname` directives, tags each `PoEntry` with its domain (default `None` = `messages`), and msgfmt accumulates messages per domain. Without `-o`, one messages object file is written per domain, named after the domain; with `-o`, all domains are merged into the single named file (directives ignored, per spec). Verified: a `.po` with `domain "other"` now yields `messages`+`other` (or `messages.mo`+`other.mo` with `-S`). Spec: each domain section → `domainname.mo`. Fix: accumulate per-domain and write per-domain.
- [x] **MF-2 — `-c`/`-v` checks never affect exit status.** ✓ fixed (Phase 8): genuine abnormalities (boundary-newline and c-format mismatches) are now recorded with `is_error: true`, so `has_errors` is set and the process exits 1. Spec: "If an abnormality is detected, the exit status shall be non-zero."

### Major
- [x] **MF-3 — `-S` is dead code.** ✓ fixed (Phase 7): the buggy `set_extension`-then-guard logic is gone. Output naming is now domain-based; `apply_suffix` appends `.mo` only when `-S` is set and the name does not already end in `.mo`. Without `-S` the bare domain name is used (POSIX leaves this implementation-defined), so `-S` has an observable effect. It also applies to the `-o` filename. Output naming is input-filename-based, not domain-based (see MF-1).
- [x] **MF-4 — `-c` runs without `-v`.** ✓ fixed (Phase 8): the abnormality checks run only when both `-c` and `-v` are given (`run_checks = args.check && args.verbose`); with just one the behavior is a no-op, per spec.
- [x] **MF-5 — Newline check is wrong.** ✓ fixed (Phase 8): `boundary_newline_mismatch` flags an abnormality only when exactly one of the strings starts with `\n`, or exactly one ends with `\n` — not on internal-newline count differences.
- [x] **MF-6 — Missing C escapes in the `.po` parser.** ✓ fixed (Phase 8): `\a`, `\b`, `\f`, `\v`, `\ooo` octal, and `\xhh` hex are decoded; an unknown escape is now a parse error (`InvalidEscape`) instead of being passed through.
- [x] **MF-7 — Header `charset` only read from `Content-Type:`.** ✓ fixed (Phase 8): `charset()` also recognizes the bare `charset=...` header line.
- [x] **MF-8 — `no-c-format` flag ignored.** ✓ fixed (Phase 8): `is_c_format` implements the last-flag-wins rule between `c-format` and `no-c-format`.

### Minor
- [x] **MF-9 — c-format check is count-only.** ✓ fixed (Phase 8): `format_signatures` compares the ordered list of conversion specifiers by length-modifier + argument-type class, so it catches both a differing count and differing types (`%s` vs `%d`) while treating equivalents (`%d`/`%i`) as the same.
- [x] **MF-10 — `truncate` slices bytes.** ✓ fixed (Phase 8): `truncate` now counts and takes `chars`, so it never splits a multibyte boundary.
- [x] **MF-11 — `-v` alone prints no statistics.** ✓ fixed (Phase 8): `-v` prints `N translated messages[, M fuzzy translations][, K untranslated messages].`
- [x] **MF-12 — zero operands → clap error.** ✓ fixed (Phase 8): `files` is no longer `required`; with no operand msgfmt prints `msgfmt: no input file given` and exits 1.

## Conformance matrix

### Options / Operands
- [x] `-f` (fuzzy skip), `-D` (dir search) CONFORMS (`msgfmt.rs:37-47`, `:203-217`).
- [x] **`-S` CONFORMS** (MF-3, Phase 7); **`-o` CONFORMS** (MF-1, Phase 7 — merges domains into one file). **`-c`/`-v` CONFORMS** (MF-2/MF-4, Phase 8 — checks gated on `-c -v`, abnormalities set exit status).
- [x] `pathname...` multiple operands CONFORMS (`msgfmt.rs:102`); STDIN "Not used" (no `-` form) consistent with spec.

### Input files (.po grammar)
- [x] `msgid`/`msgstr`/`msgid_plural`/`msgstr[n]`, multi-line concatenation, `#:`/`#.`/`#,`/`#` comments, fuzzy flag CONFORMS (`po_file.rs:232-336`).
- [x] **`domain` CONFORMS** (MF-1, Phase 7); **C escapes CONFORMS** (MF-6, Phase 8); **header charset CONFORMS** (MF-7, Phase 8); **`no-c-format` CONFORMS** (MF-8, Phase 8). `#~` obsolete handling partial.

### Env / stdout / stderr / output files / exit
- [ ] **`LC_CTYPE` PARTIAL** (input assumed UTF-8); `NLSPATH`/`LANGUAGE` N/A for the compiler.
- [x] STDOUT not used / STDERR diagnostics CONFORMS; `.mo` binary format N/A (unspecified).
- [x] **default output naming CONFORMS** (MF-1/MF-3, Phase 7 — per-domain `domainname[.mo]`); **exit status CONFORMS** (MF-2, Phase 8).

## Test coverage — not covered
- [x] Multi-domain `.po`; `-c -v` exit status; `-c`-without-`-v` no-op; `\xhh`/`\ooo`/control escapes + unknown-escape error; bare `charset=` header; zero-operand diagnostic — now covered by added msgfmt integration tests and po_file unit tests (Phases 7–8).

---

# `xgettext`

**Implementation:** `xgettext.rs` (1370 lines)
**Library:** `gettext_lib/{po_file,catalog}.rs`
**Tests:** `tests/xgettext/mod.rs` (192 lines)
**Spec:** POSIX.1-2024 `xgettext` (DEVELOPMENT; extract `~/tmp/i18n-spec/xgettext.txt`)

## TL;DR
The C-source extraction path (via the in-tree `posixutils-cc` parser), keyword-spec parsing, `-n` location comments, and singular/plural `.po` emission work. But the output file is `messages.pot` not the spec's `messages.po`, the exclude option is `-X` not `-x`, output is sorted alphabetically instead of in extraction order, `-j` does no deduplication, and the tool also parses Rust (a deliberate extension beyond the spec's "C-language source files").

## Priority issues

### Major
- [x] **XG-1 — Output file is `.pot`, not `.po`.** ✓ fixed (Phase 10): the default output is now `{default-domain}.po`. The repo `Makefile` was updated to match (template `locale/${PROJECT_NAME}.po`, and `POS` scoped to `*/LC_MESSAGES/*.po` so the generated template is not mistaken for a translation).
- [x] **XG-2 — Exclude option is `-X`, not `-x`.** ✓ fixed (Phase 10): `short = 'x'`; `-X` is no longer accepted.
- [x] **XG-3 — Output sorted alphabetically, not extraction order.** ✓ fixed (Phase 10): `Walker` records keys in first-extracted order (`order: Vec<MessageKey>`) and `render()` emits in that order. Verified: `test_clap` output is now in monotonic source order.
- [x] **XG-4 — `-j` does not deduplicate.** ✓ fixed (Phase 10): `parse_existing_keys` collects the `(msgctxt, msgid)` keys already in the target file, and `render(exclude)` omits newly extracted duplicates; the existing content is preserved ahead of the new entries.
- [x] **XG-5 — Non-`.c`/`.h`/`.rs` operands rejected.** ✓ fixed (Phase 10): only `.rs` selects the Rust path; every other operand (including a C file with no extension) is processed as C source. Verified with a no-extension fixture.
- [x] **XG-6 — `_l` keyword variants absent from defaults.** ✓ fixed (Phase 10): `gettext_l`/`ngettext_l`/`dgettext_l`/`dngettext_l`/`dcgettext_l`/`dcngettext_l` are added (mirroring their base forms' argument positions).

### Minor
- [x] **XG-7 — Rust source parsing is a spec extension.** ✓ documented (Phase 10): a comment at the operand dispatch records that `.rs` is a posixutils extension beyond "C-language source files," used for this project's own build tooling.
- [x] **XG-8 — `domain` directive not written** to the output file. ✓ acceptable (Phase 10): xgettext writes only the default output file here, and the spec makes the leading `domain` directive optional for the default output file. (Multi-domain output files are not produced.)
- [x] **XG-9 — Plural output is always `msgstr[0]`/`msgstr[1]`.** ✓ acceptable (Phase 10): a `.po` template legitimately emits two empty plural forms; the target plural count is filled in by the translator/`msgfmt`.
- [ ] **XG-10 — File-I/O errors propagate as raw Rust `Err`** (not `gettext()`-wrapped), so they are unlocalized; exit code is still `>0` (correct). _(Deferred to Phase 12: crate-wide gettext diagnostics sweep.)_
- [x] **XG-11 — `-K` default mechanism is fragile.** ✓ fixed (Phase 10): the clap `default_value = "gettext"` injection was removed; `KeywordSpec::defaults()` is the single source of the default keyword set, and `-K ""` cleanly disables it.

## Conformance matrix

### Synopsis / Options / Operands
- [x] `-a`, `-d`, `-n`, `-p`, `-K` (all four spec forms) CONFORMS (`xgettext.rs:43-91`, `:137-164`).
- [x] **`-x` CONFORMS** (XG-2, Phase 10); **`-j` CONFORMS** (XG-4, Phase 10 — dedups against the existing file).
- [x] `file...` and `-` (stdin as C) CONFORMS (`xgettext.rs:108-113`, `:1054-1062`).
- [x] **operand acceptance CONFORMS** (XG-5, Phase 10 — non-`.rs` treated as C by content, not extension).

### Input files / keyword recognition
- [x] C extraction via `posixutils-cc` AST CONFORMS; `gettext`/`ngettext`/`dgettext`/`dngettext`/`dcgettext`/`dcngettext` recognized.
- [x] **`_l` variants CONFORMS** (XG-6, Phase 10); **Rust path is a documented extension** (XG-7); wide `L"…"` literal handling unverified.

### Output files / .po content / exit
- [x] **`.po` output CONFORMS** (XG-1, Phase 10); **extraction-order CONFORMS** (XG-3, Phase 10); **`domain` directive optional-for-default OK** (XG-8).
- [x] `msgid`/`msgid_plural`/`msgstr`/`msgstr[n]`, `#:` refs (`-n`) CONFORMS (`xgettext.rs:919-938`).
- [x] STDOUT not used / STDERR diagnostics CONFORMS; exit `0`/`>0` CONFORMS (`xgettext.rs:1031-1118`).

### Environment variables
- [x] `LANG`/`LANGUAGE`/`LC_*`/`NLSPATH` handled implicitly via `setlocale`/`textdomain` (`xgettext.rs:1019-1024`) — adequate for an extraction tool.

## Test coverage — not covered
- [x] `.po` output name; extraction-order output; no-extension C operand; `_l` keywords — now covered (Phase 10: golden fixtures updated to `.po`/extraction order, no-extension fixture + `_l` unit test). `-j` dedup, adjacent C literal concatenation, and wide `L"…"` literals remain exercised only manually.

---

# Suggested PR groupings

Small, themed PRs land more easily than one mega-PR. Roughly in priority order:

- **PR A — "gencat grammar + crash"** (Critical): GC-1 (escapes), GC-2 (panic→error), GC-3 (continuation), GC-4 (collision-replace), GC-6 (delete-by-number). One cohesive rewrite of the message-text parser + the catalog-merge path. Add fixtures for each.
- **PR B — "gencat operands & directives"** (Major): GC-5 (`msgfile...`), GC-7 (`$delset`), GC-8 (msgid range/order).
- **PR C — "iconv error model"** (Critical): IC-1/IC-2/IC-4 — thread a `had_error` flag out of `iconv_lib`, drop all in-library `exit()`/`return None`-as-error, exit `>0` correctly. The single highest-leverage iconv fix.
- **PR D — "iconv codeset & naming"** (Major): IC-3 (`nl_langinfo(CODESET)`), IC-5 (`-l` hyphen names + alias/case-folding), IC-6 (output BOM).
- **PR E — "locale live values"** (Critical/Major): LO-1 (`nl_langinfo` for LC_TIME/LC_MESSAGES), LO-4 (`charmap` operand), LO-3 (error exit), LO-2 (category-operand format).
- **PR F — "locale keyword coverage"** (Major): LO-5 (numeric quoting), LO-6 (missing LC_MONETARY/LC_TIME keywords), LO-7 (`-a`/`-m` exclusivity), LO-8 (CHAR_MAX sentinel).
- **PR G — "msgfmt domains"** (Critical): MF-1 (per-domain accumulation + output naming), MF-3 (`-S`). Parse and honor `domain` directives end-to-end.
- **PR H — "msgfmt checks"** (Major): MF-2 (exit status), MF-4 (`-c -v` gating), MF-5 (newline check), MF-6 (C escapes in po_file), MF-7 (bare charset), MF-8 (`no-c-format`).
- **PR I — "gettext/ngettext spec compliance"** (Major): GT-1 (`-E`), GT-2/GT-10 (newline + tests), NG-1 (`[textdomain]` operand), GT-3 (NLSPATH), GT-5 (mo_file plural index).
- **PR J — "xgettext output conformance"** (Major): XG-1 (`.po`), XG-2 (`-x`), XG-3 (extraction order), XG-4 (`-j` dedup), XG-5 (operand extension), XG-6 (`_l` keywords).
- **PR K — "localedef: real locale output"** (Critical, large): LD-1/LD-2/LD-3 — the big one; either implement a compiled-locale writer + charmap/LC_CTYPE/LC_COLLATE parsing, or delegate to host `localedef`. Likely several sub-PRs.
- **PR L — "localedef exit codes & directives"** (Major): LD-4 (`-c` over errors), LD-5 (exit 2/3), LD-6 (escape_char/comment_char/continuation), LD-7 (`copy`/`include`), LD-9 (stdout report).
- **PR M — "crate-wide i18n diagnostics"** (Minor): wrap runtime diagnostics in `gettext()` across all eight utilities (theme 2); honor `NLSPATH`/`LANGUAGE` (theme 3).
