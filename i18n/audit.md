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
- [ ] **GT-1 — `-E` option missing from both utilities.** Spec: "`-E` Do not process C-language escape sequences," and "if `-s` … is specified then `-E` shall be the default." `Args` has no `-E` (`gettext.rs:28-49`, `ngettext.rs:28-50`); the `-e`/`-E` mutual-exclusion and the `-s`→`-E` default cannot be honored.
- [ ] **GT-2 — Spurious trailing `<newline>` outside `-s`.** APPLICATION USAGE (spec lines 99929-99930): "unless `-s` … is specified without `-n`, the message(s) … are not followed by a `<newline>`." Both use `println!` unconditionally (`gettext.rs:115`, `ngettext.rs:99`). **✓ verified:** `gettext Hello` → bytes `48 65 6c 6c 6f 0a`; `ngettext file files 1` → `…0a`. Fix: `print!` in the non-`-s` paths.
- [ ] **NG-1 — `ngettext` missing the optional `[textdomain]` operand.** Synopsis is `[textdomain] msgid msgid_plural n`; clap declares exactly three positionals (`ngettext.rs:43-49`). **✓ verified:** `ngettext mail recipient recipients 1` (the spec's own example shape) → `error: unexpected argument '1' found`.
- [ ] **GT-3 — `NLSPATH` not implemented** and must take precedence over `TEXTDOMAINDIR` (`gettext_lib/lookup.rs:55-71`).
- [ ] **GT-4 — NULL-textdomain path diverges.** When no domain can be resolved the spec returns the msgid directly; instead a catalog lookup is run against a fabricated `"messages"` domain (`gettext.rs:78-79`, `ngettext.rs:75-76`). Usually the same result, wrong code path.
- [ ] **GT-5 — `mo_file.rs::get_plural_index` ignores the parsed plural expression** and always applies the Germanic `n==1` rule (`gettext_lib/mo_file.rs:365-382`). Only reachable via the `MoFile` API directly (binaries route through `catalog.rs`, which is correct), so the binaries are presently safe — but the library is broken for Slavic/Arabic plural counts.

### Minor
- [ ] **GT-6 — `LANGUAGE` (XSI) not consulted** in locale resolution (`gettext_lib/lookup.rs:83-107`).
- [ ] **GT-7 — gettext non-`-s` mode accepts multiple msgids** and iterates them; spec's non-`-s` form has exactly one msgid (`gettext.rs:82-116`).
- [ ] **GT-8 — Octal escape requires a leading zero** (`\0NNN` only, not `\NNN`); `\0`/`\x00` silently dropped (`gettext_lib/lookup.rs:338-376`).
- [ ] **GT-9 — `-e` not applied to the fallback (catalog-miss) msgid** before output (`gettext.rs:100-106`, `ngettext.rs:84-97`).
- [ ] **GT-10 — Existing tests encode the non-conformant trailing newline** (`tests/gettext/mod.rs`, `tests/ngettext/mod.rs`) and must change alongside GT-2.

## Conformance matrix

### Options
- [x] `-d`, `-n`, `-s` (gettext) CONFORMS (`gettext.rs:29-39`).
- [x] `-d` (ngettext) CONFORMS; `-n`/`-s` correctly absent for ngettext.
- [ ] **`-E` MISSING** (GT-1); **`-e` PARTIAL** (GT-8, GT-9).

### Operands / output
- [x] gettext `[textdomain] msgid` disambiguation CONFORMS for ≤1 domain (`gettext.rs:68-91`); multi-msgid divergence (GT-7).
- [ ] **ngettext `[textdomain]` MISSING** (NG-1); count parsed via Rust `parse::<u64>` vs `strtoul` (negligible).
- [ ] **trailing newline DIVERGES** (GT-2); `-s` space-separation CONFORMS (`gettext.rs:120-150`).

### Environment variables
- [x] `LC_ALL`/`LC_MESSAGES`/`LANG` precedence CONFORMS (`lookup.rs:83-107`).
- [x] `TEXTDOMAIN` CONFORMS; **`TEXTDOMAINDIR` PARTIAL** (no NLSPATH precedence).
- [ ] **`NLSPATH` MISSING** (GT-3); **`LANGUAGE` MISSING** (GT-6).

### Plural / exit status
- [x] Plural-expression parse+evaluate via `plural.rs`/`catalog.rs` CONFORMS.
- [ ] **`mo_file.rs` plural index DIVERGES** (GT-5).
- [x] Exit `0`/`>0` CONFORMS (`gettext.rs:70`, `ngettext.rs:65`).

## Test coverage — not covered
- [ ] `-E`; `-s` default-`-E`; ngettext `[textdomain]` operand; real `.mo` translation lookup; non-Germanic plural counts.

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
- [ ] **IC-1 — Exit status 0 after a conversion/codeset error.** `main()` returns `Ok(())` even when conversion failed. **✓ verified:** `printf A | LANG=C iconv` → `Error: Could not find a codeset from your locale`, **`exit 0`**. Also the whole `charmap_conversion` path reports via `eprintln!` and returns `()` (`iconv.rs:416-480`), and `-s` error paths `return None` (`ascii.rs:31`, `utf_16.rs:84-106`, `utf_32.rs:75-82`) → exit 0. Spec: ">0 An error occurred," and "absence of `-c`/`-s` shall not affect the exit status." Fix: thread a `had_error` flag to `main` and exit `>0`.
- [ ] **IC-2 — `exit(1)` called from inside library iterators** (`ascii.rs:29`, `utf_8.rs:48-178`), aborting mid-stream and bypassing remaining file operands. Fix: return `Result`, propagate to `main`.

### Major
- [ ] **IC-3 — Default codeset reads only `LANG`, splitting on `.`.** Ignores the `LC_ALL > LC_CTYPE > LANG` chain; `LANG=C`, `LANG=POSIX`, `LANG=en_US` (no `.`), or unset all hit the error/exit branch (`iconv.rs:494-512`). **✓ verified** (see IC-1). Fix: `nl_langinfo(CODESET)` after `setlocale`.
- [ ] **IC-4 — `-s` truncates the stream.** Spec: `-s` "Suppress any messages" (stderr only). The impl uses `return None` as a stream terminator when `-s` is set without `-c` (`ascii.rs:31`, `utf_16.rs:84-106`). Fix: continue after a suppressed error; never use iteration end as an error signal.
- [ ] **IC-5 — `-l` lists names the parser rejects.** **✓ verified:** `iconv -l` prints `UTF_8`, `UTF_16LE` (underscores, Debug format) but `-f`/`-t` require `UTF-8`/`UTF-16LE` (hyphens), and names are case-sensitive (`iconv -f utf-8 …` → `Error: Unknown encoding: utf-8`). `iconv.rs:178-181`, `:165-175`. Fix: print the serialized (hyphen) form; normalize/upper-case input and accept common aliases.
- [ ] **IC-6 — Generic `UTF-16`/`UTF-32` output writes no BOM** (`utf_16.rs:143-151`, `utf_32.rs:98-108`), producing endianness-ambiguous output. Fix: emit a leading U+FEFF for the non-LE/BE variants.

### Minor
- [ ] **IC-7 — Charmap `mb_cur_max` defaults to 0** (`iconv.rs:186`), so the flush guard `len >= mb_cur_max` is always true → premature flush per byte. Fix: default 1 / require the field.
- [ ] **IC-8 — Per-byte stdout flush** in the encoding path (`iconv.rs:410-413`) — severe perf, not conformance. Use `BufWriter`.
- [ ] **IC-9 — Hardcoded English diagnostics** in `iconv_lib` (`ascii.rs:28`, etc.); `LC_MESSAGES` inert.
- [ ] **IC-10 — Typo `supress_error`** (one `s`) in `encoding_conversion` signature (`iconv.rs:362`).
- [ ] **IC-11 — `from_ucs4` silent drop:** a surrogate with `!omit_invalid` returns `Some(vec![])` (`utf_8.rs:207-210`) — neither omitted nor an error.

## Conformance matrix

### Synopsis / Options / Operands
- [x] `-f`/`-t` (both charmap-file and codeset forms) CONFORMS — slash-detection dispatch (`iconv.rs:348-354`).
- [ ] **`-c` PARTIAL** — wired, but exit-status interaction wrong (IC-1/IC-2).
- [ ] **`-s` DIVERGES** (IC-4); **`-l` PARTIAL** (IC-5).
- [x] `file...`, `-` → stdin, missing-operand → stdin CONFORMS (`iconv.rs:517-523`). **✓ verified** by the existing `-` tests.

### Environment variables
- [ ] **`LC_CTYPE`/`LC_ALL` MISSING** for default codeset (IC-3); **`LANG` PARTIAL**.
- [ ] **`LC_MESSAGES` PARTIAL** (IC-9). `NLSPATH` N/A (gettextrs equivalent).

### Conversion engine
- [x] ASCII ↔ UTF-8/16/32 (+ LE/BE) full matrix CONFORMS; UTF-16/32 **input** BOM handling CONFORMS (`utf_16.rs:46-65`, `utf_32.rs:44-63`).
- [ ] **UTF-16/32 output BOM PARTIAL** (IC-6). No ISO-8859-x / locale-derived codesets (implementation-defined per spec, but a practical gap). Mixed charmap+codeset correctly rejected (`iconv.rs:534-540`).

### Exit status
- [ ] **DIVERGES — exit 0 after error** (IC-1/IC-2/IC-4).

## Test coverage — not covered
- [ ] Exit status on unconvertible input (with/without `-c`/`-s`); `LANG=C`/unset default codeset; `-l` round-trip; generic UTF-16/32 output BOM.

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
- [ ] **LO-1 — `LC_TIME`/`LC_MESSAGES` keyword values are hardcoded.** `d_t_fmt`/`d_fmt`/`t_fmt` (`locale.rs:296-307`) and `yesexpr`/`noexpr` (`:309-315`) return fixed strings regardless of locale. **✓ verified:** `LC_ALL=C locale yesexpr` and `LC_ALL=fr_FR.UTF-8 locale yesexpr` both print `^[yY]`; `d_fmt` is `%m/%d/%y` regardless. Fix: `nl_langinfo(D_FMT/T_FMT/D_T_FMT/YESEXPR/NOEXPR)`.
- [ ] **LO-2 — Category operand without `-k` prints `CATEGORY=locale_name`** instead of one keyword value per line (`locale.rs:152-159`). Diverges from STDOUT format items 2/3.

### Major
- [ ] **LO-3 — Error paths exit 0.** Unknown keyword and the `charmap` operand print to stderr and fall through to an implicit exit 0. **✓ verified:** `locale charmap` → `locale: unknown keyword: charmap`, **`exit 0`**; `locale frac_digits` → `unknown keyword`. Spec: `>0` on error. `locale.rs:183-188`.
- [ ] **LO-4 — Reserved `charmap` / `code_set_name` operand unhandled** (`locale.rs:164-188`) — falls into the unknown-keyword arm (LO-3). Fix: add a `charmap` arm via `nl_langinfo(CODESET)`.
- [ ] **LO-5 — Numeric keywords double-quoted.** Spec: numeric keyword → `%s=%d` (unquoted). All keywords use `%s="%s"` (`locale.rs:204-206`). **✓ verified:** `locale -k grouping` → `grouping="-1"` (quoted, and the `-1` is a CHAR_MAX-sentinel bug, see LO-8).
- [ ] **LO-6 — ~30 required keywords missing from dispatch.** `LC_MONETARY` exposes only 3 of ~24 (missing `mon_thousands_sep`, `mon_grouping`, `positive_sign`, `negative_sign`, `int_frac_digits`, `frac_digits`, `p_cs_precedes`, `p_sep_by_space`, `n_cs_precedes`, `n_sep_by_space`, `p_sign_posn`, `n_sign_posn`, and the `int_*` POSIX.1-2008 set); `LC_TIME` missing `abday`/`day`/`abmon`/`mon`/`am_pm`/`t_fmt_ampm`. **✓ verified:** `locale -k frac_digits` → `unknown keyword`. `types.rs` defines the keyword arrays but the query path never consults them.
- [ ] **LO-7 — `-a`/`-m` not mutually exclusive** and not exclusive with the `[-ck] name…` form; `-a` silently wins (`locale.rs:32-37`). Add clap `conflicts_with`.

### Minor
- [ ] **LO-8 — `grouping` CHAR_MAX sentinel uses `< 127`** instead of `== CHAR_MAX` (`locale.rs:245`), surfacing `-1` (see LO-5).
- [ ] **LO-9 — No escaping** of `;`, `\`, `"`, control chars in `-k` compound values (`locale.rs:204-206`).
- [ ] **LO-10 — `-a` self-delegates** to the system `locale -a` when a `locale-archive` exists (`platform.rs:88-99`); non-Linux `-m` returns a fabricated list (`platform.rs:127-131`).
- [ ] **LO-11 — `from_name` is case-insensitive** for category names (`types.rs:58`); spec expects exact case (harmless superset).

## Conformance matrix

### Options / Operands
- [ ] **`-a`/`-m` PARTIAL** (LO-7); **`-c`/`-k` PARTIAL** (LO-5).
- [x] No-operand default output (LANG/LC_*/LC_ALL with correct implied-vs-set quoting) CONFORMS (`locale.rs:105-124`, `env.rs:83-106`).
- [ ] **category operand DIVERGES** (LO-2); **`charmap` operand MISSING** (LO-4).

### Environment variables
- [x] `LANG`/`LC_ALL`/`LC_CTYPE`/`LC_MESSAGES` + precedence CONFORMS (`env.rs:41-80`). `NLSPATH` N/A.

### Output format
- [x] `-c` category header, no-`-k` value-only format CONFORMS (`locale.rs:93-95`, `:198-200`).
- [ ] **`-k` numeric quoting / escaping DIVERGES** (LO-5/LO-9).

### Keyword coverage / exit status
- [x] `decimal_point`, `thousands_sep`, `currency_symbol`, `int_curr_symbol`, `mon_decimal_point` CONFORMS (via `localeconv()`).
- [ ] **LC_TIME/LC_MESSAGES hardcoded** (LO-1); **~30 keywords MISSING** (LO-6).
- [ ] **error exit status DIVERGES** (LO-3).

## Test coverage — not covered
- [ ] `charmap` operand; error exit code; hardcoded-vs-live `LC_TIME`/`LC_MESSAGES`; full `LC_MONETARY` keyword set; numeric-keyword quoting.

---

# `localedef`

**Implementation:** `localedef.rs` (368 lines)
**Library:** `locale_lib/{env,platform,types}.rs`
**Tests:** `tests/localedef/mod.rs` (157 lines)
**Spec:** POSIX.1-2024 `localedef` (extract `~/tmp/i18n-spec/localedef.txt`)

## TL;DR
`localedef` does not produce a usable locale. It writes a one-line `LC_IDENTIFICATION` marker file, never opens the `-f charmap`, never parses `LC_CTYPE`/`LC_COLLATE`, ignores `-u`, doesn't resolve `copy`/`include`, mishandles `escape_char`/`comment_char` and line continuation, and collapses the distinctive 0/1/2/3/>3 exit-status table down to just 0/1/4.

## Priority issues

### Critical
- [ ] **LD-1 — No usable locale is produced.** `write_locale` writes only `…/LC_IDENTIFICATION` containing `locale: <name>` (`localedef.rs:337-368`); the parsed category data is discarded (`_definition` unused). **✓ verified:** `localedef -i loc.src ./mylocale` → the only file written is `./mylocale/LC_IDENTIFICATION` = `locale: ./mylocale`, `exit 0`. No libc-consumable output. Fix: emit a real compiled locale or delegate to host `localedef`.
- [ ] **LD-2 — `-f charmap` never read.** The path is stored (`localedef.rs:35-37`) but no charmap parser exists; symbolic names are never resolved. **✓ verified:** `localedef -f /nonexistent/charmap -i loc.src ./ml2` → `exit 0` (a missing charmap file is not even noticed).
- [ ] **LD-3 — `LC_CTYPE` and `LC_COLLATE` entirely unparsed.** No match arms (`localedef.rs:262-334`); all classification/collation data is silently dropped, so the spec's "collation without total ordering → warning + exit 1" check (spec lines ~102704-102706) can never fire.

### Major
- [ ] **LD-4 — `-c` overrides hard errors.** `if !has_errors || args.force { write_locale }` (`localedef.rs:186`) lets `-c` create output even when errors were detected; spec: "If an error is detected, no permanent output shall be created" (unconditional). Fix: `-c` gates warnings only.
- [ ] **LD-5 — Exit codes 2 and 3 never returned.** Spec's table: 0 ok / 1 warnings+created / 2 limits/charset-unsupported,no-output / 3 creation-unsupported / >3 error,no-output. All hard errors map to 4 (`localedef.rs:148-193`); 2 and 3 are unreachable.
- [ ] **LD-6 — `escape_char`/`comment_char` directives ignored** (only skipped, `localedef.rs:233-236`); non-default delimiters corrupt parsing. No line-continuation join (`localedef.rs:227`), so multi-line values are split.
- [ ] **LD-7 — `copy` not resolved** (stored, never looked up: `localedef.rs:270-282`); **`include` not recognized at all.**
- [ ] **LD-8 — `-u code_set_name` parsed but unused** (`localedef.rs:41-43`).
- [ ] **LD-9 — No stdout success report.** Spec: "shall report all categories successfully processed." None is produced.

### Minor
- [ ] **LD-10 — `LC_MONETARY` missing 9 keywords, `LC_TIME` missing 11** (`localedef.rs:306-324`); `_abday`/`_day`/… fields declared with leading underscores to suppress dead-code warnings, masking the gap.
- [ ] **LD-11 — Warnings suppressed unless `-v`** (`localedef.rs:166`) — a warning-only run exits 1 with no explanation on stderr.
- [ ] **LD-12 — `END <category>` not validated** against the open category (`localedef.rs:247-249`); category-start heuristic `!line.contains(' ')` is fragile (`localedef.rs:241`).
- [ ] **LD-13 — Non-slash `name` writes to `$TMPDIR/locale/<name>`** (`localedef.rs:339-346`), never "public."

## Conformance matrix

### Options / Operands
- [x] `-i sourcefile` CONFORMS (`localedef.rs:38-40`, `:197-216`).
- [ ] **`-f` MISSING** (LD-2); **`-u` MISSING** (LD-8); **`-c` DIVERGES** (LD-4); `-v` is a non-POSIX extension (benign).
- [ ] **`name` PARTIAL** (LD-13).

### Input files (category grammar)
- [ ] **charmap MISSING** (LD-2); **LC_CTYPE/LC_COLLATE MISSING** (LD-3).
- [ ] **LC_MONETARY/LC_TIME PARTIAL** (LD-10); **LC_NUMERIC PARTIAL**; **LC_MESSAGES** keyword set complete but unwritten (LD-1).
- [ ] **`copy` PARTIAL / `include` MISSING** (LD-7); **`escape_char`/`comment_char`/continuation MISSING** (LD-6).

### Environment variables
- [x] `LC_COLLATE`/`LC_CTYPE` "POSIX locale used regardless" — CONFORMS in effect (parser is byte-oriented); `LANG`/`LC_ALL`/`LC_MESSAGES` honored via `setlocale`. `NLSPATH` N/A.

### Exit status / errors / stdout
- [ ] **exit table DIVERGES** (LD-5); **`-c` over errors DIVERGES** (LD-4); **no stdout report MISSING** (LD-9).

## Test coverage — not covered
- [ ] Any check that the output is a usable locale (would have caught LD-1); charmap resolution; LC_CTYPE/LC_COLLATE parsing; exit codes 2/3; `copy`/`include`; line continuation.

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
- [ ] **MF-1 — `domain` directives ignored → wrong output files.** Parser `continue`s on `domain` (`po_file.rs:232`); `PoFile::domain` is never populated. All sections merge into one `HashMap` and one output file named from `args.files[0]` (`msgfmt.rs:97`, `:220-235`). Spec: each domain section → `domainname.mo`. **✓ verified:** a `module1.po` containing `domain "other"` produces a single `module1.mo` (spec wants `messages.mo` + `other.mo`). Fix: accumulate per-domain and write per-domain.
- [ ] **MF-2 — `-c`/`-v` checks never affect exit status.** `validate_entry` emits every diagnostic with `is_error: false` (`msgfmt.rs:238-300`), so `has_errors` stays false and exit stays 0. Spec: "If an abnormality is detected, the exit status shall be non-zero." Fix: set `is_error: true` for genuine abnormalities.

### Major
- [ ] **MF-3 — `-S` is dead code.** `output.set_extension("mo")` runs unconditionally before the `-S` check (`msgfmt.rs:227-233`), so the `ends_with(".mo")` guard is always false. Output naming is input-filename-based, not domain-based (see MF-1).
- [ ] **MF-4 — `-c` runs without `-v`.** Spec ties the checks to `-c -v` (behavior unspecified for `-c` alone); `validate_entry` is gated on `args.check` only (`msgfmt.rs:152`).
- [ ] **MF-5 — Newline check is wrong.** Spec: abnormal if one string *starts or ends* with `\n` while the other doesn't. The impl compares *total* newline counts (`msgfmt.rs:246-249`), flagging internal newlines and missing the boundary case.
- [ ] **MF-6 — Missing C escapes in the `.po` parser.** `\a`, `\b`, `\f`, `\v`, `\ooo` octal, `\xhh` hex are unhandled (`po_file.rs:362-380`); unknown escapes are passed through verbatim instead of erroring.
- [ ] **MF-7 — Header `charset` only read from `Content-Type:`** (`po_file.rs:421-436`); the spec's bare `charset=utf-8` header form is not recognized.
- [ ] **MF-8 — `no-c-format` flag ignored** (`msgfmt.rs:269`); the "last flag wins" rule between `c-format`/`no-c-format` is not implemented.

### Minor
- [ ] **MF-9 — c-format check is count-only** — doesn't verify argument *types* differ (spec) (`msgfmt.rs:302-330`).
- [ ] **MF-10 — `truncate` slices bytes** (`&s[..max_len]`, `msgfmt.rs:334`) — panics on a multibyte boundary in diagnostics.
- [ ] **MF-11 — `-v` alone prints no statistics** (GNU prints translated/untranslated/fuzzy counts).
- [ ] **MF-12 — zero operands → clap error**, not a POSIX-idiomatic usage diagnostic (`files` is `required`, `msgfmt.rs:58`).

## Conformance matrix

### Options / Operands
- [x] `-f` (fuzzy skip), `-D` (dir search) CONFORMS (`msgfmt.rs:37-47`, `:203-217`).
- [ ] **`-S` DIVERGES** (MF-3); **`-c`/`-v` PARTIAL** (MF-2/MF-4); **`-o` PARTIAL** (domain handling, MF-1).
- [x] `pathname...` multiple operands CONFORMS (`msgfmt.rs:102`); STDIN "Not used" (no `-` form) consistent with spec.

### Input files (.po grammar)
- [x] `msgid`/`msgstr`/`msgid_plural`/`msgstr[n]`, multi-line concatenation, `#:`/`#.`/`#,`/`#` comments, fuzzy flag CONFORMS (`po_file.rs:232-336`).
- [ ] **`domain` MISSING** (MF-1); **C escapes PARTIAL** (MF-6); **header charset PARTIAL** (MF-7); **`no-c-format` MISSING** (MF-8). `#~` obsolete handling partial.

### Env / stdout / stderr / output files / exit
- [ ] **`LC_CTYPE` PARTIAL** (input assumed UTF-8); `NLSPATH`/`LANGUAGE` N/A for the compiler.
- [x] STDOUT not used / STDERR diagnostics CONFORMS; `.mo` binary format N/A (unspecified).
- [ ] **default output naming DIVERGES** (MF-1/MF-3); **exit status PARTIAL** (MF-2).

## Test coverage — not covered
- [ ] Multi-domain `.po` → multiple `.mo` (would have caught MF-1); `-c -v` exit status; `-S` suffix; `\xhh`/`\ooo` escapes; bare `charset=` header.

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
- [ ] **XG-1 — Output file is `.pot`, not `.po`.** Spec mandates `messages.po` (or `default-domain.po`). `format!("{}.pot", …)` at `xgettext.rs:1096`. **✓ verified:** `xgettext -n t.c` writes `messages.pot`. Downstream `msgfmt messages.po` won't find it. Fix: `.po`.
- [ ] **XG-2 — Exclude option is `-X`, not `-x`.** `short = 'X'` (`xgettext.rs:93-100`); spec synopsis form 2 is `-x exclude-file`. **✓ verified:** `--help` shows `-X <EXCLUDE_FILE>`. Fix: `short = 'x'`.
- [ ] **XG-3 — Output sorted alphabetically, not extraction order.** Spec: "The msgid values shall be in the same order that the strings are extracted." Output is sorted by `MessageKey` (`xgettext.rs:915-916`) — exactly the behavior the spec's RATIONALE warns deprives translators of context. Fix: preserve insertion order.
- [ ] **XG-4 — `-j` does not deduplicate.** Spec: appended duplicates "shall … be commented out or omitted." The impl concatenates `existing + new` with no merge (`xgettext.rs:1107-1113`, self-described as "simple join").
- [ ] **XG-5 — Non-`.c`/`.h`/`.rs` operands rejected.** The spec restricts operands by *content* (C source), not extension; a C file named `source` (no extension) errors and exits 1 (`xgettext.rs:1080-1088`).
- [ ] **XG-6 — `_l` keyword variants absent from defaults.** Spec lists `gettext_l`, `ngettext_l`, `dgettext_l`, `dngettext_l`, `dcgettext_l`, `dcngettext_l`; `KeywordSpec::defaults()` omits them (`xgettext.rs:227-283`).

### Minor
- [ ] **XG-7 — Rust source parsing is a spec extension.** `.rs` files are parsed with `syn`/`proc_macro2` (Rust grammar), not C (`xgettext.rs:1066-1069`). Intentional for posixutils' own build tooling, but outside "C-language source files." Document it.
- [ ] **XG-8 — `domain` directive not written** to non-default per-domain output files (the tool emits a single file anyway); spec makes it optional only for the default file.
- [ ] **XG-9 — Plural output is always `msgstr[0]`/`msgstr[1]`** (`xgettext.rs:934-938`) regardless of target plural count (acceptable for a template).
- [ ] **XG-10 — File-I/O errors propagate as raw Rust `Err`** (not `gettext()`-wrapped `eprintln!`), so they are unlocalized; exit code is still `>0` (correct).
- [ ] **XG-11 — `-K` default mechanism is fragile** — clap injects a `"gettext"` default into the spec vec, making the `-K ""` "disable defaults" contract work only by accident (`xgettext.rs:77`, `:345-351`).

## Conformance matrix

### Synopsis / Options / Operands
- [x] `-a`, `-d`, `-n`, `-p`, `-K` (all four spec forms) CONFORMS (`xgettext.rs:43-91`, `:137-164`).
- [ ] **`-x`→`-X` DIVERGES** (XG-2); **`-j` PARTIAL** (XG-4).
- [x] `file...` and `-` (stdin as C) CONFORMS (`xgettext.rs:108-113`, `:1054-1062`).
- [ ] **operand extension gate DIVERGES** (XG-5).

### Input files / keyword recognition
- [x] C extraction via `posixutils-cc` AST CONFORMS; `gettext`/`ngettext`/`dgettext`/`dngettext`/`dcgettext`/`dcngettext` recognized.
- [ ] **`_l` variants MISSING** (XG-6); **Rust path is an extension** (XG-7); wide `L"…"` literal handling unverified.

### Output files / .po content / exit
- [ ] **`.pot` vs `.po` DIVERGES** (XG-1); **ordering DIVERGES** (XG-3); **`domain` directive PARTIAL** (XG-8).
- [x] `msgid`/`msgid_plural`/`msgstr`/`msgstr[n]`, `#:` refs (`-n`) CONFORMS (`xgettext.rs:919-938`).
- [x] STDOUT not used / STDERR diagnostics CONFORMS; exit `0`/`>0` CONFORMS (`xgettext.rs:1031-1118`).

### Environment variables
- [x] `LANG`/`LANGUAGE`/`LC_*`/`NLSPATH` handled implicitly via `setlocale`/`textdomain` (`xgettext.rs:1019-1024`) — adequate for an extraction tool.

## Test coverage — not covered
- [ ] `.po` (vs `.pot`) output name; extraction-order output; `-j` dedup; no-extension C operand; `_l` keywords; adjacent C literal concatenation; wide `L"…"` literals.

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
