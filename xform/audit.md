# POSIX.1-2024 Conformance Audits — `xform/` utilities

This file collects per-utility POSIX conformance audits for the
data-transformation crate. Each audit follows the playbook in `audits.md`.

**Crate:** `xform/` — `cksum`, `compress` (+ `uncompress`, `zcat` via `argv[0]`),
`uuencode`, `uudecode`.
**Method:** static spec-vs-code against the sliced POSIX.1-2024 tree
(`~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/`), with every
Critical/Major "absent" claim confirmed by reading the cited code + spec lines
and grepping the source (`isatty`/`is_terminal`, `chown`, newline-in-pathname
checks all confirmed absent). No code was changed — this is a punch list.
**Date:** 2026-06-18

## TL;DR (crate-wide)

`cksum` is the cleanest utility audited here — **no Critical or Major
defects**; the CRC core is a line-for-line transcription of the POSIX model
program and the output/exit/stdin contract conforms. The other three each have
real defects. **`uudecode` is the worst: it panics (exit 101) on malformed or
binary input** (`String::from_utf8().unwrap()` plus a cluster of
`expect`/`panic!`/index sites), **never honors the `-` magic cookie** mandated
by Austin Group Defect 1544, and **does not "scan…searching for" the begin
line** — it requires the header to be the literal first input line, so
uuencoded data wrapped in a mail message aborts. **`uuencode` mis-parses the
single-operand form** (`… | uuencode decode_pathname`): because the optional
`file` operand is positionally *first*, clap binds the lone operand to `file`
(opened as input) and silently defaults the decode pathname to `/dev/stdout`.
**`compress` never checks whether stdin is a terminal** before prompting for an
overwrite (spec: non-terminal + no `-f` → diagnostic + exit >0, *no* prompt),
runs its hard-link guard even under `-c` (which removes nothing), and has a
suffix-less-file decompress path that can delete the decompressed output.
Cross-cutting: locale is initialized everywhere (`setlocale` + `textdomain` +
`bind_textdomain_codeset`) but runtime diagnostics are hardcoded English, so
`LC_MESSAGES` is inert; and the Austin-Group-251 newline-in-pathname
FUTURE DIRECTION is unimplemented (encouraged, not required).

Totals: **1 Critical, 9 Major, 14 Minor.**

---

## `cksum`

**Implementation:** `xform/cksum.rs` (92 lines) + `xform/crc32.rs` (74 lines)
**Tests:** `xform/tests/cksum/mod.rs` (27 lines, 1 `#[test]`)
**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 §3, pp. 2763–2767
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/cksum.md`

### TL;DR
Textbook-conforming. The CRC table and `update`/`finalize` routines are an
exact transcription of the POSIX RATIONALE model program (Ethernet polynomial,
length extension, final complement); output format, no-operand stdin handling
(pathname + leading space omitted), and exit status all match. Only Minor
i18n / future-direction gaps remain. No Critical or Major issues.

### Priority issues

#### Minor
- [ ] **#CK1 — runtime diagnostics hardcoded English.** `cksum.rs:87`
  (`eprintln!("{}: {}", …, e)`) emits Rust std I/O error text; `LC_MESSAGES`
  inert despite the spec listing it. `setlocale`/`textdomain` are wired
  (`cksum.rs:71-73`) but the strings are not `gettext`-wrapped. Fix: route
  through a shared diag helper + `gettext`.
- [ ] **#CK2 — newline-in-pathname not treated as an error.** FUTURE DIRECTIONS
  (spec 89779-89782) encourages erroring when an output pathname contains an
  encoded `<newline>`. Not implemented (`cksum.rs:59-66`). Encouraged, not
  `shall` — Minor.
- [ ] **#CK3 — clap adds `--help`/`--version` though OPTIONS is "None".**
  `cksum.rs:28-29`. Harmless extension; flag only for strict-conformance
  tracking.

### Detailed conformance matrix

#### Options
- [x] OPTIONS "None" CONFORMS — no options declared (`cksum.rs:30-33`); only
  clap's `--help`/`--version` extension (see #CK3).

#### Operands / STDIN
- [x] `file...` CONFORMS — `files: Vec<PathBuf>` (`cksum.rs:32`), processed in
  order (`cksum.rs:84-89`).
- [x] No-operand → stdin CONFORMS — empty list pushes `PathBuf::new()`
  (`cksum.rs:78-80`); `input_stream(path, false)` treats the empty path as
  stdin (`plib/src/io.rs:15`).
- [x] No-operand → pathname + leading space omitted CONFORMS —
  `filename_prefix` is `""` when the path is empty (`cksum.rs:52-58`), matching
  spec 89630.
- [ ] **`-` operand N/A** — passed to `input_stream(.., false)` so `-` is a
  literal filename, not stdin (`cksum.rs:36`). Spec makes `-`-as-stdin
  optional ("if the implementation treats the `-` as meaning standard input",
  89606-89608), so this is conforming-by-omission.

#### Environment variables
- [x] `LANG`/`LC_ALL`/`LC_CTYPE` CONFORMS (via `setlocale(LcAll, "")`,
  `cksum.rs:71`).
- [ ] **`LC_MESSAGES`/`NLSPATH` MINOR** — locale initialized but diagnostics
  not localized (#CK1).

#### STDOUT / STDERR / exit status
- [x] STDOUT format CONFORMS — `"{} {}{}{}"` = checksum, octets, prefix,
  pathname (`cksum.rs:59-65`) matches `"%u %d %s\n"` (89629).
- [x] STDERR diagnostics-only CONFORMS — errors via `eprintln!`
  (`cksum.rs:87`).
- [x] EXIT STATUS CONFORMS — `exit_code = 1` on any per-file error, else 0
  (`cksum.rs:82-91`).

#### Algorithm (EXTENDED DESCRIPTION / RATIONALE model)
- [x] CRC table CONFORMS — `CRCTAB` (`crc32.rs:11-44`) is the POSIX table.
- [x] `update` CONFORMS — `s = (s<<8) ^ CRCTAB[((s>>24)^c) & 0xff]`
  (`crc32.rs:49-58`) matches `memcrc` inner loop (89729-89732).
- [x] `finalize` CONFORMS — length extension least-significant-octet-first +
  final `~s` (`crc32.rs:61-74`) matches 89734-89739.

### Test coverage signal
Not covered:
- [ ] Multiple file operands (only single stdin case tested).
- [ ] Named-file operand (pathname printed in output).
- [ ] Binary / non-UTF-8 file content.
- [ ] Empty input (length-only CRC).
- [ ] Per-file error → exit status 1.

---

## `compress` / `uncompress` / `zcat`

**Implementation:** `xform/compress.rs` (622 lines); `uncompress` and `zcat`
are symlinks to the `compress` binary (created by `xform/build.rs`), dispatched
on `argv[0]` (`compress.rs:60-71`). LZW core in `plib/src/lzw.rs`; DEFLATE via
`flate2`.
**Tests:** `xform/tests/compress/mod.rs` (1111 lines, ~30 `#[test]`s)
**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 §3, pp. 2783–2788
(plus `uncompress.md` p. 3538, `zcat.md` pp. 3679–3684 — both "Refer to
compress").
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/compress.md`

### TL;DR
Broad and mostly correct: `argv[0]` mode detection, the full option set
(`-bcdfgmv`), `.Z`/`.gz` suffixes, magic-byte auto-detection on decompress,
multi-file concatenation for `zcat`, the size-increase→exit-2 contract, and
mode/time metadata preservation all conform. The defects are in the
overwrite-prompt path (no `is_terminal` check), an over-eager hard-link guard
that fires under `-c`, a suffix-less decompress path that can delete its own
output, and a `-b` validation range that contradicts the actual default.

### Priority issues

#### Major
- [x] **#C1 — overwrite prompt never checks whether stdin is a terminal.**
  ✓ fixed (phase 4) — `may_overwrite` gates the prompt on
  `io::stdin().is_terminal()`; when stdin is not a terminal and `-f` is absent
  it writes a diagnostic and refuses (exit 1) without reading stdin.
  `compress.rs:440-449` (compress) and `compress.rs:526-535` (decompress) call
  `prompt_user` (`compress.rs:107-112`) whenever the output file exists and
  `-f` is absent, unconditionally reading a line from stdin. Spec (90427-90432):
  prompt **only** when stdin is a terminal; when stdin is **not** a terminal
  and `-f` is absent, write a diagnostic, do not overwrite, exit >0 — no
  prompt. No `isatty`/`is_terminal` call exists anywhere in the crate.
  Consequence: in a pipeline the prompt silently consumes data from the input
  stream. Fix: gate on `std::io::stdin().is_terminal()`.
- [x] **#C2 — hard-link guard runs even with `-c`.** ✓ fixed (phase 4) — both
  call sites now gate `check_hard_links` on `!writing_to_stdout`, so `-c`/stdin
  (input never unlinked) no longer triggers the guard. `compress_file` calls
  `check_hard_links` (`compress.rs:389`) *before* the `writing_to_stdout`
  branch (`compress.rs:417`); `decompress_file` likewise (`compress.rs:494`).
  Under `-c` the input is never removed, so the multi-hard-link case (spec
  90403-90406, which is about files "to be removed after processing") does not
  apply — yet `compress -c hardlinked` warns and returns 1 with no output.
  Fix: only check when the input will actually be unlinked
  (`!stdout && !stdin`).
- [x] **#C3 — suffix-less decompress can delete the decompressed output
  (data loss).** ✓ fixed (phase 4) — `decompress_file` now refuses with an
  "unknown suffix" diagnostic (exit 1) when the resolved `output_path` equals
  the `input_path`, so the write-then-`remove_file` on the same path can no
  longer occur. For `uncompress foo` where `foo` exists but has no known
  suffix, `find_decompress_input` returns `foo` as-is (`compress.rs:336-339`)
  and `decompress_output_path(foo)` returns `foo` unchanged (no suffix to
  strip, `compress.rs:324-333`). The code then writes the decompressed bytes to
  `foo` (`compress.rs:538-539`) and immediately `fs::remove_file(&input_path)`
  on the same path (`compress.rs:548`) — destroying the result. Also diverges
  from spec 90381-90384 ("file shall be used as the name of the output file,
  and the default suffix `.Z` shall be appended to file to form the input
  pathname"). Fix: when input has no known suffix, the input is `file.Z` and
  output is `file`; refuse when input==output.

#### Minor
- [x] **#C4 — `-b` LZW range contradicts the default.** ✓ fixed (phase 5) —
  `validate_bits` now accepts LZW 9–16, matching the 16-bit default and the
  RATIONALE's 15/16 encouragement. `validate_bits`
  rejects LZW values outside 9–14 (`compress.rs:261-271`), but the default
  (no `-b`) is 16 (`plib/src/lzw.rs:18` `BITS=16`, used when `mbits==None` at
  `lzw.rs:433-437`) and the writer clamps requests to 9–16
  (`lzw.rs:434-438`). So the default emits 16-bit codes while `-b 15`/`-b 16`
  are rejected. Spec normative DESCRIPTION says the default "shall be 14"
  (90414), while the RATIONALE (90542-90544) encourages 15/16. Pick one
  consistently (recommend: default 16, accept `-b 9..16`).
- [x] **#C5 — output ownership not preserved.** ✓ fixed (phase 5) —
  `FileMetadata` now carries uid/gid and `apply_to` best-effort `libc::chown`s
  the output before restoring mode (chown clears setuid/setgid). Spec 90389-90392: with
  sufficient privilege, output ownership shall match input. `FileMetadata`
  preserves only mode + atime + mtime (`compress.rs:114-163`); no
  `chown`/`fchown` anywhere. Minor (privileged path only).
- [x] **#C6 — `{NAME_MAX}` hardcoded to 255.** ✓ fixed (phase 5) — `name_max`
  queries `pathconf(_PC_NAME_MAX)` on the target directory, falling back to 255.
  `compress.rs:23,302` uses a
  literal instead of `pathconf(_PC_NAME_MAX)`. `{PATH_MAX}` uses the libc
  constant (`compress.rs:182-195`) — acceptable since that check is "may fail".
- [ ] **#C7 — diagnostics hardcoded English.** Error/`-v`/warning strings
  (`compress.rs:169-173, 379-385, 427, 470-475, 616`) are not `gettext`-wrapped
  (the overwrite *prompt* is, `compress.rs:441,527`). `LC_MESSAGES` inert for
  diagnostics.
- [x] **#C8 — "cannot remove input" policy.** ✓ fixed (phase 5) — when the
  input cannot be removed, both paths now back out (remove) the just-written
  output and emit a "cannot remove input" diagnostic with a non-zero exit, per
  spec 90393-90400. (The S_ISVTX-specific `-f`/no-`-f` nuance is intentionally
  not micro-managed; the core "don't leave both files, report" behavior holds.)
- [x] **#C9 — exit-code ordering made deterministic.** ✓ fixed (phase 5) — a
  `merge_exit` helper combines per-file codes order-independently (1 outranks 2
  outranks 0), so the final status no longer depends on file order. Both 1 and
  2 remain non-zero per spec 90511-90516.

### Detailed conformance matrix

#### Options
- [x] `-c`/`-d`/`-f`/`-g`/`-m`/`-v` CONFORMS — all declared and consulted
  (`compress.rs:76-100`); `-g` ⇒ deflate (`compress.rs:286-288`), `-m` last-one
  semantics handled by clap single value.
- [x] **`-b`** ✓ fixed (phase 5) — see #C4.
- [x] `-m`/`-g` mutual non-error CONFORMS — `-g` takes precedence
  (`get_compress_algorithm`, `compress.rs:285-293`); spec 90447-90449 says
  specifying both "shall not be considered an error".

#### Operands / STDIN / STDOUT
- [x] `argv[0]` dispatch CONFORMS — `compress`/`uncompress`/`zcat`
  (`compress.rs:60-71, 570-580`); `zcat`⇒`-c -d`, `uncompress`⇒`-d`.
- [x] `file...` in order + no-operand stdin CONFORMS — empty list ⇒ `["-"]`
  (`compress.rs:583-585`); `-`⇒stdin⇒stdout (`compress.rs:373,417,483-484`).
- [x] Multi-file `zcat` concatenation CONFORMS — each file decompressed to
  stdout in turn (`compress.rs:596-601`), matching spec 90369-90370.
- [x] **Overwrite/terminal handling** ✓ fixed (phase 4) — see #C1.

#### Output files / metadata
- [x] `.Z`/`.gz` suffix CONFORMS — `Algorithm::suffix` (`compress.rs:33-38`);
  output path built with suffix + `{NAME_MAX}` guard (`compress.rs:296-321`).
- [x] mode/atime/mtime preservation CONFORMS — `FileMetadata::apply_to`
  (`compress.rs:131-162`).
- [x] **ownership preservation** ✓ fixed (phase 5) — see #C5.
- [x] GZIP format for deflate/gzip CONFORMS — `flate2` `GzEncoder`
  (`compress.rs:206-211`), spec 90504 (RFC 1952).

#### Decompress input resolution
- [x] magic-byte auto-detect CONFORMS — `Algorithm::from_magic`
  (`compress.rs:40-49`), `decompress_auto` (`compress.rs:237-246`); satisfies
  "examine the file's contents" for `-c` (90385-90388).
- [x] try `file`, `file.Z`, `file.gz` CONFORMS (as permitted extension) —
  `find_decompress_input` (`compress.rs:336-369`); spec 90534-90537 allows
  trying other known suffixes.
- [x] **suffix-less file path** ✓ fixed (phase 4) — see #C3.

#### Exit status / consequences of errors
- [x] compress 0/1/2/>2 CONFORMS — size-increase ⇒ 2 (`compress.rs:433-435`),
  errors ⇒ 1 (`compress.rs:609-617`); spec 90511-90516.
- [x] uncompress/zcat 0/>0 CONFORMS — code 2 never produced on the decompress
  path.
- [x] CONSEQUENCES (input unmodified on error) CONFORMS — output written to a
  *new* path; input removed only after success (`compress.rs:452-462`), spec
  90521-90522.
- [x] **exit-2 vs exit-1 ordering** ✓ fixed (phase 5) — see #C9.

### Test coverage signal
Not covered:
- [x] Non-terminal overwrite-without-`-f` (should diagnose + exit >0, not
  prompt) — #C1 (✓ phase 4, `test_overwrite_nonterminal_no_force_diagnoses`).
- [x] `compress -c` on a multiply-hard-linked file — #C2 (✓ phase 4,
  `test_compress_c_on_hardlinked_file`).
- [x] Decompress of a suffix-less existing file — #C3 (✓ phase 4,
  `test_decompress_suffixless_no_data_loss`).
- [x] `-b 15`/`-b 16` (previously rejected) and bits value in header — #C4
  (✓ phase 5, `test_compress_bits_16_accepted_and_roundtrips`).
- [ ] `-v` percentage output content.
- [ ] Ownership/time preservation assertions.

---

## `uuencode`

**Implementation:** `xform/uuencode.rs` (192 lines)
**Tests:** `xform/tests/uue/mod.rs` (220 lines, 4 `#[test]`s — shared with
uudecode)
**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 §3, pp. 3559–3563
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/uuencode.md`

### TL;DR
Both encodings (historical + Base64) are byte-correct on the happy path —
length char + `0x20` offset + `0x20`→`0x60` replacement, the trailing
backtick/`end` lines (including the empty-input case), and `begin-base64`/`====`
all match the spec, and the GNU-sharutils fixtures round-trip in the tests. The
real defect is operand parsing: the spec's optional operand (`file`) is
*first*, so the single-operand form is mis-handled.

### Priority issues

#### Major
- [x] **#UE1 — single-operand form mis-parsed.** ✓ fixed (phase 3) — operands collected into a `Vec`; 1 operand = stdin + decode_pathname, 2 = file + decode_pathname. Synopsis is
  `uuencode [-m] [file] decode_pathname` (119822) — the optional `file` is the
  *first* operand. With two positionals (`file`, `decode_path`) both declared
  `Option` (`uuencode.rs:28-33`), clap fills left-to-right, so
  `… | uuencode decode_pathname` binds the lone operand to `file` (opened as
  input, `uuencode.rs:115-146`) and defaults `decode_path` to `/dev/stdout`
  (`uuencode.rs:100-103`). The canonical pipe usage therefore reads the wrong
  source and emits the wrong decode pathname. Fix: collect operands into a
  `Vec`; 1 operand ⇒ stdin + `decode_pathname`, 2 operands ⇒ `file` +
  `decode_pathname`.
- [x] **#UE2 — required `decode_pathname` operand not enforced.** ✓ fixed (phase 3) — zero operands → usage error, exit 1. Zero
  operands is accepted: `uuencode` reads stdin and emits
  `begin <mode> /dev/stdout` (`uuencode.rs:100-103,121-138`). Spec makes
  `decode_pathname` mandatory; missing operand should be a usage error.

#### Minor
- [x] **#UE3 — `umask()` mutates process umask to read it.** ✓ fixed (phase 3) — `current_umask_mode` reads then restores the mask. The stdin mode is
  computed as `RW & !umask(RW)` (`uuencode.rs:121-132`), which *sets* the umask
  to `0666` and never restores it. Harmless (the process exits immediately) but
  a smell; query without mutating, or restore afterward.
- [ ] **#UE4 — diagnostics debug-quote the path + hardcoded English.**
  `eprintln!("{:?}: {}", …)` (`uuencode.rs:188`) wraps the pathname in quotes
  and is not `gettext`-wrapped.
- [ ] **#UE5 — `decode_pathname` not validated against the portable filename
  character set N/A.** Spec 119839-119840: results "unspecified" if non-portable
  chars appear — no obligation, tracked only.

### Detailed conformance matrix

#### Options / operands
- [x] `-m` (Base64) CONFORMS — `uuencode.rs:25-26,105-109`.
- [x] **`[file] decode_pathname`** ✓ fixed (phase 3) — all of 0/1/2-operand forms handled (#UE1, #UE2).
- [x] `decode_pathname` = `-`/`/dev/stdout` CONFORMS — passed verbatim into the
  header (`uuencode.rs:135,143`); meaning is interpreted by `uudecode`.

#### STDOUT — Base64 algorithm
- [x] header `begin-base64 <mode> <path>` CONFORMS — `uuencode.rs:42-47,135`.
- [x] 6-bit→Base64 + line length CONFORMS — 45 input bytes/line ⇒ 60 chars
  ≤ 76 (`uuencode.rs:62-66,156-160`), spec 119898.
- [x] `====` terminator CONFORMS — `uuencode.rs:160` + trailing `\n`
  (`uuencode.rs:165`), spec 119868.

#### STDOUT — Historical algorithm
- [x] header `begin <mode> <path>` CONFORMS — `uuencode.rs:43,135,143`.
- [x] 3-octet→4-char with `0x20` offset CONFORMS — `uuencode.rs:78-83`, spec
  119934-119937.
- [x] `0x20`→`0x60` replacement CONFORMS — `uuencode.rs:87-91`, spec
  119925/119999-120001.
- [x] length char per line CONFORMS — `len + 0x20` (`uuencode.rs:70`), spec
  119939-119940.
- [x] trailing backtick line + `end`, incl. empty input CONFORMS —
  `out.extend(b"`\nend")` (`uuencode.rs:154`), spec 119941-119943.
- [x] mode = 3 octal digits CONFORMS — `get_permission_values`
  (`uuencode.rs:52-60`), file perms (`uuencode.rs:141`) or umask-derived for
  stdin.

#### Exit / locale
- [x] EXIT STATUS 0/>0 CONFORMS — `uuencode.rs:184-191`.
- [x] locale init CONFORMS — `uuencode.rs:178-180`; diagnostics not localized
  (#UE4).

### Test coverage signal
Not covered:
- [x] Single-operand stdin form (`… | uuencode name`) — #UE1 (✓ phase 3).
- [x] Zero-operand usage error — #UE2 (✓ phase 3).
- [ ] Empty input (begin line then backtick/`end`).
- [ ] `decode_pathname` of `-` / `/dev/stdout` round-trip through `uudecode`.

---

## `uudecode`

**Implementation:** `xform/uudecode.rs` (196 lines)
**Tests:** `xform/tests/uue/mod.rs` (220 lines, shared with uuencode)
**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 §3, pp. 3556–3558
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/uudecode.md`

### TL;DR
Decodes well-formed historical and Base64 fixtures correctly and applies the
header's mode bits. But it is fragile and incomplete on the inputs it is meant
to consume: it **panics on malformed or binary input**, **does not honor the
`-` magic cookie** (Defect 1544), **does not scan for the begin line** (it must
be input line 1), uses a **strict** Base64 decoder that rejects transport
artifacts (CRLF/whitespace), and **skips the existing-file write-permission
check** while treating a failed `set_permissions` as fatal.

### Priority issues

#### Critical
- [x] **#UD1 — panics (exit 101) on malformed / non-text input.** ✓ fixed (phase 1) — byte-based decode (no `from_utf8().unwrap()`); `Header::parse` + line decoders return `io::Result`; short/empty lines tolerated; regression tests in `tests/uue/mod.rs`.
  `String::from_utf8(buf).unwrap()` (`uudecode.rs:118`) aborts on any non-UTF-8
  byte — and the spec RATIONALE (119781-119783) explicitly notes the encoded
  stream may be wrapped in a non-text file. Additional abort sites:
  `Header::parse` indexes `split[1]`/`split[2]` (`uudecode.rs:61,67`) and
  `from_str_radix(...).expect(...)` (`uudecode.rs:66`); `expect("No header
  line")` (`uudecode.rs:120`); `expect("No end line")` + `panic!("Invalid
  ending")` (`uudecode.rs:127-131`); `decode_historical_line` indexes
  `chunk[1..3]` on a short final chunk (`uudecode.rs:84-86`); and
  `line.as_bytes()[0] - 32` underflows in debug on a control-char first byte
  (`uudecode.rs:135`). POSIX expects a diagnostic + exit >0, not SIGABRT.
  Fix: replace every `unwrap`/`expect`/`panic!`/raw index with `?`-propagated
  `io::Error`s; decode on bytes, not `String`.

#### Major
- [x] **#UD2 — `-` magic cookie not honored.** ✓ fixed (phase 2) — `is_stdout_cookie` accepts `-` and `/dev/stdout` for both `-o` and the header. Defect 1544 (119816-119818)
  requires an output pathname of `-` (from `-o -` *or* the header) to mean
  standard output. Only `/dev/stdout` is special-cased (`uudecode.rs:154`);
  `-` would create a file literally named `-`. Fix: treat both `-` and
  `/dev/stdout` as the stdout cookie for `-o` and the header path.
- [x] **#UD3 — does not scan for the begin line.** ✓ fixed (phase 2) — input is scanned for a `begin`/`begin-base64` line. Spec DESCRIPTION
  (119705-119707): "scan the input file, searching for data compatible with one
  of the formats". The code takes `lines.next()` as the header
  (`uudecode.rs:119-120`) and panics if line 1 is not `begin`/`begin-base64`,
  so a `begin` preceded by mail headers / blank lines fails. Fix: skip lines
  until a valid begin header is found.
- [x] **#UD4 — Base64 decoder is strict.** ✓ fixed (phase 2) — each line is alphabet-filtered before decode (ignores CR/whitespace/stray chars). Spec 119899-119900: "All line breaks
  or other characters not found in the table shall be ignored by decoding
  software." The whole line is handed to `BASE64_STANDARD.decode`
  (`uudecode.rs:96-99`), which errors on a trailing `\r` (CRLF transport),
  embedded spaces, etc. The historical path replaces backtick→space
  (`uudecode.rs:125`) but does not strip `\r`. Fix: filter to the alphabet
  before decoding.
- [x] **#UD5 — existing-file write-permission check missing; overwrite is
  unlink+create.** Spec 119716-119720: if the target exists and the user lacks
  write permission, terminate with an error; if writable, overwrite in place.
  The code unconditionally `remove_file` then `File::create`
  (`uudecode.rs:156-161`), which depends only on directory write permission
  (bypassing the file's own perms) and changes inode/ownership. Fix: check
  write access; truncate-in-place rather than unlink.
- [x] **#UD6 — failed `set_permissions` treated as fatal.** ✓ fixed (phase 2) — `set_permissions` result is ignored. `set_permissions(…)?`
  (`uudecode.rs:168`) propagates as an error. Spec 119719-119720: "if the mode
  bits cannot be set, uudecode shall not treat this as an error." Fix: ignore
  the error (`let _ =`).

#### Minor
- [x] **#UD7 — mode parsing edge cases.** ✓ fixed (phase 2) — parsed mode masked to `0o7777`; symbolic mode documented out of scope. `from_str_radix(mode_str, 8)`
  (`uudecode.rs:66`) accepts arbitrary-width octal and `(mode>>9<<9)|bits`
  (`uudecode.rs:164`) can fold a 4-digit header (e.g. `4755`) into setuid bits;
  symbolic chmod notation (spec 119714-119715, "unspecified" initial mode) is
  unsupported and a leading sign panics (`uudecode.rs:63-64`). Low risk —
  `uuencode` here always emits 3 octal digits.
- [ ] **#UD8 — diagnostics debug-quote the path + hardcoded English.**
  `eprintln!("{:?}: {}", …)` (`uudecode.rs:192`); not `gettext`-wrapped.

### Detailed conformance matrix

#### Options / operands / STDIN
- [x] `-o outfile` override CONFORMS — `args.outfile … unwrap_or(&header.out)`
  (`uudecode.rs:152`).
- [x] **`-o -` (and header `-`) DIVERGES** — see #UD2 (✓ phase 2).
- [x] `file` operand / no-operand stdin CONFORMS — `uudecode.rs:105-116`.
- [x] **scan-for-header MISSING** — see #UD3 (✓ phase 2).

#### Decode correctness
- [x] historical decode CONFORMS on well-formed lines — `uudecode.rs:77-93`,
  backtick→space + per-line length truncation (`uudecode.rs:124-139`).
- [x] Base64 decode CONFORMS on well-formed lines — `uudecode.rs:95-99`;
  `====` terminator (`uudecode.rs:144-146`).
- [x] **lenient decode of transport artifacts MISSING** — see #UD4 (✓ phase 2).
- [x] **robustness on malformed input CRITICAL** — see #UD1 (✓ fixed, phase 1).

#### Output file / mode
- [x] `/dev/stdout` → stdout CONFORMS — `uudecode.rs:154-155`.
- [x] mode bits from data, umask not affecting final CONFORMS — explicit
  `set_mode` after create (`uudecode.rs:162-168`); spec 119711-119713.
- [x] **write-permission check / in-place overwrite DIVERGES** — see #UD5 (✓ phase 2).
- [x] **failed set-mode fatal DIVERGES** — see #UD6 (✓ phase 2).

#### Exit / locale
- [x] EXIT STATUS 0/>0 CONFORMS on graceful errors — `uudecode.rs:188-195`
  (but panics bypass this ⇒ 101; see #UD1).
- [x] locale init CONFORMS — `uudecode.rs:182-184`; diagnostics not localized
  (#UD8).

### Test coverage signal
Not covered:
- [x] Malformed / truncated / non-UTF-8 input (should diagnose + exit >0) —
  #UD1 (✓ phase 1).
- [x] `-o -` and header pathname `-` → stdout — #UD2 (✓ phase 2).
- [x] Header not on line 1 (leading mail headers) — #UD3 (✓ phase 2).
- [x] CRLF / whitespace-padded encoded lines — #UD4 (✓ phase 2).
- [x] Decode to an existing read-only file — #UD5 (✓ phase 2).

---

## Suggested PR groupings

- **PR A — "uudecode: no panics on bad input" (#UD1):** replace every
  `unwrap`/`expect`/`panic!`/raw-index with `?`-propagated `io::Error`s; decode
  on bytes rather than `String::from_utf8`. The single Critical.
- **PR B — "uudecode: spec semantics" (#UD2, #UD3, #UD4, #UD5, #UD6):** `-`
  magic cookie, scan-for-`begin`, lenient (alphabet-filtered) decoding,
  write-permission check + in-place overwrite, ignore failed `set_permissions`.
- **PR C — "uuencode: operand parsing" (#UE1, #UE2, #UE3):** collect operands
  into a `Vec` and disambiguate 1- vs 2-operand forms; enforce the required
  `decode_pathname`; stop mutating the umask to read it.
- **PR D — "compress: prompt & hard-link gating" (#C1, #C2, #C8):** add an
  `is_terminal` check (diagnose + exit >0 when non-terminal and no `-f`); gate
  the hard-link guard on actual removal; implement the S_ISVTX/cannot-remove
  policy.
- **PR E — "compress: decompress path & bits" (#C3, #C4, #C5):** fix the
  suffix-less input/output collision (data loss); reconcile the `-b` LZW range
  with the default; preserve ownership when privileged.
- **PR F — "xform i18n + cosmetics" (#CK1, #UE4, #UD8, #C7, #CK3, #C6):**
  route diagnostics through a shared `gettext`-backed diag helper; drop `{:?}`
  path quoting; `pathconf(_PC_NAME_MAX)`; optionally the
  Austin-Group-251 newline-in-pathname FUTURE DIRECTION (#CK2).
