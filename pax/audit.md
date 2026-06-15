# POSIX.1-2024 Conformance Audit — `pax`

Per-utility POSIX conformance audit for the `pax` archiver, following the
playbook in `../audits.md`.

**Implementation:** `pax/` crate (~10.3k lines non-test): `main.rs` (789),
`options.rs` (1062), `modes/{read,write,copy,append,list}.rs`,
`formats/{ustar,cpio,pax}.rs`, `pattern.rs`, `subst.rs`, `multivolume.rs`,
`blocked_io.rs`, `compression.rs`, `interactive.rs`, `archive.rs`.
**Tests:** `pax/tests/{archive,options,copy,subst,list,update,append,multivolume,compression,privileges,special}/mod.rs`.
**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 §3 `pax`, pp. 3298–3337.
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/pax.md` (1672 lines).
**Date:** 2026-06-14
**Method:** full spec read + full implementation read (delegated across five
specialist passes), then a **behavioral cross-check** against GNU `tar` 1.35
(ustar + pax interchange) and GNU `cpio` 2.15 (ODC), in both directions, plus
self-consistency (pax writes → pax reads). There is **no reference `pax`**
installed; `tar`/`cpio` are format-compatible oracles. Every Critical/Major
finding below was re-verified directly with the release binary against
tar/cpio.

## Headline

**The hard parts are right; the easy parts are wrong.** All three archive
formats are **bidirectionally interoperable with GNU tar/cpio on the everyday
path** — regular files, directories, symlinks, hard links, FIFOs, long names
(ustar prefix/name split ≤255), special-character/Unicode names, empty files,
and the EOF markers all round-trip; input format is auto-detected. The pax
interchange **self-counting extended-header length is correct** (including the
notorious digit-carry boundary), and the `-s` substitution engine is a genuine
POSIX BRE substitutor (`&`, `\1`–`\9`, `g`, `p`, in-order/first-success, empty
→skip, arbitrary delimiter). Pattern matching is correct fnmatch (it does *not*
let `*`/`?` cross `/`, which is the conformant behavior).

The defects cluster in **process semantics and a few numeric edges**, not the
wire formats: (1) **exit status is never set non-zero** for unmatched patterns,
missing operands, or per-file failures — and (2) a per-file error **aborts the
run**, so extraction stops mid-archive and later members are silently lost
(real data loss). Then a band of Major gaps: ustar/cpio **numeric fields
corrupt ≥8 GB members**; pax **drops sub-second mtime**; default extraction
**ignores umask**; the default **blocksize is 10240 for every format** (spec:
5120 for cpio/pax) and **`-b` misreads small values as a GNU blocking factor**;
`-s` **rejects the `s`/`S` flags**; **append silently coerces** a mismatched
format; a directory pattern **doesn't select its subtree** (`-d` inert in
read/list); and several **`-o` keyword behaviors** are unimplemented in
read/list. There are **no crashes/hangs**.

## Priority issues

### Critical

- [x] **#1 — No non-zero exit status for unmatched operands or per-file failures.** *(Fixed, Phase 1.)*
  There is no `had_error` accumulator anywhere; `run()` maps `Ok(())`→SUCCESS
  (`main.rs:155-161`) and the only non-zero path is a fatal `Err`. The spec
  mandates (DESCRIPTION 110034-110036): "If any specified pattern or file
  operands are not matched by at least one file or archive member, pax shall
  write a diagnostic message to standard error for each one that did not match
  and exit with a non-zero exit status," plus CONSEQUENCES OF ERRORS
  (111008-111012): per-file create/link/preserve failures "shall be written to
  standard error and a non-zero exit status shall be returned." Verified: a
  list/read with an unmatched pattern (`read.rs:170-221`, `list.rs:116`) and
  `pax -w` with a missing file operand (`write.rs:125-131`) all exit 0; the
  unmatched-pattern case also prints **nothing**. Fix: a shared error flag set
  on each per-file diagnostic + a per-pattern matched bitset swept after the
  walk; return a non-zero `ExitCode`.

- [x] **#2 — Per-file errors abort the whole run instead of diagnose-and-continue (data loss on extract).** *(Fixed, Phase 1.)*
  `read.rs:157` (`extract_entry(...)?` inside the `while` at `:116`), mirrored
  in `copy.rs:135` and `write.rs:93`, propagate any `fs` error via `?`,
  terminating the loop. Spec 111008-111011: a per-file failure is reported "but
  processing shall continue." Verified: extracting `a.tar` (members `fa.txt`,
  `fb.txt`) where `fa.txt` already exists as a directory → exit 1, "Is a
  directory", and **`fb.txt` is never extracted**. The hard-link-to-absent-
  target path (`read.rs:432` `fs::hard_link(...)?`) has the same abort and emits
  a generic, pathname-less message. Fix: catch the per-entry result, diagnose
  with the member name, set the error flag, and `continue`.

### Major

- [ ] **#3 — ustar numeric fields corrupt size/mtime for files ≥8 GB.** `formats/ustar.rs:460-467`.
  `write_octal` emits `format!("{:0width$o} ", val, width=width-2)` (digits +
  trailing space) then truncates to `width`. For the 12-byte size/mtime field a
  12-octal-digit value (≥8 GB) fills all 12 bytes and the space **terminator is
  truncated away** (violates 110784: "terminated by one or more `<space>` or NUL
  characters"); a 13-digit value (≥64 GB) **silently loses its high digit** →
  wrong size. Verified: a 9 GB member wrote a 12-digit size field with no
  terminator; at 64 GB the field decodes to 8589934592 ≠ 68719476736. Fix: emit
  `width-1` octal digits + one NUL (matching GNU tar's 11-digit+NUL, clean to
  8 GB) and error / switch to a base-256 escape above that.

- [ ] **#4 — cpio numeric fields silently truncate high digits ≥8 GB → stream corruption.**
  `formats/cpio.rs:685-698`. On overflow `write_octal_field` keeps only the
  *last* `width` chars (`&bytes[bytes.len()-width..]`), dropping the high-order
  digits. `c_filesize` is 11 octal digits (max ≈8 GB); a larger member writes a
  wrong size, after which the reader mis-frames the data stream. `c_ino`/`c_dev`
  (6 digits, max 262143) and uid/gid overflow identically. Verified: a 9 GB
  member made `cpio -itv` report a wrong size then "skipped 8589934592 bytes of
  junk." Fix: error on overflow (POSIX ODC has no large-value escape).

- [ ] **#5 — pax format drops sub-second mtime and never captures atime nanoseconds.**
  `modes/write.rs:528-548` (`build_entry`) stores only whole-second
  `metadata.mtime()`; `mtime_nsec`/`atime`/`atime_nsec` are never set, so the
  nanosecond machinery in `formats/pax.rs:398-417` always sees zero. Spec *pax
  Extended Header File Times* (110718-110731) requires an `mtime` record when
  the granularity is non-integer. Verified: a file with mtime
  `…00.123456789` wrote `20 mtime=1577880000` (fractional dropped) where GNU tar
  wrote `30 mtime=1577880000.123456789`. (The self-counting length itself is
  correct — only the value is truncated.) Fix: set
  `entry.mtime_nsec`/`atime`/`atime_nsec` from the `*_nsec()` stat accessors.

- [ ] **#6 — Default (no `-p`) extraction ignores umask; restores the archived mode verbatim.**
  `main.rs:633` `parse_privs` always returns `preserve_perms=true`, so
  `read.rs:585` always `chmod`s to the archived mode. Spec 110292-110297:
  attributes not preserved "shall be determined as part of the normal file
  creation action" (i.e. umask-masked). Verified: a 0777 member extracted under
  umask 022 with no `-p` → mode 0777 (GNU tar → 0755; `pax -r -p p` → 0777,
  correct). Fix: distinguish "`-p` absent" from "`-p` without p/e"; when perms
  aren't requested, don't `chmod`.

- [ ] **#7 — Default blocksize is 10240 for every format; cpio/pax require 5120.**
  `main.rs:335-338,488-491` use `DEFAULT_RECORD_SIZE` (20×512) regardless of
  `-x`. Spec `-x` (110345-110355): cpio/pax default 5120, ustar 10240. Verified:
  cpio and pax archives of a 6-byte file are 10240 bytes. Fix: select the
  default per `args.format`.

- [ ] **#8 — `-b` interprets values 1–511 as a GNU blocking factor (×512), not POSIX bytes.**
  `blocked_io.rs:235-241` does `if blocksize < 512 { blocksize * 512 }`. Spec
  `-b` (110058-110062) is "a positive decimal integer number of *bytes* per
  write." Verified: `-b 20` → 10240-byte records; `-b 256` would silently become
  131072 (then clamp). Fix: interpret `-b` strictly as a byte count.

- [ ] **#9 — `-s` rejects the trailing `s`/`S` symlink-content flags.** `subst.rs:96-107`
  accepts only `g`/`p` and errors on anything else. Spec 110319-110323 defines
  `s` (don't apply to symlink contents) and `S` (do). Verified: `-s '/a/X/s'` →
  `pax: Pattern error: unknown substitution flag: s`, aborting an otherwise
  conforming `-s`. Fix: parse `s`/`S` into an `Option<bool>` (even if symlink-
  content rewriting stays a no-op, the flags must not error).

- [ ] **#10 — Append in a format different from the existing archive is silently coerced, not an error.**
  `modes/append.rs:45-74` detects the existing format and writes in *that*,
  ignoring `-x`. Spec 110358-110359: a format mismatch on append "shall cause
  pax to exit immediately with a non-zero exit status." Verified: `pax -w -a -x
  cpio -f ustar.tar f` exits 0 and appends ustar records. Fix: pass `-x` into
  `append_to_archive` and error on mismatch. (cpio-onto-ustar etc.)

- [ ] **#11 — A directory-name pattern doesn't select its subtree; `-d` is inert in read/list.**
  Spec DESCRIPTION 109995-110000: a pattern that names a directory selects the
  hierarchy rooted there; `-d` (110065) disables that. The matcher makes
  `*`/`?` stop at `/` (correct) but matching is per-member exact, and `-d`
  (`args.dir_no_follow`) is wired only into write/copy `no_recurse`
  (`main.rs:318,425,475`), never read/list. Verified: `pax -f arc.tar src`
  prints nothing where `tar tf arc.tar src` prints the whole subtree. Fix:
  prefix-match directory members in read/list; honor `-d` there.

- [ ] **#12 — `-o keyword:=value` for standard keywords is silently ignored.**
  `formats/pax.rs:249-324` only emits a per-file override for fields the entry
  already set to `Some(..)`, and the fallback loop skips the ten standard
  keywords — so the spec's own example `-o gname:=other` (110267) does nothing,
  while a *custom* `mykey:=val` works. Verified: `-o gname:=mygroup` /
  `-o uid:=12345` produce no record. Fix: emit per-file standard keywords from
  `per_file` even when the entry field is `None`.

- [ ] **#13 — `-o` options are not applied in read mode and only partially in list mode.**
  `main.rs:262-288` (`run_read`) parses no format options (`ReadOptions` has no
  such field), so `-o delete=`, `-o keyword=value`, `-o keyword:=value`, and
  `-o invalid=` are discarded on extract; list mode stores them but consumes
  only `listopt`. Spec *Keyword Precedence* (110705-110717) requires
  delete/override/invalid to participate in read and list. Fix: thread
  `FormatOptions` into `ReadOptions`/the list path and apply before
  `ExtendedHeader::apply_to`.

- [ ] **#14 — `-o listopt=format` doesn't implement the POSIX `%(keyword)s` form.**
  `options.rs:535-589` invented a single-letter scheme (`%F`/`%s`/`%T`); the
  spec (EXTENDED DESCRIPTION exceptions 7-12, 110382-110423) requires
  `%(keyword)s`, `%(size)d`, `(mtime)T`, and the `M`/`D`/`F`/`L` specifiers.
  Verified: `-o "listopt=%(path)s"` emits the literal `%(path)s`. Fix: parse the
  parenthesized keyword form and the T/M/D/F/L conversions.

### Minor

- [ ] **#15 — ustar read strips trailing whitespace from `name`/`prefix`/`linkname`.**
  `formats/ustar.rs:273-278` `parse_string` does `.trim_end()`, correct for the
  space-padded numeric/owner fields but wrong for NUL-terminated path fields. A
  file named `"name "` lists/extracts as `"name"`. Fix: NUL-only termination for
  path fields.
- [ ] **#16 — Copy mode cannot copy FIFOs / devices / sockets.** `copy.rs:261-263,502-504`
  falls through to "unsupported file type". Spec 110019-110021: copy is
  as-if-write-then-extract, and write *does* archive FIFOs. Fix: `mknod`/`mkfifo`
  in copy mode.
- [ ] **#17 — `-v` `ls -l` listing uses a non-standard `h` type char for hard links.**
  `list.rs:246` maps `Hardlink→'h'`; `ls -l` shows `-` (hard links are regular
  files) and the format only *adds* ` == linkname`. Fix: render `-`.
- [ ] **#18 — Leading `.` is matched by `*`/`?`/bracket.** `pattern.rs:151-194`
  special-cases `/` but not a leading period. Spec 2.14.3 rule 2 (via
  110428-110431): a `.` at start or after `/` is not matched by `*`/`?`/bracket.
  Verified: `'*'` matched `.hidden`. Fix: refuse to consume a `.` at position 0
  or after `/`.
- [ ] **#19 — `globexthdr.name` default hardcodes `/tmp`, ignores `$TMPDIR`.**
  `options.rs:333`. Spec 110174-110177 + ENVIRONMENT VARIABLES: default is
  `$TMPDIR/GlobalHead.%n`. (No `env::var("TMPDIR")` in the crate.) `%n`/`%p`
  substitution itself is correct.
- [ ] **#20 — `-b` out-of-range / non-multiple-of-512 / zero is silently clamped, not diagnosed.**
  `blocked_io.rs:244-248`: `>32256`→32256, non-multiple→rounded up, `0`→512.
  Legal (behavior is impl-defined) but a diagnostic would be friendlier; `-b 0`
  in particular should be rejected (`-b` is "a positive decimal integer").
- [ ] **#21 — pax always emits an `x` extended header, even when no field needs one.**
  `formats/pax.rs:802-805` forces an `mtime` record "to identify the archive as
  pax." Spec Figure 3-1 shows members with no extended header; this adds ~1 KB
  (a 512-byte header + duplicate-of-ustar `mtime`) per file. Conformant but
  wasteful. Fix: emit only when `from_entry` produced a non-empty header (or
  `-o times`/overrides apply).
- [x] **#22 — Preservation (chown/utimes) failures warn but don't set non-zero exit.** *(Fixed, Phase 1.)*
  `read.rs:617-733` prints EPERM warnings but does not propagate to the exit
  status. Spec 110301-110303: preserve failure "shall affect the final exit
  status." (Subsumed by #1's error accumulator.)
- [ ] **#23 — `read_file_list` trims and drops blank lines.** `write.rs:622-628` /
  `copy.rs:674-681` `line.trim()` mangles stdin pathnames with leading/trailing
  blanks. Spec STDIN 110434-110435: only the `<newline>` is the terminator.
- [ ] **#24 — Hardcoded-English diagnostics; `LC_MESSAGES` inert; `-v` time is `TZ`/`LC_TIME`-unaware.**
  Diagnostics throughout are raw English; some lack the `pax: <path>:` shape
  (e.g. `copy.rs:104-119`). The `-v` time formatter (`list.rs:354`
  `days_to_ymd`) is hand-rolled and ignores `TZ`/`LC_TIME` (spec ties `-v` time
  to them).
- [ ] **#25 — `-o listopt` value is comma-split.** `options.rs:158-171` splits a
  `listopt` value on unescaped commas; spec 110238-110243: `listopt=format` must
  be the final keyword and all remaining characters are the format string.
- [ ] **#26 — non-UTF-8 path bytes are lossily mangled** (`to_string_lossy` in
  `ustar.rs:352,404,452`); ASCII/UTF-8 names are fine. cpio TRAILER header
  carries `c_mode=100644` (`cpio.rs:286`) where GNU writes `000000` — harmless.

## Non-POSIX extensions (note; keep, document)

- [x] **`-z`/`--gzip`** — gzip on write, gzip-magic auto-detect on read
  (`compression.rs`, `main.rs:102,500-510`); correctly forbidden with `-a`.
  Not a POSIX pax feature; cleanly isolated and tar/gzip-interoperable.
- [x] **`-M`/`--tape-length`/`--new-volume-script`** — GNU-tar-modeled
  multivolume (numbered `.N` volumes, GNU `'M'` continuation header)
  (`multivolume.rs`). Spec only says spanning is "implementation-defined," so
  this is legal; it is ustar-only and incompatible with `-z`. Inflates the CLI;
  document as an extension.

## Detailed conformance matrix

### CLI / options (per-option)
| Opt | Status | file:line | Note |
|---|---|---|---|
| `-r` / `-w` | CONFORMS | main.rs:63-66,184-196 | mode dispatch verified (list/read/write/copy/append) |
| `-a` | PARTIAL | append.rs | appends; **coerces mismatched format (#10)**; create-fallback if absent |
| `-b` | DIVERGES | blocked_io.rs:235-248 | **blocking-factor misread (#8)**; silent clamp (#20); default not format-aware (#7) |
| `-c` | CONFORMS | list.rs:128-161 | complement/exclude verified |
| `-d` | PARTIAL | main.rs:318 | works in write/copy; **inert in read/list (#11)** |
| `-f` | CONFORMS | main.rs:340-360,494-498 | verified read+write |
| `-H` / `-L` | PARTIAL | main.rs:84,96 | parsed; last-wins (110365) not explicitly enforced |
| `-i` | CONFORMS | interactive.rs | /dev/tty prompt, blank/`.`/rename/EOF-exit verified under a pty |
| `-k` | CONFORMS | read.rs:270,copy.rs:242 | existing file kept |
| `-l` | CONFORMS | copy.rs | copy-mode hard links |
| `-n` | CONFORMS | read.rs:199,list.rs:145 | first-match (but subtree caveat unmet, #11) |
| `-o` | PARTIAL | options.rs:146-253 | parsing CONFORMS; **read/list don't apply it (#13)**, `:=` standard keywords dropped (#12), listopt syntax (#14) |
| `-p` | PARTIAL | main.rs:633-675 | a/e/m/o/p + last-wins parse OK; **default ignores umask (#6)**; preserve-fail exit (#22) |
| `-s` | PARTIAL | subst.rs | BRE engine excellent; **`s`/`S` rejected (#9)** |
| `-t` | PARTIAL | write reset_atime | parsed+wired; futimens semantics unverified |
| `-u` | CONFORMS | read/write/copy | older-skip verified |
| `-v` | CONFORMS | list.rs,main.rs | list `ls -l` + `== link`; read/write→stderr; `h`-type (#17), TZ (#24) |
| `-x` | DIVERGES | main.rs:123,327 | format selected OK; **default blocksize wrong (#7)** |
| `-X` | CONFORMS | main.rs:126 | don't-cross-device, parsed+consumed |

### Modes
- [x] **list** — plain `"%s\n"` and `-v` `ls -l`+`== link` CONFORM; subtree/`-d` (#11), `h`-type (#17), unmatched-pattern silent (#1).
- [ ] **read/extract** — types/recursion/hardlink/symlink/fifo/mtime/re-extract-dir CONFORM; **aborts on per-file error (#2)**, unmatched silent (#1), umask ignored (#6), `-o` ignored (#13), subtree/`-d` (#11).
- [ ] **write** — all file types, recursion, hardlink dedup, stdin file-list CONFORM; missing operand → exit 0 (#1), per-file abort (#2), sub-second mtime (#5).
- [ ] **copy** — dir/file/symlink, `-l`, dest-must-be-dir CONFORM; **can't copy special files (#16)**, per-file abort (#2).
- [ ] **append** — basic append CONFORMS; **mismatched-format coercion (#10)**; cpio append correctly refused.

### Formats — ustar (header fields)
- [x] name/mode/uid/gid/chksum/typeflag/linkname/magic("ustar\0")/version("00")/uname/gname/dev*/prefix all CONFORM; checksum (spaces-for-chksum-field) verified vs GNU tar both ways; prefix/name split ≤255 verified; size=0 for symlink/hardlink/dir; 512 padding + two-zero-block EOF.
- [ ] **size/mtime ≥8 GB corrupt (#3)**; trailing-ws path strip on read (#15); to_string_lossy (#26).

### Formats — cpio (ODC)
- [x] magic "070707", c_dev/c_ino/c_mode/c_uid/c_gid/c_nlink/c_rdev/c_mtime/c_namesize(incl NUL)/c_filesize, name+NUL then data with no padding, TRAILER!!! — all CONFORM; verified vs GNU cpio both ways. Reader also accepts newc/binary (benign superset).
- [ ] **c_filesize/c_ino/uid/gid overflow truncation ≥8 GB (#4)**; TRAILER mode cosmetic (#26).

### Formats — pax interchange (extended headers)
- [x] **Self-counting record length CONFORMS** (proven: `30 mtime=…123456789` = 30 bytes; digit-carry boundary 99→101 handled). Per-file `x` / global `g` typeflag; two-zero-block EOF; ustar physical layout; overflow thresholds (uid/gid>2097151, size>8589934591, path>256) all CONFORM; bidirectional GNU `--format=pax` interop (long path, non-ASCII, 9 GB, `g` globals).
- [ ] **sub-second mtime/atime dropped (#5)**; `:=` standard-keyword overrides dropped (#12); `-o` not applied read/list (#13); listopt `%(keyword)s` missing (#14); always-emit `x` header (#21); globexthdr `$TMPDIR` (#19).

### `-s` substitution / patterns / `-i` / blocking
- [x] `-s`: BRE `old`, `&`, `\1`–`\9`, `\0`/`\&`/`\\` literals, `g`, `p` (`"%s >> %s\n"`), multi-`-s` order + first-success, arbitrary delimiter, empty→skip — all CONFORM (subst.rs). `-i` CONFORMS (/dev/tty, blank/`.`/rename/EOF).
- [ ] `-s` `s`/`S` rejected (#9). Patterns: fnmatch correct + doesn't cross `/` (CONFORMS, conformant vs GNU `--wildcards`); leading-`.` rule missing (#18).

### Exit status / consequences of errors
- [x] **#1 (no non-zero exit on unmatched/missing/per-file-fail)**, **#2 (per-file abort instead of continue)**, #22 (preserve-fail exit) — *all fixed, Phase 1.* Mode dispatch + 0-on-success CONFORM.

## Test coverage signal (gaps)

- [ ] No test asserts non-zero exit / diagnostics for unmatched patterns or
  missing operands (#1), nor diagnose-and-continue (#2).
- [ ] No test for ≥8 GB / ≥64 GB numeric-field encoding (#3/#4) — round-trip
  tests pass because the readers are lenient, masking the dropped terminator.
- [ ] No test for sub-second mtime (#5), umask-on-default-extract (#6),
  per-format default blocksize (#7), `-b` byte-vs-factor (#8).
- [ ] No test that `-s …/s` is accepted (#9), append-format mismatch errors (#10),
  directory-pattern subtree selection / `-d` in read (#11), `-o gname:=`/`-o` in
  read mode (#12/#13), `listopt=%(path)s` (#14), trailing-ws names (#15),
  copy-of-FIFO (#16), leading-`.` patterns (#18).

## Suggested PR groupings

- **PR A — "exit status + errors continue"** (#1, #2, #22): a shared `had_error`
  accumulator; per-mode loops catch per-entry results, diagnose with the
  pathname, continue; per-pattern matched-bitset swept after the walk. The
  headline correctness fix.
- **PR B — "numeric-field overflow"** (#3, #4): ustar `write_octal` → `width-1`
  digits + NUL with an 8 GB error/escape; cpio `write_octal_field` errors on
  overflow. Add 8 GB/64 GB header unit tests.
- **PR C — "pax time + `-o` semantics"** (#5, #12, #13, #21, #19): capture
  `*_nsec`; emit `:=` standard keywords; thread `FormatOptions` into read/list;
  emit `x` only when needed; honor `$TMPDIR`.
- **PR D — "blocksize"** (#7, #8, #20): format-aware default; `-b` as bytes;
  reject out-of-range/non-multiple/zero.
- **PR E — "extraction fidelity"** (#6, #16, #15, #23): umask-aware default
  perms; copy special files; NUL-only path-field termination; faithful stdin
  file-list.
- **PR F — "substitution + patterns + append + listopt"** (#9, #18, #10, #14,
  #11, #17): accept `-s s/S`; leading-`.`; append-format guard; POSIX `listopt`;
  directory-subtree selection + `-d`; `ls -l` `-` type for hard links.
- **PR G — "i18n"** (#24): route diagnostics through `gettext`; `TZ`/`LC_TIME`
  for `-v` time.

## Notes

- pax is, on balance, in good shape: the three on-the-wire formats interoperate
  with GNU tar/cpio across the common file types, the pax self-counting length
  and the `-s` BRE engine are correct, and mode dispatch/`-k`/`-u`/`-n`/`-c`/`-i`
  all conform. The actionable list is dominated by **process semantics**
  (exit status, abort-vs-continue) and a handful of **numeric/`-o`/default-value
  edges** — not format rewrites.
- Behavioral oracle: GNU `tar` 1.35 (ustar + `--format=pax`) and GNU `cpio` 2.15
  (`-H odc`). Build with `cargo build --release -p posixutils-pax`
  (binary at `target/release/pax`).
</content>
