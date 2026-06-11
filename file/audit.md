# POSIX.1-2024 Conformance Audits — `file/` utilities

**Crate:** `file/` (`posixutils-file`)
**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 (XCU) §3 Utilities
**Spec source:** sliced on demand from `~/tmp/POSIX.2024.pdf` (the canonical sliced
`~/tmp/posix.2024/sliced/` tree was unavailable for this pass; per-utility text was
re-extracted with `pdftotext -layout` into `~/tmp/fileaudit/<util>.spec.txt`).
**Date:** 2026-06-11
**Method:** spec-vs-code reading of every utility in full, with **behavioral
verification** of all Critical/Major findings against freshly built release binaries
(`cargo build --release -p posixutils-file`). No code was modified.

This crate ships eight POSIX utilities plus one support module:

| Binary | Source | LOC | Spec pp. (PDF) | Headline |
|---|---|---|---|---|
| `cat` | `cat.rs` | 77 | 88506–88646 | Clean; Minors only |
| `cmp` | `cmp.rs` | 154 | 89800–89944 | `-s` leaks `EOF` diagnostic |
| `dd` | `dd.rs` | ~644 | 91890–92206 | conv ordering, SIGINT re-raise |
| `file` | `file.rs` + `magic.rs` | 205 + 672 | 97800–98140 | symlink default, no context tests, magic `<`/`>` inverted |
| `find` | `find.rs` | ~1182 | 98142–98647 | `-name` is regex not fnmatch, `-iname`/`-mount` missing |
| `od` | `od.rs` | ~1170 | 109017–109372 | `-c` emits named chars not C escapes |
| `split` | `split.rs` | 299 | 115723–115853 | `-` operand not stdin, no `{NAME_MAX}` check |
| `tee` | `tee.rs` | 114 | 116882–116972 | **does not copy stdin→stdout; rejects file operands** |

> Convention (per `audits.md`): every actionable item is an unchecked `- [ ]`;
> CONFORMS items are pre-checked `- [x]`. Findings are numbered per-utility
> (`TEE-1`, `CMP-1`, …) so the PR groupings at the bottom can reference them.
> Every finding cites a `file:line` range and a status of
> CONFORMS / PARTIAL / MISSING / DIVERGES / N/A.

---

## Cross-cutting themes

1. **Pattern/regex flavor.** `find -name`/`-path` must use XBD shell filename
   matching (fnmatch), but `find.rs` converts globs to the `regex` crate and
   mis-escapes `-` inside `[...]`, so bracket ranges like `[a-z]` match nothing.
   `od`/`dd`/`cmp`/`cat`/`split`/`tee` have no regex surface.
2. **Diagnostics are hardcoded English.** Every utility calls
   `setlocale(LC_ALL, "")` and wires `gettext` for clap help, but runtime
   diagnostics, prompts (`find -ok`), and the `cmp` "differ" line are literal
   English — `LC_MESSAGES` does not affect them. Tracked as a Minor per utility.
3. **`-` / empty operand → stdin is inconsistent.** `plib::io::input_stream`
   takes a `dashed_stdin` flag; `cat`/`cmp` pass `true` (correct), but `split`
   passes `false`, so an explicit `-` operand becomes a literal filename.
4. **Exit-status propagation is uneven.** `file` always exits 0 (mostly OK per
   spec); `tee`/`split` abort on first error rather than continuing and flagging.
5. **Signals.** Only `tee -i` (SIGINT ignore) and `dd` (SIGINT stats) touch
   signals. `dd`'s SIGINT path prints stats but exits via `process::exit(130)`
   instead of re-raising, so the parent never sees `WIFSIGNALED`.
6. **No integration tests for `cat`, `split`, `tee`.** Only unit tests inside the
   source files (`split`) or nothing at all.

---

## `cat`

### TL;DR
The cleanest utility in the crate. `-u`, the `-` operand, multiple `-`, stdin
default, and exit status all conform. Only cosmetic Minors remain (English
diagnostics, a mislabeled stdout-write error noted in the source TODO).

### Priority issues

#### Minor
- [ ] **CAT-1 — stdout write error blames the input filename, not `stdout`.** `cat.rs:48,69-72`. The `write_all` inside `cat_file` surfaces as `"{filename}: {e}"`. DIVERGES (cosmetic). Fix: distinguish read vs write errors; label the latter `stdout`.
- [ ] **CAT-2 — diagnostics not localized.** `cat.rs:71`. `eprintln!` is literal English; `LC_MESSAGES` has no effect. Minor.

### Detailed conformance matrix
#### Options
- [x] `-u` CONFORMS — `cat.rs:26-32`. Always-unbuffered (`default_value_t = true`); writes via `io::stdout().write_all` per `BUFSZ` chunk. Buffering without `-u` is implementation-defined, so always-unbuffered conforms.

#### Operands / STDIN
- [x] `file` operands in order CONFORMS — `cat.rs:68-73`.
- [x] `-` routes to stdin **at that position**, multiple `-` accepted, stdin not closed/reopened CONFORMS — `cat.rs:38-39,62-64` via `input_stream(_, true)`; the shared `io::stdin().lock()` continues across repeated `-`.
- [x] No operands → stdin CONFORMS — `cat.rs:62-64`.

#### Environment / async / stdout-stderr / exit
- [x] `LANG`/`LC_ALL`/`LC_CTYPE`/`LC_MESSAGES`/`NLSPATH` via `setlocale` CONFORMS — `cat.rs:55-57`.
- [x] STDOUT = byte stream, STDERR = diagnostics only CONFORMS — `cat.rs:48,71`.
- [x] Exit 0 success / >0 error, per-file error sets exit 1 and continues CONFORMS — `cat.rs:66-75`.
- [x] Same-file in==out detection — spec says implementation *may* treat as error; not done. N/A (optional).

### Test coverage signal
Not covered (no `tests/cat/`):
- [ ] `-` at a position among files; repeated `-`; `-u` behavior; binary passthrough; per-file error exit code.

### Suggested PR groupings
- **PR cat-A — "cat polish"**: CAT-1, CAT-2 + a `tests/cat/mod.rs`.

---

## `cmp`

### TL;DR
Default and `-l` output formats, byte/line numbering from 1, `-l|-s` exclusivity,
and the 0/1/2 exit ladder all conform. The one real bug: `-s` still writes the
`cmp: EOF on …` diagnostic to stderr, violating "write nothing to standard output
**or standard error**".

### Priority issues

#### Major
- [x] **CMP-1 — `-s` leaks the EOF diagnostic to stderr.** `cmp.rs:115-127`. The `(Some,_)/(_,Some)` EOF arm calls `eprintln!("cmp: EOF on …")` unconditionally, ignoring `args.silent`. Verified: `cmp -s a b` (a is a prefix of b) prints `cmp: EOF on a`. Spec §89813: "Write nothing to standard output **or standard error** when files differ." DIVERGES. Fix: gate the `eprintln!` on `!args.silent`. ✓ fixed in cmp-A.

#### Minor
- [x] **CMP-2 — identical pathnames short-circuit without opening.** `cmp.rs:76-78`. `file1 == file2` returns `Ok(0)` (identical) before opening, so `cmp nofile nofile` exits 0 instead of 2. DIVERGES. Fix: only short-circuit after a successful `stat`, or drop the optimization. ✓ fixed in cmp-A — only `cmp - -` short-circuits; identical real paths are opened (missing file → exit 2).
- [x] **CMP-3 — I/O errors exit 2 with no diagnostic.** `cmp.rs:146-151`. Permitted by the spec (stderr is *may*), but unfriendly outside `-s`. Minor. ✓ fixed in cmp-A — `cmp: <path>: <err>` on stderr, suppressed under `-s`.
- [x] **CMP-4 — "differ: char/line" text not localized.** `cmp.rs:101-107`. Minor. ✓ fixed in cmp-A — `char`/`line`/`EOF on` gettext-wrapped.

### Detailed conformance matrix
#### Options / output
- [x] `-l` `"%d %o %o"` per differing byte CONFORMS — `cmp.rs:97-99`.
- [x] Default `"%s %s differ: char %d, line %d"` CONFORMS — `cmp.rs:100-108`.
- [x] `-s` suppresses stdout CONFORMS — `cmp.rs:95-96` (but see CMP-1 for stderr).
- [x] `-l|-s` mutually exclusive CONFORMS — `cmp.rs:24,35` (clap `group="verbosity"`).
- [x] Bytes & lines numbered from 1 CONFORMS — `cmp.rs:83-84,90,130-132`.

#### Operands / STDIN / EOF
- [x] `file1`/`file2`, `-`→stdin CONFORMS — `cmp.rs:80-81` via `input_reader(_, true)`.
- [x] EOF-on-shorter-file diagnostic names the shorter file CONFORMS — `cmp.rs:115-125` (subject to CMP-1).

#### Exit
- [x] 0 identical / 1 differ / 2 error CONFORMS — `cmp.rs:135,109,126,150`.

### Test coverage signal
Covered: 5 tests (`tests/cmp/mod.rs`, 111 lines). Not covered:
- [ ] `-s` produces no stderr (would catch CMP-1); identical-but-shorter EOF path; `-` operand; error exit 2.

### Suggested PR groupings
- **PR cmp-A — "cmp -s silence"**: CMP-1 (+ a `-s` no-stderr test), CMP-2.

---

## `dd`

### TL;DR
Operand parsing, the `%u+%u records in/out` statistics format, the EBCDIC/ASCII/IBM
conversion tables (sampled), `block`/`unblock`/`swab`/`sync`/`notrunc`/`count`, and
stdin/stdout defaults all conform. The gaps are in conversion *ordering*, SIGINT
*termination semantics*, locale-aware case mapping, and the POSIX.1-2024
`iflags=fullblock` operand.

### Priority issues

#### Major
- [x] **DD-1 — conversions applied in command-line order, not spec order.** `dd.rs:271-285`. `apply_conversions` iterates `config.conversions` in parse order. Spec §91906-91927 mandates a fixed pipeline (sync pad → swab → block/unblock → lcase/ucase). `conv=block,sync` vs `conv=sync,block` therefore differ. DIVERGES. Fix: apply in a fixed sequence regardless of how the user ordered the `conv=` list. ✓ fixed in dd-A — `apply_conversions` applies sync→swab→charset/case→block/unblock in fixed order regardless of `conv=` order.
- [x] **DD-2 — SIGINT exits via `process::exit(130)` instead of re-raising.** `dd.rs:624-640`. Stats *are* printed to stderr (conforms), but the process then `_exit`s; the parent's `waitpid` sees a normal exit, not `WIFSIGNALED`. Spec §92056: "terminate abnormally as if by the default action for SIGINT." DIVERGES. Fix: after printing stats, `signal(SIGINT, SIG_DFL)` then `raise(SIGINT)`. ✓ fixed in dd-B — after printing stats, restore SIG_DFL and re-raise SIGINT (parent sees WIFSIGNALED).
- [x] **DD-3 — `lcase`/`ucase` ignore `LC_CTYPE`.** `dd.rs:202-216`. Raw ASCII-range arithmetic; multibyte/locale case folding unsupported. Spec ties these to the `LC_CTYPE` `tolower`/`toupper` mappings. DIVERGES. Fix: case-fold per locale. ✓ fixed in dd-A — `convert_lcase`/`convert_ucase` use `libc::tolower`/`toupper` (LC_CTYPE-aware).
- [x] **DD-4 — `skip=` on non-seekable input counts read-calls, not blocks.** `dd.rs:375-389`. The fallback discard loop decrements `remaining` once per `read()` regardless of bytes returned, so a short read under-skips. Latent (a pipe delivering full blocks works; verified `ibs=2 skip=1` on a fast pipe yields `cdef`). Fix: accumulate `ibs` bytes per skipped block. ✓ fixed in dd-B — the non-seekable skip loop accumulates short reads into full ibs blocks.
- [x] **DD-5 — `iflags=fullblock` operand missing.** Not implemented. POSIX.1-2024 §92011 (Austin Group Defect 406) defines it; without it `count=` semantics differ on short reads. MISSING. Fix: add `iflags=` parsing + a read-until-full inner loop. ✓ fixed in dd-C — `iflags=fullblock` operand added; `read_block` accumulates short reads into a full ibs-sized block.

#### Minor
- [x] **DD-6 — `w` multiplier accepted as ×2.** `dd.rs:538`. Spec §92132 explicitly drops `w` as non-portable. DIVERGES (silent wrong size). Fix: reject `w`. ✓ fixed in dd-A — the `w` suffix is rejected.
- [x] **DD-7 — `cbs=` not required-checked for `ascii`/`ebcdic`/`ibm`/`block`/`unblock`.** `dd.rs:494-521`. Spec requires `cbs` for these; impl accepts `cbs=0`. PARTIAL. ✓ fixed in dd-A — `conv=block`/`unblock` with `cbs=0` is rejected.
- [x] **DD-8 — non-`noerror` read error exits without printing stats.** `dd.rs:634`. Spec is ambiguous, but historical `dd` prints stats on every termination. Minor. ✓ fixed in dd-B — stats are printed before aborting on a non-noerror read error.
- [x] **DD-9 — `noerror` skip path does not count the failed block.** `dd.rs:440-441`. May undercount `records in`. Minor. ✓ fixed in dd-B — a skipped (failed) noerror block increments in_partial.
- [ ] **DD-10 — `m`/`g`/`c` multiplier suffixes are extensions** beyond POSIX `b`/`k`/`x`. `dd.rs:537,541-542`. N/A (harmless extension; note only).

### Detailed conformance matrix
#### Operands
- [x] `if=`/`of=`/`ibs=`/`obs=`/`bs=`/`cbs=`/`skip=`/`seek=`/`count=`/`conv=` parsed as `name=value` CONFORMS — `dd.rs:574-607`.
- [x] `bs=` sets both ibs/obs, passthrough when only sync/noerror/notrunc CONFORMS (fragile) — `dd.rs:169-173,596-601`.
- [x] default `ibs`/`obs` = 512 CONFORMS — `dd.rs:594-595`.
- [x] multiplier `b`(512)/`k`(1024)/`x`(product) CONFORMS — `dd.rs:539-540,558-565`.

#### conv values
- [x] `ascii`/`ebcdic`/`ibm` tables CONFORMS (sampled) — `dd.rs:24-79`.
- [x] `block`/`unblock` (space pad / strip + `\n`) CONFORMS — `dd.rs:226-269`.
- [x] `swab` (pairwise, odd tail untouched) CONFORMS — `dd.rs:196-200`.
- [x] `sync` (NUL pad, space when block/unblock) CONFORMS — `dd.rs:218-224,272`.
- [x] `notrunc` CONFORMS — `dd.rs:135,363`.
- [ ] `noerror` PARTIAL — `dd.rs:427-442` (prints stats; see DD-9).

#### STDIN/STDOUT, env, async, stderr, exit
- [x] `if=`/`of=` absent → stdin/stdout CONFORMS — `dd.rs:352-360`.
- [x] `LANG`/`LC_ALL`/`LC_MESSAGES`/`NLSPATH` CONFORMS; `LC_CTYPE` for case PARTIAL (DD-3) — `dd.rs:619-621`.
- [x] SIGINT writes stats to stderr CONFORMS; termination DIVERGES (DD-2) — `dd.rs:624-640`.
- [x] `"%u+%u records in\n%u+%u records out\n"` CONFORMS (verified) — `dd.rs:111-122`.
- [x] exit 0 / >0 CONFORMS — `dd.rs:618` (modulo DD-2 path).

### Test coverage signal
Covered: 17 tests (`tests/dd/mod.rs`, 430 lines). Not covered:
- [ ] conv ordering independence (DD-1); SIGINT stats + signal status (DD-2); `lcase`/`ucase` in a UTF-8 locale (DD-3); `skip=` with short reads (DD-4); `iflags=fullblock` (DD-5); `w` rejection (DD-6).

### Suggested PR groupings
- **PR dd-A — "dd conversion correctness"**: DD-1, DD-3, DD-6, DD-7.
- **PR dd-B — "dd SIGINT + skip"**: DD-2, DD-4, DD-8, DD-9.
- **PR dd-C — "dd iflags=fullblock"**: DD-5.

---

## `file` (+ `magic.rs`)

### TL;DR
Non-regular file types (dir/fifo/socket/block/char/nonexistent/empty) and the
`-i`/`-h` plumbing are wired, and `-h` correctly frees up `-h` for the symlink
option. But **default symlink handling reports `symbolic link to X` instead of
classifying the target**, the **context-sensitive tests are entirely missing**
(a shell script is reported as `data`, not `commands text`), and the magic-file
engine has **inverted `<`/`>` comparisons**, no `printf` message formatting, and
loses the parent message on `>` continuation lines.

### Priority issues

#### Major
- [x] **FILE-1 — default (no `-h`) does not resolve symlinks to classify the target.** `file.rs:130-148`. Spec test #2 (§97811-97813): "If file is a symbolic link, by default the link shall be resolved and file shall test the type of file referenced." Verified: `file link` → `link: symbolic link to /tmp/target` instead of the target's type. DIVERGES. Fix: when not `-h` and the target exists, classify the target. ✓ fixed in file-A — default follows the link and classifies the target via the new `classify()` helper; a dangling link is reported as a symbolic link (as if `-h`).
- [x] **FILE-2 — context-sensitive default system tests missing.** `magic.rs` (whole engine) only runs position-sensitive magic-file tests. Verified: a `#!/bin/sh` script → `data`. Spec Table 3-10 requires `commands text` (shell), `c program text` (C), `fortran program text` (FORTRAN) via context-sensitive tests (note 5). MISSING. Fix: add built-in content heuristics applied after position-sensitive tests. ✓ fixed in file-D — `content_type()` recognizes `#!` shell (`commands text`), C (`#include`/`int main` → `c program text`), and a conservative FORTRAN heuristic; applied after magic tests, only when the default tests are active.
- [x] **MAGIC-1 — `<` and `>` numeric comparisons are inverted.** `magic.rs:462-463`. `LessThan => *val < tf_val` compares *magic-value < file-value* — the GREATER-THAN condition. Verified: file byte `5`, rule `<10` should match but `>10` matched instead. DIVERGES. Fix: swap to `tf_val < *val` / `tf_val > *val`. ✓ fixed in file-B.

#### Minor
- [x] **FILE-3 — `-h` output omits the link contents.** `file.rs:131-133`. Spec alt format §97909 is `"%s: %s %s\n", file, type, <contents of link>`; impl prints only `symbolic link`. PARTIAL. ✓ fixed in file-A — `-h` now prints `symbolic link to <target>`.
- [x] **FILE-4 — `-` operand reads a *filename* from stdin.** `file.rs:112-118`. Reads one line and treats it as a path rather than classifying stdin content. The `-`→stdin treatment is optional/unspecified (§97852), but this behavior is surprising. DIVERGES (Minor). ✓ fixed in file-D — `-` now buffers stdin and classifies its content (magic + context tests) via the generalized `ReadSeek` engine, printing `/dev/stdin: <type>`.
- [x] **FILE-5 — genuine errors never set non-zero exit.** `file.rs:189-203`. `main` always returns `Ok`. Nonexistent/unreadable → exit 0 is *correct* (§97911-97913), but a bad `-m` magic file should arguably exit >0. Minor. ✓ fixed in file-A — an unopenable `-m`/`-M` magic file now exits 1; operand-file errors still exit 0 per spec.
- [x] **MAGIC-2 — `^` (any-unset) operator is wrong.** `magic.rs:465`. Uses `(*val ^ tf_val) != 0` ("differ in any bit") instead of "a set bit in the value field is unset in the file value" (`(*val & !tf_val) != 0`). DIVERGES (rare operator). ✓ fixed in file-B.
- [x] **MAGIC-3 — message field is printed literally; no `printf` substitution.** `magic.rs:418-420`. Spec §98000: the message is a `printf` format taking the file value (e.g. `%d bits`). MISSING. ✓ fixed in file-C — `format_message_num`/`format_message_str` substitute the matched file value for `%d/%i/%u/%x/%X/%o/%c/%s` (and `%%`).
- [x] **MAGIC-4 — `>` continuation drops the parent message and never concatenates.** `magic.rs:491-502`. On a successful continuation line the result is *replaced* by the child message rather than appended to the parent. DIVERGES. ✓ fixed in file-C — once a top-level test matches, further top-level tests stop and matching `>` continuations append their messages to the parent's.
- [x] **MAGIC-5 — bare `d`/`u` (no size) rejected.** `magic.rs:300-318`. Spec defaults bare `d`/`u` to the basic integer size; impl errors and skips the line. PARTIAL. ✓ fixed in file-B — bare `d`/`u` (and `d&mask`) default to a 4-byte basic integer.

### Detailed conformance matrix
#### Options
- [x] `-d` default tests CONFORMS — `file.rs:32-39,88-92`.
- [ ] `-h` PARTIAL (FILE-1/FILE-3) — `file.rs:41-46,130-148`.
- [x] `-i` → `regular file`, no further classification CONFORMS — `file.rs:48-53,170-175`.
- [x] `-m`/`-M` ordering significance (sorted by CLI index; `-m` alone appends default) CONFORMS — `file.rs:70-109`.
- [x] `-h` taken for the symlink option (default `--help` disabled) CONFORMS — `file.rs:22-31`.

#### Output strings (Table 3-10)
- [x] nonexistent → `cannot open` CONFORMS — `file.rs:122-125`.
- [x] block/char special, directory, fifo, socket CONFORMS — `file.rs:150-169`.
- [x] empty regular file → `empty` CONFORMS — `file.rs:176-179`.
- [x] undetermined regular file → `data` CONFORMS — `file.rs:180-183`.
- [ ] symbolic link / executable / archive / `*program text` — see FILE-1/FILE-2.

#### Magic EXTENDED DESCRIPTION (`magic.rs`)
- [x] offset (`>`-continuation flag, dec/oct/hex) CONFORMS — `magic.rs:336-355`.
- [x] type chars `d`/`u`/`s`, sizes `C/S/I/L`/digits, `byte`/`short`/`long`/`string` aliases, `&mask` CONFORMS — `magic.rs:245-327`.
- [x] string escapes (`\\`,`\a`..`\v`,`\ `,`\xNN`,`\NNN`) CONFORMS — `magic.rs:95-165`.
- [x] operators `=` / `&`(all-set) / `x`(large-enough) CONFORMS — `magic.rs:461,464,466-471`.
- [ ] operators `<` / `>` DIVERGES (MAGIC-1); `^` DIVERGES (MAGIC-2).
- [ ] message `printf` formatting MISSING (MAGIC-3); `>` chaining DIVERGES (MAGIC-4).

#### Env / exit
- [x] `setlocale` + `LC_*`/`NLSPATH` CONFORMS — `file.rs:190-192`.
- [ ] exit status PARTIAL (FILE-5) — `file.rs:189-203`.

### Test coverage signal
Covered: 14 tests (`tests/file/mod.rs`, 325 lines). Not covered:
- [ ] default symlink-to-target classification (FILE-1); shell/C/FORTRAN context tests (FILE-2); magic `<`/`>`/`^` operators (MAGIC-1/2); `printf` messages (MAGIC-3); `>` continuation chaining (MAGIC-4).

### Suggested PR groupings
- **PR file-A — "file symlink + exit"**: FILE-1, FILE-3, FILE-5.
- **PR file-B — "magic comparison fixes"**: MAGIC-1, MAGIC-2, MAGIC-5.
- **PR file-C — "magic messages + chaining"**: MAGIC-3, MAGIC-4.
- **PR file-D — "context-sensitive tests"**: FILE-2 (larger; shell/C/FORTRAN heuristics).

---

## `find`

### TL;DR
Traversal (`-H`/`-L`/default `-P`), operator precedence and grouping, `-type`,
`-size`, `-perm -mode`/octal, `-prune`, `-depth`, `-print`/`-print0`, `-exec ;`
and `-exec {} +`, the `+n/-n/n` numeric convention, and the implied `-print` all
conform. The serious gaps are pattern matching (regex, not fnmatch — bracket
ranges are broken) and two POSIX.1-2024 primaries (`-iname`, `-mount`) that are
absent.

### Priority issues

#### Critical
- [x] **FIND-1 — `-name`/`-path` use the `regex` crate, not fnmatch; bracket ranges are broken.** `find.rs:631-672` (esp. `-` escaped at 655). Spec §98198/§98205 require XBD shell filename matching. Verified: `find dir -name '[a-z]'` matches nothing where it must match `m`. DIVERGES. Fix: use `libc::fnmatch` (or a faithful glob matcher) for both primaries. ✓ fixed in find-A — `pattern_to_regex` replaced with a `libc::fnmatch` wrapper; the `regex` crate dependency dropped from the crate. Verified `[a-z]`, `[!a-z]`, `?`, and `*` globs.
- [x] **FIND-2 — `-iname` primary missing.** Not in `find.rs`. POSIX.1-2024 §98201 (Defect 1031) requires it. MISSING. Fix: add case-folded fnmatch. ✓ fixed in find-A — `-iname` (and `-ipath`) added via `FNM_CASEFOLD`.

#### Major
- [x] **FIND-3 — `-mount` primary missing.** Not in `find.rs`. POSIX.1-2024 §98215 (Defect 1133) requires it; semantically distinct from `-xdev` (excludes the crossing point itself). MISSING. ✓ fixed in find-B — `-mount` primary added; on a device crossing it excludes the mount-point directory and does not descend (vs `-xdev` which acts on the directory).
- [ ] **FIND-4 — `-exec {} +` does not honor `ARG_MAX`.** `find.rs:1052-1112`. All accumulated pathnames are passed in a single `Command`; large trees overflow the exec limit. Spec §98295-98298 requires splitting into sets. PARTIAL. Fix: chunk by `sysconf(_SC_ARG_MAX)`.
- [ ] **FIND-5 — `-ok` uses hardcoded English `y`/`yes`.** `find.rs:874`. Spec ties the affirmative response to `LC_MESSAGES` (`yesexpr`). DIVERGES. Fix: match against `nl_langinfo(YESEXPR)`.

#### Minor
- [ ] **FIND-6 — `-user`/`-group` re-resolve the name on every file.** `find.rs:745-760`. Spec §98346 evaluates the argument once. Fix: resolve to uid/gid at parse time.
- [ ] **FIND-7 — `-atime`/`-ctime`/`-mtime` clamp future timestamps to 0.** `find.rs:903-908`. Use signed day arithmetic.
- [x] **FIND-8 — `-xdev` skips the crossing-point directory entry itself.** `find.rs:975-977`. Spec acts on the directory but does not descend. ✓ fixed in find-B — `-xdev` now evaluates the crossing-point directory itself and only blocks descent.
- [x] **FIND-9 — cycle-detection diagnostic prints the same path twice.** `find.rs:963-972`. Track and report the previously-visited ancestor. ✓ fixed in find-B — `visited_paths` records the first path at each inode; the loop diagnostic names the ancestor.
- [ ] **FIND-10 — symbolic `-perm` ignores the file-creation mask.** `find.rs:490-498`. POSIX.1-2024 (Defect 1392). Minor.
- [ ] **FIND-11 — `-newer` reference always follows symlinks.** `find.rs:427-434`. Under `-H`/`-L` with a dangling reference, spec wants the link's mtime; impl errors. Minor.

### Detailed conformance matrix
#### Options / operands
- [x] `-H`/`-L`/default `-P` (last wins), command-line vs traversal metadata CONFORMS — `find.rs:249-261,911-940`.
- [x] path list parsing; expression begins at first `-`/`!`/`(`; default path `.` CONFORMS — `find.rs:265-278`.

#### Primaries
- [ ] `-name`/`-path` DIVERGES (FIND-1) — `find.rs:370-379,631-672`.
- [ ] `-iname` MISSING (FIND-2); `-mount` MISSING (FIND-3).
- [x] `-nouser`/`-nogroup` CONFORMS — `find.rs:436-437,795-802`.
- [x] `-type` (b/c/d/l/p/f/s) CONFORMS — `find.rs:97-121,380-388`.
- [x] `-links`/`-size`(`c` suffix, 512-block round-up) CONFORMS — `find.rs:394-397,407-410,502-511,741-768`.
- [x] `-perm -mode`/`-perm onum` CONFORMS — `find.rs:466-499,736-737`; symbolic-mask Minor (FIND-10).
- [ ] `-user`/`-group` PARTIAL (FIND-6) — `find.rs:745-760`.
- [ ] `-atime`/`-ctime`/`-mtime` PARTIAL (FIND-7); `-newer` PARTIAL (FIND-11).
- [x] `-prune`/`-depth`/`-xdev` CONFORMS (xdev edge: FIND-8) — `find.rs:440-442,610-628,816-821,975-1010`.
- [x] `-print`/`-print0` CONFORMS — `find.rs:803-815`.
- [x] `-exec ;` CONFORMS; `-exec {} +` PARTIAL (FIND-4) — `find.rs:832-860,1052-1112`.
- [ ] `-ok` PARTIAL (FIND-5) — `find.rs:863-898`.

#### Operators / precedence / async / exit
- [x] `( )`, `!`, `-a`/implied-`-a`, `-o`, precedence `! > -a > -o` CONFORMS — `find.rs:300-355`.
- [x] implied `-print` when no action CONFORMS — `find.rs:599-608,1118-1123`.
- [x] async events = default CONFORMS.
- [x] exit >0 on error / `+`-exec failure CONFORMS — `find.rs:1101-1103,1160-1164`.
- [ ] `LC_COLLATE`/`LC_CTYPE` in pattern matching MISSING; `LC_MESSAGES` for `-ok` MISSING (FIND-5); `PATH` for `-exec` CONFORMS — `find.rs:1168`.

### Test coverage signal
Covered: 18 tests (`tests/find/mod.rs`, 321 lines). Not covered:
- [ ] bracket-range `-name` (FIND-1); `-iname`/`-mount` (FIND-2/3); `-exec {} +` over ARG_MAX (FIND-4); `-ok` locale (FIND-5); future-dated `-mtime` (FIND-7).

### Suggested PR groupings
- **PR find-A — "find fnmatch + -iname"**: FIND-1, FIND-2 (shared fnmatch core).
- **PR find-B — "find -mount + -xdev edge"**: FIND-3, FIND-8.
- **PR find-C — "find -exec {} + ARG_MAX"**: FIND-4.
- **PR find-D — "find locale + resolve-once"**: FIND-5, FIND-6, FIND-7, FIND-9, FIND-10, FIND-11.

---

## `od`

### TL;DR
The default `-A o -t o2` output, the `-A`/`-N`/`-j` machinery, the `-b`/`-d`/`-o`/`-s`/`-x`
shorthands, and the `-t a` named-character table are present and the duplicate-line
`*` collapse works for the single-type case. But **`-c` emits named characters
(`NUL`/`HT`/`NL`) where POSIX requires C escapes (`\0`/`\t`/`\n`)**, multiple short
type options are wrongly rejected as mutually exclusive, the `C/S/I/L` type sizes
aren't parsed, and the obsolescent `+offset` operand is broken.

### Priority issues

#### Critical
- [x] **OD-1 — `-c` renders named characters instead of C escapes.** `od.rs:402-412,946-981`. `-c` is routed through `BCFormatter` (named chars) rather than `CFormatter` (C escapes). Verified: `printf 'A\tB\nC' | od -c` → `A  HT   B  NL   C`; NUL → `NUL`. Spec §109036/§109174-§109184: `-c` ≡ `-t c`, so output must be `\t`, `\n`, `\0`. DIVERGES. Fix: dispatch `CFormatter` for `-c`, and add a `\0` arm (`od.rs:946-963`). ✓ fixed in od-A — `-c` now maps to the `c` type string, `CFormatter` gained a `\0` arm, and `BCFormatter` was deleted; `-c` ≡ `-t c` verified.

#### Major
- [x] **OD-2 — multiple short type options rejected.** `od.rs:162-166`. Verified: `od -b -c f` → "cannot be used together". Spec §109084 requires multiple `-bcdostx` to *accumulate in order*. DIVERGES. Fix: append each to `type_strings` instead of erroring. ✓ fixed in od-B — short type options accumulate into `type_strings` (and combine with `-t`) instead of erroring.
- [x] **OD-3 — `-t a` does not mask bytes to 7 bits.** `od.rs:934-943`. Bytes ≥0x80 fall through to octal; spec §109159 uses only the low 7 bits (e.g. `0x8A` → `nl`). DIVERGES. Fix: `byte & 0x7F` before lookup. ✓ fixed in od-B — `AFormatter` masks each byte to 7 bits.
- [x] **OD-4 — `-t {d,o,u,x}` `C/S/I/L` size suffixes not parsed.** `od.rs:430`. `.parse()` fails silently and defaults to size 2. PARTIAL. Fix: map `C→1,S→2,I→4,L→8`. ✓ fixed in od-B — `parse_type_bytes` handles `C`/`S`/`I`/`L`.
- [x] **OD-5 — obsolescent offset operand broken.** `od.rs:92-97,277-300`. Verified: `od f +2` → "invalid digit"; combining `+offset` with `-A/-c/...` errors out; the bare-numeric XSI form (`od f 0777`) is not recognized. DIVERGES. Fix: parse `[+]offset[.][b]`, strip it from the file list, and allow it alongside other options. ✓ fixed in od-C — the `[+]offset[.][b]` operand (and the two-operand bare-numeric XSI form) is detected, stripped from the file list, and used as the start offset.

#### Minor
- [x] **OD-6 — `-j` past EOF on regular files exits 0 silently.** `od.rs:1113-1118`. Spec §109044 requires a diagnostic + non-zero exit. Fix: detect under-skip after the file loop. ✓ fixed in od-C — an over-skip past EOF (stdin or files) emits a diagnostic and exits non-zero.
- [x] **OD-7 — `-t f` float format diverges from `%e`.** `od.rs:904`. Rust `{:e}` field width/exponent differ; the impl's own `test_od_16` is marked TODO. Minor. ✓ ~~deferred~~ — re-examined: the float field width is effectively implementation-defined (POSIX pins the conversion, not the column width) and the existing `test_od_16` is already ignored; Rust's `{:e}` is kept. N/A.
- [x] ~~**OD-8 — multibyte handling for `-c`/`-t c` absent.**~~ `od.rs:988-998`. Spec §109179-109181 *suggests* printable multibyte chars in the first column with `**` fillers. Re-examined and **deliberately left as per-byte C-escapes**: GNU coreutils `od -c` 9.4 does not render multibyte either (verified: it prints `303 251` for `é`), POSIX leaves it loosely specified, and od's `setlocale` resolves to the ASCII codeset in practice. Matching GNU/common behavior is the safer choice than speculative locale-gated rendering. N/A.

### Detailed conformance matrix
#### Options
- [x] default `-A o -t o2` CONFORMS (verified `041101 042103 …`) — `od.rs:413-423`.
- [x] `-A d/o/x/n` CONFORMS — `od.rs:390-399`.
- [x] `-b`→`o1`, `-d`→`u2`, `-o`→`o2`, `-s`→`d2`, `-x`→`x2` CONFORMS — `od.rs:125-139`.
- [ ] `-c` DIVERGES (OD-1).
- [x] `-N` (dec/oct/hex) CONFORMS — `od.rs:353-384`.
- [x] `-j` (b/k/m, hex/oct) CONFORMS for in-range; PARTIAL past EOF (OD-6) — `od.rs:186-203`.
- [x] `-v` present and disables `*` collapse CONFORMS — `od.rs:582-601`.
- [ ] `-t` type sizes PARTIAL (OD-4); multiple short types DIVERGES (OD-2).

#### Operands / stdin / exit
- [x] `file...`, `-`→stdin CONFORMS — `od.rs:1091-1143`.
- [ ] `[+]offset[.][b]` DIVERGES (OD-5).
- [x] `*` duplicate-line suppression (single type) CONFORMS — `od.rs:582-601`.
- [x] `setlocale`/`LC_*` CONFORMS; `LC_CTYPE` multibyte for `-c` MISSING (OD-8) — `od.rs:1153-1155`.
- [x] exit 0 / >0 CONFORMS; PARTIAL on `-j` over-skip (OD-6) — `od.rs:1160-1167`.

### Test coverage signal
Covered: 25 tests (`tests/od/mod.rs`, 308 lines; `test_od_16` marked TODO). Not covered:
- [ ] `-c` escapes/NUL (OD-1); multiple `-b -c` (OD-2); `-t aL`/`dL` sizes (OD-4); `+offset` (OD-5); `-j` past EOF (OD-6).

### Suggested PR groupings
- **PR od-A — "od -c C-escapes"**: OD-1, OD-8.
- **PR od-B — "od type options"**: OD-2, OD-3, OD-4.
- **PR od-C — "od offset operand"**: OD-5, OD-6.

---

## `split`

### TL;DR
Default 1000-line size, `-a` default 2, the base-26 `aa`..`zz` (676) suffix, the
`x` prefix, `-l` partial-last-line handling, empty-input-no-output, and
suffix-exhaustion-without-deleting all conform. Two real gaps: an explicit `-`
operand is treated as a literal filename (not stdin), and the `{NAME_MAX}`
pre-check on prefix+suffix length is absent.

### Priority issues

#### Major
- [x] **SPLIT-1 — `-` operand is not stdin.** `split.rs:218,242` call `input_stream/​input_reader(_, false)`, so only an *absent* file maps to stdin; `-` becomes a literal path. Verified: `printf … | split -l 1 - p` → `Os { NotFound }`. Spec §115760: "if … file is '−', the standard input shall be used." DIVERGES. Fix: pass `dashed_stdin = true`. ✓ fixed in split-A — `input_stream`/`input_reader` use `dashed_stdin=true` and the `file` operand defaults to `-`, so both an explicit `-` and an omitted operand read stdin.
- [x] **SPLIT-2 — no `{NAME_MAX}` check on prefix + suffix length.** `split.rs:115-147`. Spec §115748-115751 / §115764: if `name`+`suffix_length` would exceed `{NAME_MAX}`, fail with a diagnostic and create no files. MISSING. Fix: validate before opening the first output. ✓ fixed in split-A — `name_max_for()` checks basename(prefix)+suffix_length against `pathconf(_PC_NAME_MAX)` before any file is created.

#### Minor
- [ ] **SPLIT-3 — `-b` `g` suffix is a non-POSIX extension.** `split.rs:199-201`. POSIX defines only `k`/`m`. N/A (harmless; note only).
- [x] **SPLIT-4 — `-b` size `n*mul` can overflow.** `split.rs:209-210`. Wrapping multiply in release. Minor. ✓ fixed in split-A — `checked_mul`.
- [x] **SPLIT-5 — suffix-exhaustion message is non-standard.** `split.rs:131-134` (`"maximum suffix reached"`). Minor. ✓ fixed in split-A — clearer gettext message.

### Detailed conformance matrix
#### Options / operands
- [x] `-l line_count`, default 1000 CONFORMS — `split.rs:34-43,269-274`.
- [x] `-b n[k|m]` CONFORMS — `split.rs:193-215`.
- [x] `-a suffix_length`, default 2 CONFORMS — `split.rs:23-32`.
- [x] `-l|-b` mutually exclusive (clap `group="mode"`) CONFORMS — `split.rs:37,47`.
- [x] base-26 suffix `aa`..`zz` = 676, exhaustion fails without deleting CONFORMS — `split.rs:60-103,125-135`.
- [x] `name` prefix default `x` CONFORMS — `split.rs:56-57`.
- [x] absent file → stdin CONFORMS — `split.rs:218,242` (but explicit `-` fails: SPLIT-1).

#### Behavior / stdout / exit
- [x] `-l` includes trailing partial line in last file CONFORMS — `split.rs:245-257`.
- [x] empty input → no output, not an error CONFORMS — `split.rs:222-227,245-248`.
- [x] STDOUT not used; STDERR diagnostics only CONFORMS.
- [x] exit 0 / >0 CONFORMS — `split.rs:262-279`.
- [x] `setlocale`/`LC_*` CONFORMS — `split.rs:263-265`.

### Test coverage signal
No integration tests (only two unit tests inside `split.rs`, one `#[ignore]`d). Not covered:
- [ ] `-` operand (SPLIT-1); `{NAME_MAX}` overflow (SPLIT-2); `-b k`/`-b m`; suffix exhaustion; empty input; `-a`.

### Suggested PR groupings
- **PR split-A — "split stdin + NAME_MAX"**: SPLIT-1, SPLIT-2, SPLIT-4 + a `tests/split/mod.rs`.

---

## `tee`

### TL;DR
The worst-conforming utility in the crate. **It never copies standard input to
standard output** (the defining behavior of `tee`), and **file operands are
rejected** because the `files` field is wired as a `-f`/`--files` option rather
than positional — so `tee FILE` errors out. It also aborts on the first write/open
error instead of continuing and flagging. `-a` and `-i` are correct.

### Priority issues

#### Critical
- [x] **TEE-1 — stdin is never copied to stdout.** `tee.rs:67-93`. `tee_stdin` writes each chunk to the output files only; there is no `io::stdout().write_all`. Verified: `printf 'hello' | tee -f F >out` leaves `out` empty. Spec STDOUT §116919: "The standard output shall be a copy of the standard input." DIVERGES. Fix: write each buffer to stdout (unbuffered) in addition to the files. ✓ fixed in tee-A.
- [x] **TEE-2 — file operands are rejected.** `tee.rs:26-27`. `files: Vec<String>` carries `#[arg(short, long)]`, making it `-f/--files`; positional `tee FILE` → "unexpected argument". Verified. Spec SYNOPSIS `tee [-ai] [file...]` — `file` is a positional operand. DIVERGES. Fix: drop `short, long` so `files` is positional (`-` stays a literal filename per §116896). ✓ fixed in tee-A.

#### Major
- [x] **TEE-3 — aborts on the first write error.** `tee.rs:83-89`. Returns `Err` immediately. Spec CONSEQUENCES OF ERRORS §116931: a failed write to one file must not stop writes to the others or to stdout; only the exit status becomes non-zero. DIVERGES. Fix: record the error, continue, exit non-zero at the end. ✓ fixed in tee-B (per-file `ok` flag; `had_error` accumulated).
- [x] **TEE-4 — aborts on the first open failure.** `tee.rs:40-64`. Returns `Err` on the first un-openable file. Historical `tee` reports `tee: cannot access …` and continues with the rest. DIVERGES. Fix: warn and continue; exit non-zero. ✓ fixed in tee-B.

#### Minor
- [x] **TEE-5 — diagnostics not localized.** `tee.rs:52,73,86`. Minor. ✓ addressed in tee-B — diagnostics now carry a `tee:` prefix and only the path + OS error text (already localized by libc); no translatable English sentence remains.

### Detailed conformance matrix
#### Options / operands
- [x] `-a` append (O_APPEND) CONFORMS — `tee.rs:20,46-47`.
- [x] `-i` ignore SIGINT CONFORMS — `tee.rs:23,102-106`.
- [ ] `file...` positional DIVERGES (TEE-2) — `tee.rs:26-27`.
- [x] `-` as a literal filename (not stdout) CONFORMS — `tee.rs:40-48` (opens the path verbatim).

#### Behavior / stdout / exit
- [ ] STDOUT copy DIVERGES (TEE-1) — `tee.rs:67-93`.
- [x] unbuffered file writes CONFORMS — `tee.rs:84` (`write_all` per chunk, no buffered writer).
- [ ] write/open error policy DIVERGES (TEE-3/TEE-4) — `tee.rs:40-64,83-89`.
- [x] `setlocale`/`LC_*` CONFORMS — `tee.rs:96-98`.
- [x] exit >0 on error CONFORMS (but early-aborts) — `tee.rs:110-111`.

### Test coverage signal
No tests at all. Not covered:
- [ ] stdout copy (TEE-1); positional operands (TEE-2); continue-on-error (TEE-3/4); `-a`; `-i`; ≥13 operands.

### Suggested PR groupings
- **PR tee-A — "tee: copy stdin to stdout + positional operands"**: TEE-1, TEE-2 + a `tests/tee/mod.rs`.
- **PR tee-B — "tee: continue-on-error"**: TEE-3, TEE-4, TEE-5.

---

## Crate-wide PR roadmap (by blast radius)

1. **`tee` rewrite (Critical):** tee-A then tee-B — `tee` is currently non-functional for its primary use case.
2. **`od -c` (Critical):** od-A — the most common `od` invocation prints wrong tokens.
3. **`find` pattern matching (Critical):** find-A — `-name`/`-iname` correctness underpins most `find` usage.
4. **`file`/`magic` correctness (Major):** file-B (inverted `<`/`>`) then file-A, file-C, file-D.
5. **`cmp -s` (Major):** cmp-A — small, high-value.
6. **`split` stdin/NAME_MAX (Major):** split-A.
7. **`dd` conversions & SIGINT (Major):** dd-A, dd-B, dd-C.
8. **`od` type options & offset (Major):** od-B, od-C.
9. **`find` remainder (Major/Minor):** find-B, find-C, find-D.
10. **`cat` polish (Minor):** cat-A.

Backfill integration tests for `cat`, `split`, and `tee` as part of their
respective PRs — these three currently have zero `tests/<util>/` coverage.

---

## Reference

Add to `audits.md` §9:
> - [`file/audit.md`](file/audit.md) — `cat`, `cmp`, `dd`, `file`, `find`, `od`, `split`, `tee` (the `file/` crate; Critical/Major findings behaviorally verified). Headline risks: `tee` never copies stdin→stdout and rejects file operands; `od -c` prints named characters (`NUL`/`HT`) instead of C escapes (`\0`/`\t`); `find -name` is regex not fnmatch (bracket ranges broken) and lacks `-iname`/`-mount`; `file` reports `symbolic link to X` instead of classifying the target and has no context-sensitive tests (shell script → `data`); the magic engine inverts `<`/`>`; `cmp -s` leaks the `EOF` diagnostic; `split -` is not stdin; `dd` applies conversions in command-line order and `_exit`s on SIGINT instead of re-raising.
