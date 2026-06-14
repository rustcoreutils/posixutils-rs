# POSIX.1-2024 Conformance Audits — `pathnames/` utilities

This file collects per-utility POSIX conformance audits for the pathname
utilities crate (`basename`, `dirname`, `pathchk`, `realpath`). Each audit
follows the playbook in `audits.md`.

**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 §3.
**Reference slices:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/{basename,dirname,pathchk,realpath}.md`
**Date:** 2026-06-14
**Verification:** All Critical/Major findings were reproduced against the
release binaries (`cargo build -p posixutils-pathnames`) before publishing.

## Crate-wide TL;DR

| Utility | Verdict | Headline |
|---|---|---|
| `basename` | **Broken on common inputs** | Panics (exit 101) on `/`, `..`, `x/..`; suffix is stripped from the *whole path* before the final component is extracted, and the "suffix identical to result" guard is missing → wrong output. |
| `dirname` | **Largely conforms** | Lexical `PathBuf::pop()` matches the spec on every tested case. Only minor gaps (non-UTF-8 lossy output, `//`). |
| `pathchk` | **Default mode unusable** | The no-option (filesystem) check errors `pathconf error(path length)` for *every existing file* and for creatable relative names; `-p`/`-P` are wired mutually-exclusive (spec mandates using them together); `-p` uses `is_ascii()` instead of the portable-filename character set. |
| `realpath` | **Does not resolve symlinks except under `-e`** | Default and `-E` modes do purely lexical normalization — the canonical job of `realpath` (eliminating symbolic-link components) is not performed. Only `-e` (via `fs::canonicalize`) is correct. |

Cross-cutting: runtime diagnostics in `pathchk`/`realpath` are hardcoded English
(not routed through `gettext`), so `LC_MESSAGES` is inert despite `setlocale`
being called. `basename`/`pathchk` take `String` operands (clap rejects
non-UTF-8 paths); `dirname` takes `OsString` but prints via `to_string_lossy`.

---

## `basename`

**Implementation:** `pathnames/basename.rs` (64 lines)
**Tests:** `pathnames/tests/basename/mod.rs` (70 lines, 5 `#[test]`s)
**Spec:** POSIX.1-2024, Vol. 3 §3, pp. 2692–2694
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/basename.md`

### TL;DR

The implementation collapses the spec's 6-step algorithm into "optionally
`strip_suffix` on the whole argument, then `Path::file_name()`". This is wrong
in two structural ways: (1) `Path::file_name()` returns `None` for `/`, `..`,
and any path ending in `..`, and the code `.expect()`s on it → **panic / exit
101** on those very common operands; (2) the suffix is removed from the entire
pathname *before* the directory prefix and trailing slashes are processed, and
the step-6 guard "suffix is not identical to the characters remaining" is not
implemented — so `basename /usr/bin/env env` prints `bin` instead of `env`.

### Priority issues

#### Critical
- [ ] **#B1 — `basename` panics on `/`, `..`, and any path whose last component is `..`.** `basename.rs:48-50`. `Path::file_name()` returns `None` for these; `.expect("Input is not a pathname.")` aborts with exit 101. Verified: `basename /`, `basename ..`, `basename a/b/..` all panic. Spec steps 3/5 require `/`→`/` and `..`→`..`. Fix: implement the slash-trimming algorithm on the raw string instead of delegating to `Path::file_name()`.

#### Major
- [ ] **#B2 — suffix is stripped from the full pathname, not the final component, and the "identical to result" guard is missing.** `basename.rs:29-37`. The code runs `pathname.strip_suffix(suffix)` on the whole argument before extracting the component. Consequences verified: `basename /usr/bin/env env` → `bin` (spec: `env`, because step 6 forbids removing a suffix identical to the whole result); `basename src/dir/ ir` → `dir` (spec: `d`). Fix: perform steps 3-5 first, then apply step 6 to the resulting component only, and skip removal when `suffix == result`.
- [ ] **#B3 — suffix interacts wrongly with trailing slashes.** `basename.rs:31`. Because suffix removal precedes trailing-slash trimming, a trailing `/` makes `strip_suffix` silently no-op. Same root cause as #B2; folds into the same fix.

#### Minor
- [ ] **#B4 — operands are `String`; non-UTF-8 pathnames are rejected by clap.** `basename.rs:22-23`. POSIX pathnames are byte strings. Use `OsString`/bytes (as `dirname` does for the operand).
- [ ] **#B5 — no `--` is documented and leading-`-` operands fail.** `basename.rs:15-24`. `basename -n` is parsed as an unknown option by clap; the spec EXAMPLES rely on `basename -- "$1"`. clap supplies `--`, but a bare `-foo` string operand errors. Minor; spec OPTIONS is "None".
- [ ] **#B6 — newline-in-pathname not treated as an error (FUTURE DIRECTIONS).** Whole-program. Encouraged, not required.

### Detailed conformance matrix

#### Options / Operands / STDIN
- [x] **OPTIONS none** CONFORMS — no options defined. `basename.rs:15-24`.
- [ ] **`string` operand** PARTIAL — present, but algorithm diverges (#B1-#B3). `basename.rs:26-52`.
- [x] **`suffix` operand present** CONFORMS (mechanically) — optional second operand. `basename.rs:23`. Semantics diverge (#B2).
- [x] **STDIN not used** CONFORMS — never reads stdin. Whole file.
- [x] **empty `string`** CONFORMS — prints empty line; spec allows `.` or null. `basename.rs:39-42`.
- [x] **`string == "."`** CONFORMS — prints `.`. `basename.rs:39`.

#### Algorithm steps (DESCRIPTION 1-6)
- [x] step 1 (null string) — handled. `basename.rs:39`.
- [ ] **step 2 (`//`)** PARTIAL — falls through to `file_name()`; impl-defined, but currently yields `Path::file_name("//")` = `None` → panic. Folds into #B1.
- [ ] **step 3 (all slashes → `/`)** DIVERGES — panics instead of printing `/`. #B1.
- [x] step 4 (trailing slash) — `file_name()` trims trailing slash for normal names. `basename.rs:48`.
- [x] step 5 (strip prefix) — `file_name()` does this for normal names. `basename.rs:48`.
- [ ] **step 6 (suffix removal)** DIVERGES — wrong order, missing identical-guard. #B2.

#### STDOUT / STDERR / Exit / Environment
- [x] **STDOUT `"%s\n"`** CONFORMS — `println!`. `basename.rs:46`.
- [ ] **STDERR diagnostics only** PARTIAL — the only error path is a Rust panic to stderr (#B1), not a clean diagnostic.
- [ ] **EXIT STATUS 0/>0** PARTIAL — success=0, but error path is panic=101 rather than a controlled `>0`. #B1.
- [x] **`setlocale`/`textdomain`** CONFORMS — `basename.rs:55-57`.
- [ ] `LC_MESSAGES` MINOR — no runtime diagnostics to translate (panic text is hardcoded). #B1.

### Test coverage signal
Not covered:
- [ ] `basename /`, `basename //`, `basename ///` (panic — #B1)
- [ ] `basename ..`, `basename x/..` (panic — #B1)
- [ ] suffix identical to result (`env env` — #B2)
- [ ] suffix + trailing slash (#B3)
- [ ] non-UTF-8 operand (#B4)

---

## `dirname`

**Implementation:** `pathnames/dirname.rs` (52 lines)
**Tests:** `pathnames/tests/dirname/mod.rs` (82 lines, 6 `#[test]`s)
**Spec:** POSIX.1-2024, Vol. 3 §3, pp. 2852–2854
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/dirname.md`

### TL;DR

The cleanest utility in the crate. `PathBuf::pop()` plus an empty→`.` fallback
reproduces the spec's lexical behavior on every case tested, including trailing
slashes, no-slash → `.`, root → `/`, empty → `.`, and `..` as the final
component. The spec explicitly permits elision of redundant `/` and `.`
components (which `PathBuf` does) and forbids removing non-final `..` (which
`pop()` never does). Only minor gaps remain.

### Priority issues

#### Minor
- [ ] **#D1 — output via `to_string_lossy()` mangles non-UTF-8 directory names.** `dirname.rs:34,39`. The operand is correctly an `OsString`, but the result is printed lossily. Use `OsStr`/byte-faithful output.
- [ ] **#D2 — `//` handling is not the rationale-preferred `//`.** `dirname.rs:31-39`. Rust normalizes `//`; the spec rationale says `dirname //` should be `//` where `//` is distinct. Implementation-defined — low priority.
- [ ] **#D3 — newline-in-pathname not treated as error (FUTURE DIRECTIONS).** Encouraged, not required.

### Detailed conformance matrix

#### Options / Operands / STDIN
- [x] **OPTIONS none** CONFORMS — `dirname.rs:16-23`.
- [x] **`string` operand** CONFORMS — single operand, treated as a path. `dirname.rs:21-22`.
- [x] **STDIN not used** CONFORMS.

#### DESCRIPTION
- [x] **no `/` → `.`** CONFORMS — `dirname filename` → `.`. `dirname.rs:35-37`. (tested)
- [x] **empty → `.`** CONFORMS — `dirname.rs:26-29`. (tested)
- [x] **trailing slash not counted** CONFORMS — `dirname /usr/bin/` → `/usr`. `dirname.rs:31-32`. (tested)
- [x] **root `/` → `/`** CONFORMS — `pop()` no-ops on `/`. (tested)
- [x] **no pathname resolution / file-type independence** CONFORMS — purely lexical. `dirname.rs:31-32`.
- [x] **non-final `..` preserved** CONFORMS — `pop()` only removes the last component; `dirname a/../b` → `a/..`.

#### STDOUT / STDERR / Exit / Environment
- [x] **STDOUT `"%s\n"`** CONFORMS — `dirname.rs:39`.
- [x] **EXIT STATUS** CONFORMS — returns 0; no error paths.
- [x] **`setlocale`/`textdomain`** CONFORMS — `dirname.rs:43-45`.

### Test coverage signal
Not covered:
- [ ] non-final `..` preservation (`a/../b` → `a/..`)
- [ ] redundant-slash input (`/usr//bin`)
- [ ] non-UTF-8 operand (#D1)

---

## `pathchk`

**Implementation:** `pathnames/pathchk.rs` (155 lines)
**Tests:** none (no `pathnames/tests/pathchk/`)
**Spec:** POSIX.1-2024, Vol. 3 §3, pp. 3293–3297
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/pathchk.md`

### TL;DR

`-P` (basic: empty-string + leading-hyphen component) conforms. Everything else
has defects. The **default (filesystem) mode is unusable**: `find_fshandle()`
returns an empty string both when the path already exists and when its only
nonexistent ancestor is the current directory, so `pathconf("")` fails and the
utility reports `pathconf error(path length)` (exit 1) for *every existing file*
and for *every creatable relative name* — verified. `-p` and `-P` are placed in
the same clap `group = "mode"`, making them **mutually exclusive**, directly
contradicting the spec's APPLICATION USAGE ("use both the `-p` and `-P` options
together") and EXAMPLES (`pathchk -p -P {}`). `-p`'s portability check uses
`is_ascii()` rather than the portable-filename character set, so it accepts
spaces, `*`, `$`, etc.

### Priority issues

#### Critical
- [ ] **#P1 — default mode reports `pathconf error` for existing files.** `pathchk.rs:88-106,112-126`. `find_fshandle` initializes `fsh = ""` and, when the path already exists, never enters the `while` loop → returns `""` → `pathconf("")` = `-1` → error. Verified: `pathchk /etc/passwd` → `/etc/passwd: pathconf error(path length)`, exit 1. Fix: when the full path (or its parent) exists, use it as the fs handle.
- [ ] **#P2 — default mode rejects creatable relative pathnames.** `pathchk.rs:88-103`. `Path::new("foo").parent()` is `Some("")` (empty), not `Some(".")`; the loop walks to `""`, `pathconf("")` fails. Verified: `pathchk newfile` → `newfile: pathconf error(path length)`, exit 1 — but the spec says a creatable path "shall not be considered an error". Fix: treat an empty parent as `"."` (cwd).

#### Major
- [ ] **#P3 — `-p` and `-P` are mutually exclusive.** `pathchk.rs:24,35` (`group = "mode"` on both). Verified: `pathchk -p -P foo` → clap error, exit 2. Spec APPLICATION USAGE: "applications should use both the `-p` and `-P` options together." Fix: drop the shared group; the three modes (`-p`, `-P`, default) are independent and combinable per the spec.
- [ ] **#P4 — `-p` uses `is_ascii()` instead of the portable filename character set.** `pathchk.rs:78`. The portable set is `[A-Za-z0-9._-]` only. Verified: `pathchk -p 'a b'` → exit 0 (space accepted; spec requires a diagnostic). Fix: reject any component byte outside `[A-Za-z0-9._-]`.
- [ ] **#P5 — default mode never checks "component in a directory that is not searchable" nor "byte sequence not valid in its containing directory."** `pathchk.rs:63-85,112-126`. Only length checks are performed. The search-permission check is a spec bullet; the invalid-byte check is harder (rare on common filesystems) but also mandated. Fix: stat/access ancestor dirs for search permission.

#### Minor
- [ ] **#P6 — `_POSIX_PATH_MAX` is 255; XBD `<limits.h>` minimum is 256.** `pathchk.rs:16`. Off-by-one understates the portable limit. Fix: `256`.
- [ ] **#P7 — diagnostics are hardcoded English.** `pathchk.rs:48,55,70,75,79,117,121` and `eprintln!` at `150`. `LC_MESSAGES` is inert despite `setlocale`. Wrap messages in `gettext`.
- [ ] **#P8 — operands are `String`; non-UTF-8 pathnames rejected by clap.** `pathchk.rs:44`. Portable-filename and byte-validity checks logically operate on bytes; accept `OsString`.
- [ ] **#P9 — `CString::new(fsh).unwrap()` panics if a fs-handle path contains an interior NUL.** `pathchk.rs:114`. Unreachable from clap `String` operands today, but a latent panic. Fix: propagate as a diagnostic.

### Detailed conformance matrix

#### Options
- [ ] **`-p` (portable)** PARTIAL — present but wrong char set (#P4), mutually exclusive (#P3), wrong PATH_MAX const (#P6). `pathchk.rs:22-31,108-110`.
- [x] **`-P` (basic)** CONFORMS — flags empty pathname and any component beginning with `-`. `pathchk.rs:33-41,47-61`.
- [ ] **default (filesystem)** DIVERGES — broken (#P1, #P2), incomplete (#P5). `pathchk.rs:112-126`.
- [x] **`--` end-of-options** CONFORMS — clap provides it (used in spec EXAMPLES).

#### Operands / STDIN / STDOUT / STDERR
- [x] **`pathname...` (multiple)** CONFORMS — loops over all operands in order. `pathchk.rs:147-152`.
- [x] **STDIN not used** CONFORMS.
- [x] **STDOUT not used** CONFORMS — nothing on stdout. 
- [x] **STDERR diagnostics only** CONFORMS (channel) — but English-only (#P7). `pathchk.rs:150`.

#### Exit status / Environment
- [x] **EXIT 0 all pass / >0 error** CONFORMS (mechanism) — accumulator `exit_code`, `exit(1)` on any failure. `pathchk.rs:145-154`. (Defeated in practice by #P1/#P2.)
- [x] **`setlocale`/`textdomain`** CONFORMS — `pathchk.rs:139-141`.
- [ ] `LC_MESSAGES` MINOR — diagnostics not translated (#P7).

### Test coverage signal
**No test file exists for `pathchk`.** Add `pathnames/tests/pathchk/mod.rs` covering:
- [ ] `pathchk <existing-file>` → exit 0 (#P1)
- [ ] `pathchk <creatable-relative-name>` → exit 0 (#P2)
- [ ] `pathchk -p -P foo` → exit 0 (#P3)
- [ ] `pathchk -p 'a b'` and `-p 'a*b'` → exit 1 (#P4)
- [ ] `pathchk -P -foo` and `pathchk -P ''` → exit 1
- [ ] over-long component / over-long path

---

## `realpath`

**Implementation:** `pathnames/realpath.rs` (96 lines)
**Tests:** `pathnames/tests/realpath/mod.rs` (170 lines, 8 `#[test]`s)
**Spec:** POSIX.1-2024, Vol. 3 §3, pp. 3375–3377 (first released Issue 8)
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/realpath.md`

### TL;DR

`-e` is correct (delegates to `std::fs::canonicalize`, which performs full
`realpath()`-equivalent symlink resolution and errors on missing components).
But the **default mode and `-E` mode do not resolve symbolic links at all** —
they call `normalize()`, a purely lexical cargo-derived routine that only folds
`.`/`..` and prepends the cwd. The spec requires the default result to "not
contain any components that refer to files of type symbolic link," and `-E` to
"expand all symbolic links … using the algorithm in XBD §4.16." Verified: with
`link → real`, `realpath /tmp/.../link` and `realpath -E /tmp/.../link` both
print `…/link` (unresolved), while `realpath -e …/link` prints `…/real`. The
existing tests codify this lexical-only behavior, so they will need updating.

### Priority issues

#### Critical
- [ ] **#R1 — default mode (no option) does not resolve symbolic links.** `realpath.rs:34-60,74`. `normalize()` is lexical only. Spec: the result "does not contain any components that refer to files of type symbolic link and does not contain any components that are dot or dot-dot." Verified: `realpath /tmp/rp_test/link` → `/tmp/rp_test/link`. Fix: resolve symlinks (e.g. emulate `realpath()` allowing a missing final component, matching the unspecified-but-symlink-free requirement).

#### Major
- [ ] **#R2 — `-E` does not expand symlinks and never errors on non-ENOENT conditions.** `realpath.rs:72-77`. `-E` shares the default `normalize()` path. Spec `-E`: expand all symlinks via XBD §4.16, fail on any error other than a final-component `[ENOENT]`, ignore trailing slashes. Verified: `realpath -E …/link` → `…/link` (unresolved); `realpath -E /tmp/regfile/` does not raise "Not a directory" (spec RATIONALE example expects it). Fix: implement `-E` as "resolve symlinks; tolerate only a missing last component."
- [ ] **#R3 — tests encode the divergent lexical behavior.** `tests/realpath/mod.rs:140-164` asserts default == `-E` == lexical normalization. These assertions contradict the spec and must be revised when #R1/#R2 land. (Noted so a fix PR isn't blocked by "passing" tests.)

#### Minor
- [ ] **#R4 — accepts multiple `file` operands; spec SYNOPSIS is a single `file`.** `realpath.rs:28-29,71`. Common extension (GNU); harmless but beyond POSIX.
- [ ] **#R5 — missing operand defaults to `.`; spec requires a `file` operand.** `realpath.rs:28`. `realpath` (no args) prints the cwd instead of erroring. Extension; note only.
- [ ] **#R6 — `-q`/`--quiet` is a non-POSIX extension.** `realpath.rs:25-26`. Spec defines only `-E`/`-e`. Harmless; document as extension.
- [ ] **#R7 — output via `to_string_lossy()` mangles non-UTF-8 paths.** `realpath.rs:81`. The code comments acknowledge this; use `OsStr::as_bytes` to stdout.
- [ ] **#R8 — diagnostic embeds Rust's `(os error N)` text and `gettext` on a dynamic string is a no-op.** `realpath.rs:84-88`. `LC_MESSAGES` cannot translate it. Use `strerror`/catalog-backed messages.
- [ ] **#R9 — newline-in-pathname not treated as error (FUTURE DIRECTIONS).** Encouraged, not required.

### Detailed conformance matrix

#### Options
- [x] **`-e`** CONFORMS — `fs::canonicalize`; resolves symlinks, errors on ENOENT. `realpath.rs:19,72-73`. (tested)
- [ ] **`-E`** DIVERGES — lexical only, no symlink expansion, no error handling (#R2). `realpath.rs:22-23,74`.
- [x] **`-E`/`-e` last-wins** CONFORMS — `overrides_with` makes the last flag win; "not an error" to repeat. `realpath.rs:19,22`. (tested)
- [ ] **default (no option)** DIVERGES — lexical only (#R1). `realpath.rs:74`.
- [x] **`--` end-of-options** CONFORMS — clap provides it.

#### Operands / STDIN / STDOUT / STDERR
- [ ] **`file` operand (single)** PARTIAL — accepts many (#R4) and defaults to `.` when absent (#R5). `realpath.rs:28-29`.
- [x] **STDIN not used** CONFORMS.
- [x] **STDOUT canonical path + `\n`** CONFORMS (format) — `println!`. `realpath.rs:81`; lossy (#R7).
- [x] **STDERR diagnostics only; nothing on stdout on failure** CONFORMS — `realpath.rs:82-91`.

#### Exit status / Environment
- [x] **EXIT 0 success / >0 failure** CONFORMS — `exit_code |= 1` per failed operand. `realpath.rs:69-95`. (tested)
- [x] **`setlocale`/`textdomain`** CONFORMS — `realpath.rs:63-65`.
- [ ] `LC_MESSAGES` MINOR — diagnostics not translatable (#R8).

### Test coverage signal
Not covered (and #R3 tests need revision):
- [ ] symlink resolution in default and `-E` modes (#R1, #R2)
- [ ] `-E` on `regfile/` (trailing slash → "Not a directory")
- [ ] `-E` on `dir/symlink-to-missing` → expanded target path
- [ ] non-UTF-8 operand (#R7)

---

## Suggested PR groupings

- **PR A — "basename: fix the algorithm"**: #B1 (panic on `/`,`..`,`x/..`), #B2/#B3 (suffix order + identical-guard + trailing slash). Reimplement DESCRIPTION steps 1-6 directly on the raw string; add the missing test cases.
- **PR B — "pathchk: make default mode work"**: #P1 (existing-file `find_fshandle`), #P2 (empty-parent → cwd), #P9 (CString panic). Create `pathnames/tests/pathchk/mod.rs`.
- **PR C — "pathchk: portability checks"**: #P3 (`-p`+`-P` combinable), #P4 (portable char set), #P5 (searchable-dir check), #P6 (`_POSIX_PATH_MAX` = 256).
- **PR D — "realpath: resolve symlinks"**: #R1 (default), #R2 (`-E`), #R3 (revise tests). The behavioral core of the utility.
- **PR E — "pathnames: i18n + non-UTF-8 + extensions"**: #B4/#P8/#D1/#R7 (OsString/byte-faithful I/O), #P7/#R8 (gettext diagnostics), #R5/#R6 documentation of extensions. Low-risk cleanup.
