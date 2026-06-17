# POSIX.1-2024 Conformance & Race-Safety Audit — `tree/` cp, mv, rm

**Scope.** This is a *partial* audit of the `tree/` crate covering only the three
data-destroying, security-critical utilities **`cp`**, **`mv`**, and **`rm`**, plus the
**`ftw/`** dependency crate they rely on for race-free directory traversal. Unlike the
other crate audits, this one adds an explicit **filesystem-race / security-hardening
lens** on top of plain POSIX conformance, because these utilities are the canonical
targets of TOCTOU and symlink-swap attacks and the canonical cause of accidental data
loss. **Part II** (appended below, 2026-06-17) extends the audit to the **remaining 13
`tree/` utilities** — `chmod`, `chown`, `chgrp`, `mkdir`, `mkfifo`, `rmdir`, `link`,
`unlink`, `ln`, `readlink`, `du`, `touch`, `ls` — as an **audit-only** pass (findings
recorded, not yet remediated; cp/mv/rm remain the only Stage-6 utilities).

**Method.** Static spec-vs-code (each `shall` read against the cited implementation
line) with `grep` proofs for absence claims, plus `cargo build --release`, the existing
`tree`/`ftw` test suites (baseline: **tree 143 passed / 9 root-ignored; ftw 8 passed**,
2026-06-17), and behavioral spot-checks of every Critical/Major finding against the
release binaries and GNU coreutils. **No source was modified** — this pass is audit-only.

**Specs.** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/{cp,mv,rm}.md`
(POSIX.1-2024, IEEE Std 1003.1-2024, Vol. 3 §3). Spec citations use the absolute line
number printed in the slice.

**ID scheme.** `#F*` = ftw, `#C*` = cp, `#M*` = mv, `#R*` = rm. Status vocabulary:
CONFORMS / PARTIAL / MISSING / DIVERGES / N/A. Severity: **Critical** (data loss, crash,
hang, or race-exploitable security hole) / **Major** (spec behavior absent on a common
path; wrong exit status / I/O channel) / **Minor** (strict-conformance gaps that don't
bite real users today). Every actionable item is `- [ ]`; CONFORMS items are `- [x]`.

---

## Cross-cutting: `ftw` race-free foundation

**Implementation:** `ftw/src/lib.rs` (1043 lines), `ftw/src/dir.rs` (263 lines)
**Tests:** `ftw/tests/integration.rs`, `ftw/tests/integration2.rs` (8 tests)
**Date:** 2026-06-17

### TL;DR
`ftw` is genuinely well-engineered for the *ancestor* class of races: traversal is an
iterative DFS that pins each parent directory to an **open file descriptor** and performs
every child operation with `*at` syscalls (`openat`/`fstatat`/`unlinkat`/`readlinkat`)
relative to that fd, so renaming a path component *above* the cursor cannot redirect an
in-progress walk (proven by `test_ftw_path_prefix_modification`). It also solves
fd-exhaustion (a `DeferredDir` mode kicks in near `RLIMIT_NOFILE`), descends to arbitrary
depth without native recursion, and disambiguates `readdir` end-of-stream from error via
`errno` reset. **However, there is one residual race on the *leaf* component**: the
descent `openat` uses bare `O_RDONLY` with no `O_NOFOLLOW`/`O_DIRECTORY` and no post-open
`dev`/`ino` re-verification, leaving a classic symlink-swap window (#F1). Several
`panic!`/`unreachable!()`/`.unwrap()` sites can abort the process on rare I/O races.

### Priority issues

> **Resolved 2026-06-17** (Phase 1, this branch). #F1/#F2/#F4/#F5 fixed; #F3 hardened to fail-closed with a documented deep-tree residual. New regression tests in `ftw/tests/race.rs` (`descent_refuses_dir_swapped_for_symlink`, `descent_refuses_dir_swapped_for_other_dir`, `nonfollowing_walk_still_lists_symlink_entries`) — verified to fail without the fix.

#### Critical
- [x] **#F1 — Leaf-component symlink-swap TOCTOU on directory descent (race → arbitrary-directory deletion via `rm`).** `ftw/src/lib.rs:918` → `ftw/src/dir.rs:94-95` (`OwnedDir::open_at` → `openat(dir_fd, name, O_RDONLY)`). `process_file` captures the entry type with a **non-following** `fstatat` at `lib.rs:506-510`, the consumer's `file_handler` then runs (for `rm -i` this *blocks on a prompt*, widening the window unboundedly), and only afterward does the engine open the child for descent. The descent open carries **no `O_NOFOLLOW` and no `O_DIRECTORY`**, and the resulting fd is **never `fstat`'d and compared** against the metadata captured pre-descent. An attacker who can replace a subdirectory entry with a symlink in the window between the stat and the open redirects the walk into an arbitrary directory; for `rm -r` the subsequent `unlinkat(new_dir_fd, …)` calls then delete the *attacker-chosen* directory's contents. ✓ **Fixed:** descent opens now use `O_DIRECTORY` (+ `O_NOFOLLOW` when not following symlinks) via a threaded `descent_flags`, and the `OwnedDir` path `fstat`s the opened fd and aborts the descent on `(dev, ino)` mismatch with the pre-descent stat (`ftw/src/lib.rs` descent block; `ftw/src/dir.rs` `OwnedDir::open_at`/`DeferredDir`). The root operand keeps `O_DIRECTORY` only so a symlinked directory argument is still honored per `follow_symlinks_on_args`. **Residual:** the rare fd-conserving `DeferredDir` path has no captured `(dev, ino)` baseline, so it relies on `O_NOFOLLOW` alone (fail-closed) and does not detect a swap to a *different real directory* — see #F3.
- [x] **#F5 — Symlink cycles now detected on all paths.** ✓ **Fixed:** when `follow_symlinks` is set, before descending, the candidate `(dev, ino)` is checked against the active ancestor chain (`stack`) and a cycle is reported as `ELOOP` instead of looping until the fd budget forces the deferred path. (Originally filed Minor: the `visited: HashSet<ino_t>` lived only on `DeferredDir`.)

#### Minor
- [x] **#F2 — `panic!` on `getrlimit` failure aborts the process.** `ftw/src/lib.rs` (was `panic!("{}", io::Error::last_os_error())`). ✓ **Fixed:** falls back to a conservative `FALLBACK_FD_LIMIT` (1024) and continues; threshold computed with `saturating_sub`.
- [x] **#F3 — `DeferredDir` fd re-open uses `.unwrap()` — a mid-traversal race panics.** `ftw/src/dir.rs` (`OwnedDir::new(...).unwrap()`, `open_long_filename(...).unwrap()`, `FileDescriptor::open_at(...).unwrap()`). ✓ **Hardened (partial):** the deferred reopen now applies `O_DIRECTORY`/`O_NOFOLLOW`, so a swapped leaf **fails closed** (a panic that aborts the walk) rather than silently redirecting it. **Residual:** converting the remaining deep-tree-only reopen-failure panic into a graceful `err_reporter` call requires a fallible reopen API threaded through three expression-context call sites (`lib.rs` `dir_fd` resolution); deferred as a low-impact follow-up (only reachable beyond `RLIMIT_NOFILE − 7` simultaneously-open directories).
- [x] **#F4 — `unreachable!()` on unknown `S_IFMT` aborts the process.** `ftw/src/lib.rs:162` (`Metadata::file_type`). ✓ **Fixed:** added `FileType::Unknown` and return it for unrecognized `S_IFMT` bits; `tree/rm.rs` gained an `Unknown` prompt arm; the `ls` matches already had `_` catch-alls.

### Items closed by design (what ftw already guarantees)

| Concern | Status | Mechanism |
|---|---|---|
| Ancestor path-component swap redirects walk | **closed** | parent dir-fd pinning + `*at` child ops; `test_ftw_path_prefix_modification` (`integration.rs:334`). |
| `unlinkat`/`openat`/`fstatat` race-relative to parent | **closed** | every child op takes `dir_fd()` + leaf name, never a re-resolved absolute path. |
| fd exhaustion on wide/deep trees | **closed** | `DeferredDir` engages at `RLIMIT_NOFILE − 7` (`lib.rs:789-874`); `test_ftw_fd_raii`, `test_ftw_too_many_open_files`. |
| Arbitrary depth without `{PATH_MAX}` failure | **closed** | iterative DFS + `open_long_filename` component walk (`lib.rs:596-665`); `test_ftw_deep` (depth 300). |
| `readdir` EOF vs error ambiguity | **closed** | `errno` zeroed before each `readdir` (`dir.rs:124,219`). |
| Final-component `unlink` following a symlink | **closed** | `unlinkat(..., 0)`/`AT_REMOVEDIR` never follows the leaf; only the *descent* open (#F1) does. |

---

## Cross-cutting: shared copy engine, prompts, i18n

**Implementation:** `tree/common/copy.rs` (838 lines, used by cp + mv), `tree/common/mod.rs` (55).

### Path-based (non-race-hardened) seams
The recursive engines operate through ftw's dir-fd model, but several **top-level and
control-flow operations resolve a user path from the root** and so are not race-hardened:

| Site | Operation | Note |
|---|---|---|
| `cp.rs:118`, `mv.rs:391` | `fs::metadata(target)` | target-type probe before dispatch (follows symlinks). |
| `mv.rs:107,125,152-154` | `ftw::Metadata::new(AT_FDCWD, full-path, …)` | source/target stats by full path, not dir-fd-relative. |
| `mv.rs:228` | `std::fs::rename(source, target)` | same-filesystem move (atomic; conforms — see #M6). |
| `mv.rs:273-286,352-356,421-424` | `fs::remove_dir`/`remove_file`/`remove_dir_all` | cross-device target removal & source cleanup by path. |
| `mv.rs:352,420` | `source.is_dir()` | **follows symlinks** when choosing `remove_dir_all` vs `remove_file` (#M3). |
| `rm.rs:402` | `fs::symlink_metadata` | top-level classify (no follow). |
| `rm.rs:388` | `fs::remove_file(filepath)` | top-level single-file unlink by path (#R4). |
| `rm.rs:246` | `fs::canonicalize` | `/`-guard only. |

These are largely unavoidable for *operands* (the user hands `cp`/`mv`/`rm` a path, not an
fd), but they widen the top-level TOCTOU window relative to the recursive interior.

### Prompts & i18n (applies to all three)
- [x] Prompts and diagnostics are written to **stderr**; stdout is untouched — verified behaviorally for `rm -i` (prompt on stderr, stdout empty). Conforms to cp STDERR (90768-90770), mv STDERR (108164-108167), rm STDERR (113448-113451).
- [x] `setlocale(LC_ALL, "")` + `textdomain` are called in every `main` (`cp.rs:101`, `mv.rs:371`, `rm.rs:427`), so `LANG`/`LC_ALL`/`LC_MESSAGES` select the message catalog.
- [x] **Affirmative responses are hardcoded `response.to_lowercase().starts_with('y')`**, ignoring the `LC_MESSAGES` `yesexpr` ERE the spec ties to "process affirmative responses" (cp 90758-90761, mv 108154-108157, rm 113438-113441). (#C8/#M5/#R6) ✓ **Fixed (Phase 7):** new shared `plib::locale::is_affirmative` matches the locale's `nl_langinfo(YESEXPR)` ERE (fallback `^[yY]`); cp/mv/rm `prompt_user` all use it. Test: `plib::locale::tests::is_affirmative_basic`.
- [x] **`prompt_user` does `io::stdin().read_line(...).unwrap()`** — a hard read error or EOF panics the utility mid-operation. ✓ **Fixed (Phase 7):** `read_line(...).unwrap_or(0)` and an EOF/`0` read is treated as a non-affirmative response (no panic).

---

## `cp`

**Implementation:** `tree/cp.rs` (157) + `tree/common/copy.rs` (838, shared)
**Tests:** `tree/tests/cp/mod.rs` (977 lines)
**Spec:** POSIX.1-2024, Vol. 3 §3, pp. 2789–2796
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/cp.md`
**Date:** 2026-06-17

### TL;DR
The core copy algorithm faithfully follows the spec's step structure (1–4) and does the
real work through race-free `*at` syscalls relative to ftw dir-fds (mkdirat / openat /
symlinkat / mknodat / utimensat / fchownat / fchmodat). Two correctness/security defects
stand out. **Critical:** `cp -p` re-applies the source's `S_ISUID`/`S_ISGID` via
`fchmodat` even when the `chown` to duplicate ownership has failed — the exact
privilege-leak POSIX 90720-90721 forbids (behaviorally confirmed: `cp -p /usr/bin/passwd`
→ mode `4755` owned by the caller). **Major:** with more than two operands and a target
that is not an existing directory, `cp` silently copies only the *first* source and exits
0 instead of erroring (90605-90606), and a per-file failure mid-hierarchy aborts the whole
copy instead of continuing with same-level siblings (90829-90832). Symlink-handling
defaults, same-file detection, special-file creation, and `-i`/`-f` prompting otherwise
conform.

### Priority issues

> **Resolved 2026-06-17** (this branch). All cp items closed: #C1 (Phase 2), #C2 (Phase 3), #C3 (Phase 4), #C8 (Phase 7); #C4/#C5/#C6 documented WON'T-FIX. Regression tests in `tree/tests/cp/mod.rs`.

#### Critical
- [x] **#C1 — `cp -p` does not clear `S_ISUID`/`S_ISGID` when ownership can't be duplicated (privilege leak).** `tree/common/copy.rs:793-803` ignored the `fchownat` result, then `:806-811` applied the **full source mode** via `fchmodat`, re-setting the setuid/setgid bits on a file now owned by the copying user. POSIX 90720-90721: *"If the user ID or the group ID cannot be duplicated, the file permission bits S_ISUID and S_ISGID shall be cleared."* Behaviorally: as non-root, `cp -p /usr/bin/passwd ./x` → `./x` ended up `-rwsr-xr-x caller:caller` (`4755`); GNU clears it to `0755`. ✓ **Fixed (Phase 2):** `copy_characteristics` now records whether `fchownat` succeeded and masks `~(S_ISUID|S_ISGID)` out of the mode before `fchmodat` when it did not. Re-verified: `cp -p /usr/bin/passwd` now yields `0755` (matches GNU); same-owner `cp -p` still preserves the bits. Tests: `test_cp_preserve_keeps_setuid_same_owner` (non-root) and root-gated `test_cp_preserve_clears_setuid_on_chown_fail` in `tree/tests/cp/mod.rs`.

#### Major
- [x] **#C2 — >2 operands with a non-directory target silently copies only the first source.** `tree/cp.rs`: when the target was not a directory, `main` took the single-file branch and dropped `sources[1..]`, exiting 0. POSIX 90605-90606: *"It shall be an error if target does not exist and more than two operands are specified, or if target exists and does not name a directory."* ✓ **Fixed (Phase 3):** `main` now diagnoses `target '…' is not a directory` and exits 1 when `!dir_exists && sources.len() > 1`, before any copy. Test: `test_cp_multi_source_nondir_target`.
- [x] **#C3 — A per-file failure aborts the entire hierarchy copy instead of continuing with same-level siblings.** `tree/common/copy.rs` set `terminate = true` on the first `copy_file_impl` error, short-circuiting every subsequent entry. POSIX 90829-90832: *"When a failure occurs during the copying of a file hierarchy, cp is required to attempt to copy files that are on the same level in the hierarchy or above the file where the failure occurred."* ✓ **Fixed (Phase 4):** `CopyConfig` gained `continue_on_error` (cp `true`, mv `false`) and `prog`. In cp mode each per-file failure is diagnosed immediately (`{prog}: …`), an exit-status flag is set, and the walk continues; only mv still stops on the first structural error (so it never removes a source it could not fully duplicate). Additionally, **mv 108114-108115** is now honored: a characteristics-duplication failure is never fatal and does **not** change mv's exit status (cp still exits non-zero). Re-verified: `cp -R` over a tree with interspersed unreadable files copies every readable sibling, diagnoses each failure, and exits 1. Test: `test_cp_recursive_continue_on_error`.

#### Minor
- [x] ~~**#C4 — `source/.` trailing form is a non-POSIX extension.**~~ **WON'T FIX:** GNU-compatible (`cp -R dir/.` copies contents), harmless, and covered by `test_cp_dir_slash`. Kept intentionally.
- [x] ~~**#C5 — Special-file destination mode is hardcoded `0o644`.**~~ **WON'T FIX:** POSIX leaves special-file permissions/owner/group **unspecified** (90683-90687, RATIONALE 90833-90839); FIFOs already use the source mode and `-p` overwrites the mode via `copy_characteristics`. Conforms as-is.
- [x] ~~**#C6 — `-H`/`-L` require `-R`.**~~ **WON'T FIX:** POSIX lists `-H`/`-L` only under the `-R` synopsis form, so they are only meaningful with `-R`; requiring it is defensible (`-P` already works without `-R`).

### Detailed conformance matrix

#### SYNOPSIS / operand dispatch
- [x] Three synopsis forms recognized via target-is-directory probe (`cp.rs:117-131`) + `copy_files`/`copy_file`.
- [x] **>2 operands + non-dir target now errors** (#C2 ✓, Phase 3) — `cp.rs` `main`.
- [x] `-` operand treated as a literal filename, not stdin (no special-casing; `PathBuf` operands). Conforms 90731-90736.

#### OPTIONS

| Opt | Status | Notes (file:line) |
|---|---|---|
| `-f` | CONFORMS | `cp.rs:23`; force path at `copy.rs:362-385` (unlink-then-recreate when the fd can't be obtained), per step 3.a.iii. |
| `-H` | CONFORMS | `cp.rs:26-37` → `follow_symlinks_on_args` (`copy.rs:608`). |
| `-i` | CONFORMS | `cp.rs:65`; prompt to stderr before overwriting an existing non-dir (`copy.rs:304-309`), reads stdin. |
| `-L` | CONFORMS | `cp.rs:39-51`, default `true` → `follow_symlinks` (`copy.rs:609`). Default (no `-R`/`-P`) follows the link per 90610-90612. |
| `-P` | CONFORMS | `cp.rs:53-63`; copies the link via `symlinkat` (`copy.rs:392-402`). |
| `-p` | DIVERGES | (#C1, Critical) setuid/setgid not cleared on `chown` failure — `copy.rs:793-811`. Times/owner/mode otherwise preserved via `utimensat`/`fchownat`/`fchmodat` (all `AT_SYMLINK_NOFOLLOW`). |
| `-R`/`-r` | CONFORMS | `cp.rs:71`, `-r` a visible alias of `-R`; special files handled only with `-R` (`copy.rs:71-82`) per the `-R`-vs-removed-`-r` distinction (RATIONALE 90822-90828). |
| mutually-exclusive `-H/-L/-P` | CONFORMS | `overrides_with_all` → last wins (90727-90728). |

#### OPERANDS / STDIN / INPUT FILES
- [x] STDIN used only to read the prompt response (cp.rs `prompt_user`); otherwise unused (90737-90739). Conforms.
- [x] Input files may be any type; special types handled under `-R` (90741).

#### ENVIRONMENT VARIABLES

| Var | Status | Notes |
|---|---|---|
| `LANG` / `LC_ALL` / `LC_MESSAGES` | PARTIAL | catalog selection via `setlocale`/`textdomain`; but affirmative-response `yesexpr` is hardcoded `y` (#C8). |
| `LC_COLLATE` / `LC_CTYPE` | MISSING | only relevant to the unimplemented `yesexpr` ERE; no other use. |
| `NLSPATH` (XSI) | N/A | gettext domain bound via `textdomain`, not `NLSPATH`. |

#### ASYNCHRONOUS EVENTS / STDOUT / STDERR
- [x] Default signal handling (90763-90764); cp is non-interactive apart from `-i`/`-f` prompts. `grep -nE 'SIGINT|signal|libc::signal' tree/cp.rs tree/common/copy.rs` → 0 matches (expected). Note CONSEQUENCES OF ERRORS 90781-90783 already warns partial copies are possible on signal.
- [x] STDOUT not used (90766). STDERR = prompts + diagnostics only (90768-90770); prompt contains the destination pathname (`copy.rs:284-309`).

#### EXIT STATUS / CONSEQUENCES OF ERRORS
- [x] 0 on success, >0 on error (90776-90779); `copy_files` accumulates failures and `main` exits 1 (`cp.rs:135-138`). A non-affirmative `-i` response is **not** an error (`CopyResult::Skipped`, `copy.rs:558`), matching 90777-90778.
- [x] (#C3 ✓, Phase 4) On error mid-hierarchy, cp now continues with same-level/above entries (90829-90832).

### Race-safety & security (cp)
- **Hardened:** all in-tree file creation/overwrite is dir-fd-relative (`openat`/`mkdirat`/`symlinkat`/`mknodat` against ftw `target_dirfd` and `source.dir_fd()`), so the recursive interior is not path-re-resolving.
- **Residual:** inherits ftw #F1 (descent open without `O_NOFOLLOW`) — under `-R` a swapped directory→symlink can redirect copies; under `-L`/`-H` symlink following compounds it. Top-level `fs::metadata(target)` (`cp.rs:118`) is a path probe.
- **Security:** #C1 (setuid/setgid leak) is the headline. The created-file just-overwritten guard (`copy.rs:243-250`) prevents a source from clobbering a file cp created this run.

### Test coverage signal
- [x] setuid/setgid cleared under `cp -p` when `chown` fails (#C1) — `test_cp_preserve_clears_setuid_on_chown_fail` (root-gated) + `test_cp_preserve_keeps_setuid_same_owner`.
- [x] error for `cp a b non_dir_target` with >2 operands (#C2) — `test_cp_multi_source_nondir_target`.
- [x] same-level siblings still copied after a per-file error (#C3) — `test_cp_recursive_continue_on_error`.
- [x] Symlink, dangling-symlink, hard-link, preserve-times, special-file, trailing-slash, copy-into-self paths are exercised (`tree/tests/cp/mod.rs`).

### Suggested PR groupings
- **PR A — "cp/mv setuid-leak fix"**: #C1 + #M1 (shared `copy_characteristics`). Mask `S_ISUID|S_ISGID` when `fchownat` fails.
- **PR B — "cp/mv multi-operand target validation"**: #C2 + #M2.
- **PR C — "cp continue-on-error per POSIX 90829"**: #C3.
- **PR D — "i18n: yesexpr + prompt read hardening"**: #C8/#M5/#R6 (shared across all three).

---

## `mv`

**Implementation:** `tree/mv.rs` (434) + `tree/common/copy.rs` (838, cross-device path)
**Tests:** `tree/tests/mv/mod.rs` (1457 lines)
**Spec:** POSIX.1-2024, Vol. 3 §3, pp. 3246–3251
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/mv.md`
**Date:** 2026-06-17

### TL;DR
Same-filesystem moves go through `std::fs::rename` (atomic, preserves the inode and all
metadata) and the prompt/same-file/dir-mismatch precondition checks closely track the
spec's steps 1–5; the cross-device fallback duplicates the hierarchy (via the shared cp
engine with `preserve = true`, `dereference = false`, so links are copied as links and
hard-link structure is kept via `linkat`) and then removes the source, satisfying the
CONSEQUENCES-OF-ERRORS guarantee that source-or-destination is always left complete.
**Critical:** the cross-device duplication inherits #C1 — it does not clear
`S_ISUID`/`S_ISGID` when ownership can't be duplicated, which mv 108104-108105 forbids.
**Major:** like cp, `mv a b c` with `c` not an existing directory silently moves only the
first source (removing it) and exits 0.

### Priority issues

> **Resolved 2026-06-17** (this branch). All mv items closed: #M1 (Phase 2), #M2 + trailing-slash (Phase 3), characteristics exit-status (Phase 4), #M3/#M4/#M5 (Phase 7). Regression tests in `tree/tests/mv/mod.rs`.

#### Critical
- [x] **#M1 — Cross-filesystem `mv` does not clear `S_ISUID`/`S_ISGID` when ownership can't be duplicated.** `tree/mv.rs:80` sets `preserve = true` and routes through the shared `copy_characteristics` (`copy.rs`), which had the #C1 defect. POSIX 108104-108105: *"If the user ID, group ID, or file mode of a regular file cannot be duplicated, the file mode bits S_ISUID and S_ISGID shall not be duplicated."* ✓ **Fixed (Phase 2):** closed by the shared `copy_characteristics` fix (#C1).

#### Major
- [x] **#M2 — >2 operands with a non-directory target silently moves only the first source.** `tree/mv.rs`: the single-move branch dropped `sources[1..]`. Behaviorally: `mv a b c` (c not a dir) → `a` renamed to `c` (**`a` gone**), `b` ignored, exit 0. More damaging than #C2 because the first source is actually removed. ✓ **Fixed (Phase 3):** `main` rejects `!dir_exists && sources.len() > 1` with `target '…' is not a directory` (exit 1) before moving; additionally the first-synopsis-form trailing-slash rule (108049-108050) now errors for a non-directory source. Tests: `test_mv_multi_source_nondir_target`, `test_mv_nondir_source_trailing_slash`.

#### Minor
- [x] **#M3 — Cross-device source cleanup uses path-based `source.is_dir()` (follows symlinks).** Choosing `fs::remove_dir_all` vs `fs::remove_file` via `source.is_dir()` dereferenced a symlink source. ✓ **Fixed (Phase 7):** both cleanup sites classify with `fs::symlink_metadata` so a symlink source is removed as a link (`remove_file`), never `remove_dir_all` on its target.
- [x] **#M4 — `EXDEV` detection re-reads global errno.** Used `std::io::Error::last_os_error().raw_os_error()` rather than the captured `e` from `fs::rename`. ✓ **Fixed (Phase 7):** uses `e.raw_os_error()`.

### Detailed conformance matrix

#### SYNOPSIS / operand dispatch
- [x] Two forms via target-is-existing-directory probe (`mv.rs:390-402`); destination = `target.join(file_name)` (108054-108056).
- [x] **>2 operands + non-dir target now errors** (#M2 ✓, Phase 3).
- [x] **Trailing-slash on a non-directory source→target_file now enforced** (Phase 3). POSIX 108049-108050: a non-directory source with a `target_file` ending in `/` is an error.

#### OPTIONS

| Opt | Status | Notes (file:line) |
|---|---|---|
| `-f` | CONFORMS | `mv.rs:30`; suppresses the prompt; `overrides_with_all` (last of `-f`/`-i` wins, 108126-108127). |
| `-i` | CONFORMS | `mv.rs:33`; prompt logic `mv.rs:143` exactly matches the RATIONALE pseudo-code `exists AND !f AND ((!writable AND tty) OR i)` (108206-108207). |

#### Steps 1–7 (DESCRIPTION 108059-108118)
- [x] Step 1 prompt — `mv.rs:143-149`. CONFORMS.
- [x] Step 2 same dirent / hard-link-to-self — `mv.rs:152-174` compares `dev`/`ino` on both raw and dereferenced metadata and issues a diagnostic (the 108071-108073 option (b)). CONFORMS.
- [x] Step 3 `rename(2)` — `mv.rs:228`; `EINVAL`→"subdirectory of itself", other non-`EXDEV` errors diagnosed and skipped (108082-108086). CONFORMS.
- [x] Step 4 dir/non-dir mismatch + just-created guard — `mv.rs:186-225`. CONFORMS (the just-created overwrite is the 108090-108092 "unspecified" case, handled as an error like coreutils).
- [x] Step 5 remove existing destination before cross-device copy — `mv.rs:273-286`. CONFORMS.
- [x] Step 6 duplicate hierarchy, links-as-links — `mv.rs:68-92` (`dereference=false`); hard links preserved via `linkat`. The 108114-108115 rule "duplication-of-characteristics failure shall not modify exit status" is now honored (Phase 4): a `copy_characteristics` error is diagnosed but no longer sets `terminate` or the exit status for mv.
- [x] Step 7 remove source hierarchy — `mv.rs:350-365,418-425`. CONFORMS.

#### ENVIRONMENT / ASYNC / STDOUT / STDERR / EXIT
- Same as cp: catalog via `setlocale`; `yesexpr` hardcoded (#M5). STDOUT unused (108162); prompts+diagnostics to stderr (108164-108167). Exit 0/`>0` (108173-108176); `move_files` accumulates failures (`mv.rs:336,345,363`).
- [x] **#M6 — CONSEQUENCES OF ERRORS (108178-108181)** — *"shall not modify both source_file and the destination path simultaneously; termination at any point shall leave either source_file or the destination path complete."* Same-fs `rename` is atomic; cross-fs removes the old destination then copies then removes the source, so an interruption always leaves the **source** complete (it is removed last). CONFORMS.

### Race-safety & security (mv)
- **Hardened:** same-fs path is a single atomic `rename(2)`. Cross-fs duplication uses the dir-fd-relative cp engine.
- **Residual:** top-level stats use `ftw::Metadata::new(AT_FDCWD, full-path, …)` (`mv.rs:107,125,152-154`) and `fs::rename`/`fs::remove_*` are path-based — top-level TOCTOU window. Cross-fs interior inherits ftw #F1. `source.is_dir()` symlink-follow (#M3).
- **Security:** #M1 (setuid/setgid leak across filesystems).

### Test coverage signal
- [x] `mv a b non_dir_target` (>2 operands) errors (#M2) — `test_mv_multi_source_nondir_target`.
- [x] setuid/setgid clearing when ownership can't be duplicated (#M1) — covered by the shared `copy_characteristics` fix and `test_cp_preserve_clears_setuid_on_chown_fail` (mv cross-device uses the same engine; a dedicated cross-fs root test would add `OTHER_PARTITION_TMPDIR` plumbing — deferred).
- [x] non-directory source with a trailing-slash `target_file` (108049-108050) — `test_mv_nondir_source_trailing_slash`.
- [x] atomic move, cross-device (`part_rename`/`part_hardlink`), hard-link preservation, same-file/symlink-onto-self, dir-vs-file, into-self, fd-leak paths are exercised (`tree/tests/mv/mod.rs`).

### Suggested PR groupings
- **PR A** (shared with cp): #M1. **PR B** (shared with cp): #M2. **PR E — "mv path-class & errno hygiene"**: #M3, #M4, trailing-slash check.

---

## `rm`

**Implementation:** `tree/rm.rs` (453)
**Tests:** `tree/tests/rm/mod.rs` (1209 lines)
**Spec:** POSIX.1-2024, Vol. 3 §3, pp. 3382–3387
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/rm.md`
**Date:** 2026-06-17

### TL;DR
`rm`'s recursive engine is the most race-conscious of the three: it removes entries with
`unlinkat` relative to ftw's pinned parent dir-fd, never follows symlinks
(`TraverseDirectoryOpts::default()` — all-false), guards `.`/`..` and `/`, satisfies the
arbitrary-depth / fd-budget requirements through ftw, and implements the two-prompt
`-r -i` descend/remove behavior with the exact RATIONALE pseudo-code for write-protected
prompting. Its gaps are **missing options**, not broken logic: POSIX.1-2024 adds **`-d`**
(remove empty directories, Austin Group Defect 802) and **`-v`** (verbose, Defects
1154/1365/1487) and rm implements **neither** — both are rejected as unknown arguments.
The single-file (non-recursive) removal path is also path-based rather than
dir-fd-relative, and a handful of `unreachable!()`/`unwrap()` sites are process-aborting.

### Priority issues

> **Resolved 2026-06-17** (this branch). All rm items closed: #R1/#R2 (Phase 5), #R3/#R4/#R5 (Phase 6), #R6 (Phase 7). Regression tests in `tree/tests/rm/mod.rs`.

#### Major
- [x] **#R1 — `-d` (remove empty directories) is unimplemented.** POSIX SYNOPSIS `rm [-diRrv] file...` (113356), OPTIONS 113404, DESCRIPTION 113369-113370 (Austin Group Defect 802). ✓ **Fixed (Phase 5):** added `-d/--dir`; a directory operand with `-d` and without `-r`/`-R` is removed via `fs::remove_dir` (rmdir) after the standard prompt, failing with `Directory not empty` on a non-empty directory; `-r`/`-R` take precedence (113534-113535). Tests: `test_rm_d_empty`, `test_rm_d_nonempty`, `test_rm_dr_precedence`.
- [x] **#R2 — `-v` (verbose) is unimplemented.** POSIX SYNOPSIS 113356, OPTIONS 113412-113413, STDOUT 113445-113447 (Austin Group Defects 1154/1365/1487). ✓ **Fixed (Phase 5):** added `-v/--verbose`; each successful removal writes `removed '…'` / `removed directory '…'` to stdout, wired into `rm_file`, the recursive file/postprocess closures, the direct empty-dir removal, and the `-d` path. Tests: `test_rm_v_file`, `test_rm_dv_empty_dir`.

#### Minor
- [x] **#R3 — No operands without `-f` exits 0 silently.** POSIX SYNOPSIS form 1 is `rm [-diRrv] file...` (≥1 operand); only the `-f` form (113357) permits zero. ✓ **Fixed (Phase 6):** `main` now diagnoses `rm: missing operand` and exits 1 when no operands and `-f` is absent; the `-f` no-operand case stays silent/0. Tests: `test_rm_no_operand`, `test_rm_f_no_operand`.
- [x] **#R4 — Top-level single-file removal is path-based (not dir-fd-relative).** `tree/rm.rs` classified with `fs::symlink_metadata` then removed via `fs::remove_file` on the full path. ✓ **Fixed (Phase 6):** new `open_parent` opens the operand's parent directory (`O_RDONLY|O_DIRECTORY`) and `rm_file` now `fstatat`/`unlinkat`s the basename relative to that pinned parent fd, narrowing the classify→unlink TOCTOU window and matching the recursive interior's model.
- [x] **#R5 — Process-aborting `unreachable!()` sites.** `tree/rm.rs` `should_remove_file` Directory arm and the `ReadLink` error arm. ✓ **Fixed (Phase 6):** both degrade to a safe fallback (a generic prompt / a diagnostic) instead of `unreachable!()`.

### Detailed conformance matrix

#### SYNOPSIS / OPTIONS

| Opt | Status | Notes (file:line) |
|---|---|---|
| `-d` | CONFORMS | (#R1 ✓) `rm_dir_empty` — rmdir an empty directory operand, `-r`/`-R` precedence. |
| `-f` | CONFORMS | `rm.rs:28`; suppresses prompt; `NotFound` swallowed (`rm.rs:406`); `overrides_with_all` clears prior `-i` (113405-113407). |
| `-i` | CONFORMS | `rm.rs:31`; prompts even when stdin is not a terminal (113472-113474). |
| `-R` | CONFORMS | `rm.rs:34` (`-R` a visible alias of `-r`). |
| `-r` | CONFORMS | `rm.rs:34`; required for directory recursion (`rm.rs:227`). |
| `-v` | CONFORMS | (#R2 ✓) `report_removed` writes each removed name to stdout. |

#### DESCRIPTION steps (113363-113400)
- [x] Step 1 nonexistent operand — diagnosed unless `-f` (`rm.rs:404-416`). CONFORMS.
- [x] Step 2 directory — requires `-r`/`-R` (`rm.rs:227-233`); empty-vs-nonempty split and the descend/remove two-prompt model (`process_directory` `rm.rs:173-219`, `descend_into_directory`, `should_remove_directory`). The `-d`-without-`-r` sub-case (113369-113370) is the missing #R1.
- [x] Step 2c symlinks not traversed — `TraverseDirectoryOpts::default()` (`rm.rs:373`, all-false) (113379-113381, 113529-113531). CONFORMS.
- [x] Step 3 file prompt — `ask_for_prompt` (`rm.rs:67-69`) matches the RATIONALE pseudo-code `!f AND ((!writable AND tty) OR i)` (113494-113495). CONFORMS.
- [x] Step 4 `remove()`-equivalent — `unlinkat(dir_fd, name, 0)` for files (`rm.rs:281`), `unlinkat(..., AT_REMOVEDIR)` for dirs (`rm.rs:184,303`). CONFORMS.
- [x] dot/dot-dot and root-directory guard — `rm.rs:236-258` (113360-113362, 113466-113468). CONFORMS.
- [x] Arbitrary-depth / no `{PATH_MAX}` failure / fd-budget — delegated to ftw (113398-113400, 113523-113528). CONFORMS (modulo ftw #F2/#F3 panics).

#### ENVIRONMENT / ASYNC / STDOUT / STDERR / EXIT
- ENV: catalog via `setlocale`; `yesexpr` hardcoded `y` (#R6, `rm.rs:55`).
- [x] ASYNCHRONOUS EVENTS Default (113443-113444); no signal handlers (`grep -nE 'SIGINT|signal' tree/rm.rs` → 0).
- [x] STDERR = prompts + diagnostics (113448-113451); prompt contains the pathname and goes to **stderr** (verified). STDOUT used only by `-v` (113445-113447) — moot until #R2.
- [x] EXIT 0/`>0`; per-operand errors set `exit_code = 1` and processing continues (`rm.rs:438-450`). CONFORMS. CONSEQUENCES OF ERRORS = Default (113463-113464).

### Race-safety & security (rm)
- **Hardened:** the recursive path removes via `unlinkat` against ftw's pinned `entry.dir_fd()`; symlinks are never followed; `.`/`..`/`/` are guarded; `ENOTEMPTY` floods are suppressed (`rm.rs:317`).
- **Residual:** (1) **ftw #F1** — the descent `openat` lacking `O_NOFOLLOW` is the one place `rm -r` can be redirected into an attacker's directory; this is the most consequential race in the whole `tree`/`ftw` surface because the consequence is *deletion*. (2) **#R4** top-level single-file path is not dir-fd-relative. (3) #R1's absence means there is no race-free "file-or-empty-dir" primitive (the very gap 113532-113535 calls out).
- **Security note:** the `-i` prompt between ftw's stat and the descent open (#F1) makes the rm race window arbitrarily wide when prompting is enabled.

### Test coverage signal
- [x] `-d` / `-v` (#R1, #R2) — `test_rm_d_empty`, `test_rm_d_nonempty`, `test_rm_dr_precedence`, `test_rm_v_file`, `test_rm_dv_empty_dir`.
- [x] bare `rm` (no operands, no `-f`) errors (#R3) — `test_rm_no_operand`, `test_rm_f_no_operand`.
- [x] descent-open symlink-swap race (#F1) — `ftw/tests/race.rs` deterministically swaps a directory for a symlink/other-dir inside the handler (no concurrency needed) and is verified to fail without the fix.
- [x] cycle, dangling-symlink, deep tree, write-protected dir, dot-relative, empty/inaccessible, readdir-bug, `-i`/`-f`/`-r` combinations, root-link, EACCES paths are exercised (`tree/tests/rm/mod.rs`).

### Suggested PR groupings
- **PR F — "rm -d (Austin Group Defect 802)"**: #R1.
- **PR G — "rm -v (Austin Group Defects 1154/1365/1487)"**: #R2 (+ newline-in-pathname FUTURE-DIRECTIONS guard).
- **PR H — "rm operand & panic hygiene"**: #R3, #R5, and #R4 (dir-fd-relative single-file removal).
- **PR I — "ftw race hardening"**: #F1 (`O_NOFOLLOW|O_DIRECTORY` + `dev`/`ino` re-verify) — benefits cp/mv/rm; #F2/#F3/#F4 panic→error conversions; #F5 symlink-cycle set.

---

## Cross-utility summary

**Status: all findings remediated 2026-06-17** (8 phases on this branch). `#C4/#C5/#C6`
are documented WON'T-FIX (intentional/impl-defined); everything else is fixed with
regression tests.

| # | Util | Sev | One-liner | Status |
|---|---|---|---|---|
| #F1 | ftw | **Critical** | descent `openat` without `O_NOFOLLOW`/dev-ino re-check → leaf symlink-swap redirects `rm -r` to delete arbitrary dirs. | ✓ Ph1 |
| #C1 | cp | **Critical** | `cp -p` keeps `S_ISUID`/`S_ISGID` when `chown` fails (privilege leak). | ✓ Ph2 |
| #M1 | mv | **Critical** | cross-fs `mv` keeps setuid/setgid when ownership can't be duplicated (same root cause). | ✓ Ph2 |
| #C2 | cp | Major | `cp a b c` (c not a dir) copies only `a`, exit 0. | ✓ Ph3 |
| #M2 | mv | Major | `mv a b c` (c not a dir) moves only `a` (removing it), exit 0. | ✓ Ph3 |
| #C3 | cp | Major | first per-file error aborts the whole hierarchy copy (vs continue same-level). | ✓ Ph4 |
| #R1 | rm | Major | `-d` (remove empty dir) unimplemented. | ✓ Ph5 |
| #R2 | rm | Major | `-v` (verbose) unimplemented. | ✓ Ph5 |
| #F2–#F5, #M3–#M6, #R3–#R6, #C8 | — | Minor | panic surfaces, symlink-cycle, path-based seams, `yesexpr`, prompt `unwrap`, errno hygiene. | ✓ Ph1/6/7 |
| #C4, #C5, #C6 | cp | Minor | `src/.` extension, special-file `0o644`, `-H/-L` require `-R`. | WON'T-FIX |

**Headline.** The recursive *interior* of all three utilities was already built correctly
on ftw's dir-fd model; this remediation closed the three Critical issues that cut across
the family — the ftw descent race (#F1, now `O_NOFOLLOW`/`O_DIRECTORY` + `(dev,ino)`
re-verification), and the shared `copy_characteristics` privilege-leak (#C1/#M1, now masks
setuid/setgid when `chown` fails) — plus the operand-validation bugs (#C2/#M2), cp
continue-on-error (#C3), the two missing rm options (#R1 `-d`, #R2 `-v`), and the panic /
i18n / errno minors. **Residuals (documented):** the rare fd-conserving `DeferredDir` path
has no `(dev,ino)` baseline (fails closed via `O_NOFOLLOW`), and #F3's deep-tree reopen
still `panic!`s on a mid-walk race rather than routing to `err_reporter`.

---
---

# Part II — Remaining `tree/` utilities (audit only)

**Date:** 2026-06-17. **Status: audit-only** — findings recorded; **no source changed** and
no README "Stage" promotion (cp/mv/rm remain the only Stage-6 utilities). Covers the 13
remaining `tree/` utilities: `chmod`, `chown`, `chgrp`, `mkdir`, `mkfifo`, `rmdir`, `link`,
`unlink`, `ln`, `readlink`, `du`, `touch`, `ls`.

**Method.** Each spec slice and implementation read in full; `grep` proofs for absence
claims; **every Critical/Major finding independently verified** by reading cited lines and,
where feasible, reproduced behaviorally against the release binary and GNU coreutils.
Verification **refuted** two agent-proposed findings (rmdir `-p` non-empty-parent handling
and du `-x` foreign-device counting both actually conform / match GNU) and **demoted** two
others (readlink silent-on-non-symlink and du `-H/-L` last-wins) — see the notes inline.

**ID scheme (Part II).** `#CM*` chmod, `#CO*` chown, `#CG*` chgrp, `#MK*` mkdir, `#MF*`
mkfifo, `#RD*` rmdir, `#LK*` link, `#UN*` unlink, `#LN*` ln, `#RL*` readlink, `#DU*` du,
`#TO*` touch, `#LS*` ls. Same status/severity vocabulary as Part I.

## Cross-cutting (Part II)

- **Shared abort-on-first-error latch.** `chmod`, `chown`, `chgrp` (via
  `tree/common/change_ownership.rs`) and `du` all use a `terminate: RefCell<bool>` that is
  set on the first per-file failure and short-circuits every later entry — so one unreadable
  file silently drops the rest of a `-R`/recursive walk. This is the same anti-pattern Part I
  fixed in the cp/mv engine (#C3). Verified: `du -a` over a tree with an unreadable subdir
  drops the readable siblings. Findings #CM1/#CO1/#CG1/#DU1; one shared fix.
- **Non-NUL-terminated `&str` → libc (memory unsafety).** `mkfifo` (#MF1) and `touch` (#TO1)
  pass `filename.as_ptr()` of a Rust `&str` straight to `libc::mkfifo`/`libc::open`/
  `libc::utimes`. A `&str` is length-delimited, **not** NUL-terminated; the C call reads past
  the slice to the next stray NUL — UB / wrong-path. `grep -c CString` → 0 in both. Every
  other tree utility uses `CString::new`.
- **i18n.** All 13 call `setlocale(LC_ALL,"")` + `textdomain`, but most per-file diagnostics
  are hardcoded English (the `io::Error` body and several literals bypass `gettext`).
  Pervasive Minor; noted per-utility, not repeated.

---

## `chmod`
**Implementation:** `tree/chmod.rs` (149) + shared `plib/src/modestr.rs` (symbolic-mode engine)
**Tests:** `tree/tests/chmod/mod.rs` (183)
**Spec:** POSIX.1-2024, Vol. 3 §3 — slice `chmod.md`
**Date:** 2026-06-17

### TL;DR
The strongest of the ownership trio. `-R` runs over the Part-I-hardened `ftw` walker
(dir-fd-relative `fchmodat`, `O_NOFOLLOW|O_DIRECTORY` descent, `(dev,ino)` re-verify — TOCTOU-safe).
The symbolic grammar in `modestr.rs` is unusually complete: `who=ugoa`, `op=+-=`,
`perm=rwxXst` + permcopy `ugo`, the `X` conditional-execute bit, setuid/setgid/sticky,
comma-separated clauses, and the full umask interaction for the omitted-`who` case. Octal 3/4-digit
modes work. One real conformance gap: a per-file error **aborts the rest of the `-R` subtree**
(#CM1). Minors: missing `file` operand exits 0; hardcoded-English diagnostics; redundant
double-set of setuid in the symbolic `Add` path.

### Priority issues
#### Major
- [ ] **#CM1 — `-R` aborts the whole subtree on the first per-file error.** `tree/chmod.rs:99-105` (`*terminate.borrow_mut()=true; return Err(())`) + the `:41-43` early-out. EXIT STATUS / CONSEQUENCES OF ERRORS ("Default") expect report-and-continue (matching historical/GNU `chmod`). One `fchmodat` failure stops the walk; later siblings are skipped. Proof: `grep -n terminate tree/chmod.rs` → 35,41,79,103,114. Cross-operand iteration (`:141`) still continues, so only the current tree is abandoned. Fix: replace the `terminate` latch with a `had_error` flag (continue, set non-zero exit) — shared with #CO1/#CG1/#DU1.

#### Minor
- [ ] **#CM2 — Missing `file` operand exits 0 silently.** `tree/chmod.rs:31` (`files: Vec<String>` not `required`). SYNOPSIS `chmod [-R] mode file...` makes `file` mandatory. Fix: `#[arg(required = true)]`.
- [ ] **#CM3 — Hardcoded-English diagnostics.** `tree/chmod.rs:136,144` — `LC_MESSAGES` inert for the error body.
- [ ] **#CM4 — Redundant double-set of setuid/setgid in symbolic `Add`.** `plib/src/modestr.rs:266-274` then `:347-357` set the same bits twice (idempotent; dead code).

### Detailed conformance matrix
#### OPTIONS
| Opt | Status | Notes |
|---|---|---|
| `-R` | CONFORMS (w/ #CM1) | recursion `tree/chmod.rs:38`; dir changed before contents (APPLICATION USAGE allows either order). |
| `--`/`-`-prefixed mode | CONFORMS | `allow_hyphen_values` on `mode` (`:27`) so `chmod -w f` parses. |

#### Symbolic-mode grammar (EXTENDED DESCRIPTION)
- [x] who `ugoa` / op `+-=` / perm `rwxXst` / permcopy `ugo` — CONFORMS (`modestr.rs:96-159,130-148,307-319`).
- [x] `X` conditional-execute (dir, or any exec bit set, evaluated against post-prior-clause state) — CONFORMS `:382`; tested `test_mutate_mode_exec_dir`.
- [x] omitted-`who` `+`/`-`/`=` honor umask; explicit-`who` `+`/`-` do not — CONFORMS `:258-264,289-297,334-340`.
- [x] `=` with no who clears all then sets perm−umask — CONFORMS `:343`.
- [x] `o+s` with no other who does not touch setid bits (not an error) — CONFORMS (traced).
- [x] sticky `t`=S_ISVTX umask-independent — CONFORMS `:370-380`.
- [x] octal absolute incl. setid table; directory setid-preservation for <5-digit octal is a documented coreutils-compat extension (spec leaves non-regular setid impl-defined) — CONFORMS (`tree/chmod.rs:48-64`).
- [x] dangling-symlink → diagnostic; Linux symlinks skipped — CONFORMS (`:68-87`).

#### Other sections
- [x] STDIN/STDOUT not used, STDERR diagnostics-only, ASYNCHRONOUS "Default" — CONFORMS.
- [x] EXIT 0/`>0` (`exit_code=1` `:143`) — CONFORMS; [ ] CONSEQUENCES "Default" DIVERGES via #CM1.

### Test coverage signal
- [ ] continue-after-error in `-R` untested (#CM1); missing-`file` exit untested (#CM2).
- [x] symbolic grammar broadly covered (modestr unit tests + `test_chmod_no_x`/`_octal`/`_symlinks`/`_thru_dangling`).

### Suggested PR groupings
- **PR A — "ownership/mode continue-on-error in -R"**: #CM1 + #CO1 + #CG1 + #DU1 (shared latch→flag).
- **PR B — "mandatory operands + i18n + dead code"**: #CM2, #CM3, #CM4.

---

## `chown`
**Implementation:** `tree/chown.rs` (178) + `tree/common/change_ownership.rs` (115, shared)
**Tests:** `tree/tests/chown/mod.rs` (661)
**Spec:** slice `chown.md`
**Date:** 2026-06-17

### TL;DR
Parses `owner[:group]` (numeric-or-name for both halves), supports `-h`, `-R`, and `-H/-L/-P`
(last-wins) over the shared race-safe core. Two Major issues: the shared **`-R` abort-on-first-error**
(#CO1), and **chown omits the `-R`-without-`-H/-L/-P` no-dereference default that chgrp has** (#CO2),
making `chown -R` and `chgrp -R` treat a leaf symlink's target differently for identical invocations.
The setuid/setgid-clear-on-chown requirement is satisfied **by the Linux kernel** inside `fchownat`,
not by the utility (#CO3 — portability note). Minors: `owner:` (trailing colon) rejected; missing
`file` exits 0; hardcoded English.

### Priority issues
#### Major
- [ ] **#CO1 — `-R` aborts the whole subtree on the first per-file error.** `tree/common/change_ownership.rs:93-97` + `:64-66`. Same mechanism/fix as #CM1. Proof: `grep -n terminate tree/common/change_ownership.rs` → 59,64,95,104,113.
- [ ] **#CO2 — Missing the `-R`-default no-dereference block chgrp has.** No equivalent of `tree/chgrp.rs:84-86` (`if recurse && !(follow_cli||follow_symlinks){ no_dereference=true }`). Proof: `grep -n 'no_dereference = true' tree/chown.rs tree/chgrp.rs` → chgrp only. The spec calls the bare-`-R` default "unspecified," so conformant in isolation, but the chown/chgrp asymmetry is a latent surprise / divergence from the obviously-shared design. Fix: add the same default-to-`-P` block to `chown.rs`.

#### Minor
- [ ] **#CO3 — setuid/setgid clearing is kernel-implicit, not done by the utility.** `grep -rn 'S_ISUID|S_ISGID|fchmod' tree/common/change_ownership.rs tree/chown.rs` → 0. Spec "shall be cleared upon successful completion" is met de facto on Linux; nothing explicit, and the "other file types" case is unhandled. Portability flag.
- [ ] **#CO4 — `owner:` (empty group after colon) rejected as `invalid spec`.** `tree/chown.rs:126-131`; GNU treats `owner:` as "set group to owner's login group." Tested as the intended behavior — deliberate divergence, note only.
- [ ] **#CO5 — Missing `file` operand exits 0** (`:28`). **#CO6 — hardcoded `chown:` English diagnostics.**

### Detailed conformance matrix
#### OPTIONS
| Opt | Status | Notes |
|---|---|---|
| `-h` | CONFORMS | `AT_SYMLINK_NOFOLLOW` (`change_ownership.rs:85-89`); tested. |
| `-H`/`-L`/`-P` | CONFORMS | `:107-108,85`; last-wins via `overrides_with_all`. |
| `-R` | CONFORMS (w/ #CO1, #CO2) | `:38-39`. |

#### Operand parsing & recursion
- [x] `owner[:group]` numeric-or-name (numeric used as-is when present) — CONFORMS (`tree/chown.rs:73-99`).
- [x] `:group` (empty owner) ≡ chgrp; period-separator not supported (colon-only) — CONFORMS.
- [x] recursion race-safety (ftw `O_NOFOLLOW|O_DIRECTORY` + dir-fd `fchownat`) — CONFORMS.
- [x] STDIN/STDOUT not used, STDERR diagnostics-only, ASYNCHRONOUS "Default" — CONFORMS. [ ] CONSEQUENCES DIVERGES via #CO1.

### Test coverage signal
- [ ] `-R` continue-after-error (#CO1); chown/chgrp `-R` symlink-default divergence (#CO2); setid-clear (#CO3, needs privilege).
- [x] strong: `-h`,`-R`,`-P`,name/numeric,invalid user/group,`:group`,non-member group.

### Suggested PR groupings
- **PR A** (shared): #CO1. **PR C — "chown/chgrp -R symlink-default parity"**: #CO2. **PR B — "operands+i18n"**: #CO5,#CO6 (consider #CO4/#CO3).

---

## `chgrp`
**Implementation:** `tree/chgrp.rs` (120) + `tree/common/change_ownership.rs` (shared)
**Tests:** `tree/tests/chgrp/mod.rs` (520)
**Spec:** slice `chgrp.md`
**Date:** 2026-06-17

### TL;DR
Shares the core with chown and is the cleaner of the two on symlinks (correctly defaults bare `-R`
to no-dereference — the parity chown lacks). Group resolution (name via `getgrnam`, else numeric)
is correct; the owner is preserved. The one Major is the shared **`-R` abort-on-first-error** (#CG1).
setid-clear is again kernel-implicit (#CG2). Minors: missing `file` exits 0; empty group string
silently no-ops; hardcoded English.

### Priority issues
#### Major
- [ ] **#CG1 — `-R` aborts the whole subtree on the first per-file error.** Shared core `change_ownership.rs:93-97`. Same as #CM1/#CO1.

#### Minor
- [ ] **#CG2 — setid-clear is kernel-implicit, not by the utility.** (= #CO3.)
- [ ] **#CG3 — Empty group operand silently no-ops.** `tree/chgrp.rs:32-36`→`change_ownership.rs:75` maps `None`→`gid_t::MAX` (don't-change sentinel); `chgrp "" f` exits 0 changing nothing.
- [ ] **#CG4 — Missing `file` operand exits 0** (`:28`). **#CG5 — hardcoded `chgrp:` English.**

### Detailed conformance matrix
#### OPTIONS
| Opt | Status | Notes |
|---|---|---|
| `-h`/`-H`/`-L`/`-P` | CONFORMS | last-wins; `-h` vs `--help` clash resolved via `disable_help_flag` + long-only `--help`. |
| `-R` | CONFORMS (w/ #CG1) | default no-deref correctly set (`tree/chgrp.rs:84-86`). |

#### Other
- [x] numeric-or-name group; owner preserved (`uid.unwrap_or(md.uid())`); recursion race-safe; STDIN/STDOUT not used; STDERR diagnostics-only; ASYNCHRONOUS "Default"; EXIT 0/`>0` — CONFORMS. [ ] CONSEQUENCES DIVERGES via #CG1.

### Test coverage signal
- [ ] `-R` continue-after-error (#CG1); empty-group no-op (#CG3); missing-`file` (#CG4).
- [x] strong: `-h`,`-R`,`-P`,`-H`/`-L`,name/numeric,non-member, invalid group.

### Suggested PR groupings
- **PR A** (shared): #CG1. **PR B — "operands+i18n"**: #CG4,#CG5 (consider #CG3).

---

## `mkdir`
**Implementation:** `tree/mkdir.rs` (90)
**Tests:** `tree/tests/mkdir/mod.rs` (100)
**Spec:** slice `mkdir.md`
**Date:** 2026-06-17

### TL;DR
Core path conforms, but two **Major** `-m`/`-p` mode bugs (both behaviorally reproduced vs GNU):
`-m` is **masked by umask** (so `mkdir -m 777` under umask 022 yields `0755`, *more* restrictive
than requested — verified: posixutils `755` vs GNU `777`), and `-p` applies the **`-m` mode to
intermediate directories** instead of the spec's default-mode-modified-by-`u+wx` (verified:
`mkdir -p -m 700 a/b/c` → `a` is `700` vs GNU `755`; with `-m 000` the intermediate becomes
inaccessible and the descendant creation fails). Minors: `-p` existence test follows symlinks;
CString panic on NUL; hardcoded English. (mkfifo, by contrast, handles `-m`/umask correctly.)

### Priority issues
#### Major
- [ ] **#MK1 — `-m` mode is masked by umask; result can be more restrictive than requested.** `tree/mkdir.rs:32-42,57` calls `libc::mkdir(c_path, c_mode)` with **no `umask(0)` guard** (`grep -c umask tree/mkdir.rs` → 0). POSIX 107050-107051: "the directory shall at no time have permissions less restrictive than the −m mode." Verified: `mkdir -m 777 d` (umask 022) → `0755` (posixutils) vs `0777` (GNU). Fix: `umask(0)` around the `mkdir` for explicit `-m` (or post-`chmod`), as mkfifo (`tree/mkfifo.rs:35`) already does.
- [ ] **#MK2 — `-p` applies `-m` to intermediate dirs instead of default+`u+wx`.** `tree/mkdir.rs:50-58` applies the single `mode_val` to every component. POSIX 107062-107075 / APPLICATION USAGE: intermediate components get the default mode "modified by `u+wx` … regardless of the file mode creation mask"; only the final component gets `-m`. Verified: `mkdir -p -m 700 a/b/c` → `a`=`700` (posixutils) vs `755` (GNU); `-m 000` breaks descendant creation. Fix: create intermediates `0o777 & ~umask | 0o300`, apply `mode_val` only to the leaf.

#### Minor
- [ ] **#MK3 — `-p` existence test (`path.is_dir()`) follows symlinks and mis-handles a non-dir collision** (`:53-54`). [ ] **#MK4 — `CString::new(...).expect()` panics (exit 101) on a NUL-in-name operand; no `<newline>` guard** (`:33`). [ ] **#MK5 — hardcoded-English diagnostic body** (`:85`).

### Detailed conformance matrix
#### OPTIONS
| Opt | Status | Notes |
|---|---|---|
| `-m` octal/symbolic (base `a=rwx`) | PARTIAL | parse CONFORMS (`:45-47`); but #MK1 umask. |
| `-p` | PARTIAL | parents created, existing skipped; but #MK2 (mode) + #MK3 (symlink). |

#### Other
- [x] operands in order; STDIN/STDOUT not used; STDERR diagnostics; ASYNCHRONOUS "Default"; EXIT 0/`>0`, partial-completion continues; `-p`-already-exists → exit 0 — CONFORMS. [ ] NUL-operand panic DIVERGES (#MK4).

### Test coverage signal
- [ ] `-m` under non-zero umask (#MK1 — existing `test_set_directory_mode` uses `-m 755` which survives umask 022); `-p` intermediate mode (#MK2); symbolic `-m`; multi-operand partial-failure exit.

### Suggested PR groupings
- **PR A — "mkdir -m/-p mode correctness"**: #MK1, #MK2 (+ umask tests). **PR B — "robustness"**: #MK3, #MK4. **PR C — "i18n"**: #MK5.

---

## `mkfifo`
**Implementation:** `tree/mkfifo.rs` (85)
**Tests:** `tree/tests/mkfifo/mod.rs` (171)
**Spec:** slice `mkfifo.md`
**Date:** 2026-06-17

### TL;DR
Gets the mode semantics right (default `a=rw`→`0o666`, symbolic base `0o666`, **umask bypass for
explicit `-m`** — the thing mkdir is missing). But it has a **Critical memory-safety bug**: it
passes `filename.as_ptr()` of a Rust `&str` (not NUL-terminated) straight to `libc::mkfifo`.

### Priority issues
#### Critical
- [ ] **#MF1 — non-NUL-terminated `&str` passed to `libc::mkfifo` (OOB read / wrong path).** `tree/mkfifo.rs:41-46`: `mkfifo(filename.as_ptr() as *const c_char, …)` where `do_mkfifo(filename: &str, …)` (from `files: Vec<String>`). A Rust `&str` is length-delimited, **not** NUL-terminated; the C call reads past the slice end until a stray NUL — UB; can create a FIFO with a garbage-suffixed name. `grep -c CString tree/mkfifo.rs` → 0 (every other syscall in the tree uses `CString::new`). Tests pass only because short literal argv strings happen to be NUL-followed. Fix: `let c = CString::new(filename)?; libc::mkfifo(c.as_ptr(), …)`.

#### Minor
- [ ] **#MF2 — no `<newline>`-in-name guard** (FUTURE DIRECTIONS). [ ] **#MF3 — hardcoded-English diagnostic body** (`:79`).

### Detailed conformance matrix
#### OPTIONS
| Opt | Status | Notes |
|---|---|---|
| `-m` octal/symbolic (base `a=rw`) | CONFORMS | `:28-30`; umask bypass `:35-39,49-51`. |
| default mode applies umask | CONFORMS | `:35-39` only bypasses for explicit `-m`. |

#### Other
- [x] operands in order; STDIN/STDOUT not used; STDERR diagnostics; ASYNCHRONOUS "Default"; existing FIFO → error+exit 1; partial-completion continues — CONFORMS.

### Test coverage signal
- [ ] a heap-allocated / long path to surface #MF1 deterministically; `-m` octal under non-zero umask; multi-operand partial-failure exit.

### Suggested PR groupings
- **PR A — "mkfifo NUL-termination"**: #MF1 (Critical) + a dynamically-built-path test. **PR B**: #MF2, #MF3.

---

## `rmdir`
**Implementation:** `tree/rmdir.rs` (64)
**Tests:** `tree/tests/rmdir/mod.rs` (94)
**Spec:** slice `rmdir.md`
**Date:** 2026-06-17

### TL;DR
Removes only empty directories (`fs::remove_dir`→`rmdir(2)`). `-p` removes the leaf then walks up
parents, stopping with an error at the first non-empty parent — **which matches GNU and conforms**
(an agent-proposed "should exit 0" finding was **refuted behaviorally**: GNU `rmdir -p` also exits 1
on a non-empty parent). The real defects are Minor: the error message names the **operand** path,
not the parent that actually failed; and the `-p` walk uses lexical `Path::parent()` (mishandles a
trailing-slash operand and walks toward `/` on absolute paths).

### Priority issues
#### Minor
- [ ] **#RD1 — `-p` error message names the operand, not the failing parent.** `tree/rmdir.rs:27-44` + `main`. After `rmdir -p a/b/c` removes `c` and `b` then fails on a non-empty `a`, the error reads `a/b/c: Directory not empty` (the original operand) where GNU reads `failed to remove directory 'a'`. Verified vs GNU. The exit status (1) and the partial removal are correct — only the misattributed path is wrong. ~~(Originally proposed as a Major "should exit 0"; refuted — behavior matches GNU.)~~ Fix: report the path of the component that actually failed.
- [ ] **#RD2 — `-p` lexical `Path::parent()` mishandles trailing-slash / absolute operands.** `tree/rmdir.rs:31-34`. `a/b/c/` → after removing the leaf, `parent()` yields an already-removed path → spurious `ENOENT`; `rmdir -p /a/b` walks up toward `remove_dir("/")`. Fix: strip trailing slashes; stop at `/`, `.`, `..`.
- [ ] **#RD3 — hardcoded-English diagnostic body** (`:36,59`).

### Detailed conformance matrix
#### OPTIONS
| Opt | Status | Notes |
|---|---|---|
| `-p` | CONFORMS (w/ #RD1/#RD2) | removes parents, stops+errors at first non-empty (matches GNU). |
| only-empty removal | CONFORMS | `rmdir(2)`; non-empty → `ENOTEMPTY` (tested). |

#### Other
- [x] operands in order; STDIN/STDOUT not used (System V `-p` stdout status message correctly **absent**); STDERR diagnostics; ASYNCHRONOUS "Default"; EXIT 0/`>0`; `-p a/b/c` removes all three when empty (tested, matches EXAMPLE) — CONFORMS.

### Test coverage signal
- [ ] `-p` with a non-empty parent (asserting the correct failing-path in the diagnostic, #RD1); `-p` trailing-slash / absolute operand (#RD2); multi-operand mixed success/failure exit.

### Suggested PR groupings
- **PR A — "rmdir -p diagnostics & path-walk"**: #RD1, #RD2. **PR B — "i18n"**: #RD3.

---

## `link`
**Implementation:** `tree/link.rs` (45)
**Tests:** `tree/tests/link/mod.rs` (138)
**Spec:** slice `link.md`
**Date:** 2026-06-17

### TL;DR
A near-textbook thin wrapper over `link(2)`: exactly two operands, no options, hard-link only,
diagnostics to stderr, exit 0/1. Conforms on every golden path. Only nits are i18n and the
optional `<newline>` FUTURE-DIRECTIONS guard.

### Priority issues
#### Minor
- [ ] **#LK1 — Diagnostic text hardcoded English** (`tree/link.rs:41`). `setlocale`/`textdomain` initialized but the `link: {} -> {}: {}` message bypasses `gettext`.
- [ ] **#LK2 — no `<newline>`-in-`file2` guard** (FUTURE DIRECTIONS; optional).

### Detailed conformance matrix
- [x] OPTIONS "None" — only two positionals (`:18-22`). OPERANDS `file1`/`file2` exactly two required; semantics = `fs::hard_link` ≡ `link(file1,file2)` (no symlink-follow, no truncation). STDIN not used. ASYNCHRONOUS/STDOUT/STDERR/EXIT all CONFORMS (`:26,31-44`).

### Test coverage signal
- [x] success + `EEXIST` covered. [ ] missing/extra-operand usage errors; `ENOENT`; directory source — low risk.

### Suggested PR groupings
- **PR "tree-wrapper i18n"**: #LK1 (+ #UN1/#LN4/#RL2). Optional: #LK2.

---

## `unlink`
**Implementation:** `tree/unlink.rs` (43)
**Tests:** `tree/tests/unlink/mod.rs` (87)
**Spec:** slice `unlink.md`
**Date:** 2026-06-17

### TL;DR
Faithful thin wrapper: exactly one operand, no options, exit 0/1. `fs::remove_file` is `unlink(2)`
for files and refuses directories (as `unlink(2)` would `EISDIR`/`EPERM` anyway), so observationally
correct. Nits are i18n / strict-syscall-equivalence only.

### Priority issues
#### Minor
- [ ] **#UN1 — Diagnostic text hardcoded English** (`tree/unlink.rs:39`).
- [ ] **#UN2 — `fs::remove_file` is not the verbatim `unlink(2)` the spec names** (`:23`); observationally conforms (directory → error). Strict-only.

### Detailed conformance matrix
- [x] OPTIONS "None"; OPERAND `file` exactly one required; STDIN not used; ASYNCHRONOUS/STDOUT/STDERR/EXIT CONFORMS (`:17-42`).

### Test coverage signal
- [x] existing/nonexistent/directory covered. [ ] missing/extra-operand usage; symlink operand (removes the link, not target) — worth a test.

### Suggested PR groupings
- **PR "tree-wrapper i18n"**: #UN1. Optional/strict: #UN2.

---

## `ln`
**Implementation:** `tree/ln.rs` (80)
**Tests:** **(no dedicated test module — `mod ln;` absent from `tree/tests/tree-tests.rs`)**
**Spec:** slice `ln.md`
**Date:** 2026-06-17

### TL;DR
The weak link of this group. The two golden paths (hard-link `src target`, symlink with `-s`) work,
but **four spec-mandated behaviors are broken or absent** and there is **no test module at all**:
`-f` is declared but never consulted (dead flag); the first-vs-second synopsis form is chosen by
operand **count**, not by "does the final operand name an existing directory"; `-L`/`-P` are
entirely missing; and per-file diagnostics lack the `ln:` prefix and are untranslated.

### Priority issues
#### Major
- [ ] **#LN1 — `-f` (force) is inert; declared but never used.** `tree/ln.rs:19-20` (field `force`), no use site (`grep -n force tree/ln.rs` → only line 20, the declaration — verified). POSIX `-f`: "Force existing destination pathnames to be removed to allow the link." `ln -f existing src` → `EEXIST`, identical to no `-f`; the unlink-then-link steps are absent. Fix: when the destination exists and `-f` is set, `unlink` it (skipping the "same directory entry"/`ln a a` cases) before linking.
- [ ] **#LN2 — Synopsis-form selection ignores the "existing directory" rule.** `tree/ln.rs:64-77` branches purely on `sources.len()==1` vs `>1`; never stats the final operand (`grep -nE 'is_dir|metadata|exists' tree/ln.rs` → 0 — verified). POSIX: first form "when the final operand does not name an existing directory," second "when the final operand names an existing directory." Symptoms: (a) `ln src existing_dir` tries to hard-link onto the directory path (→ `EEXIST`/`EPERM`) instead of creating `existing_dir/src`; (b) `ln a b c` with `c` a non-directory should error but silently links `a→c` then fails `b→c`. Fix: stat the final operand; existing dir → target-dir form for all sources, else require exactly two operands.
- [ ] **#LN3 — `-L` and `-P` options missing.** `tree/ln.rs:18-27` declares only `force`/`symlink` (the two `[LP]` grep hits are help-text, not options — verified). POSIX mandates `-L`/`-P` (link to a symlink's referent vs the symlink itself, last-wins, ignored under `-s`). `ln -L`/`-P` are rejected as unknown options. The *default* (neither given) is implementation-defined, so that path is fine. Fix: add `-L`/`-P`.

#### Minor
- [ ] **#LN4 — Per-file diagnostics lack the `ln:` prefix and are untranslated** (`tree/ln.rs:68,74` emit `"{} -> {}: {}"`).
- [ ] **#LN5 — `.expect()`/`.unwrap()` panic on odd source names** (`:40,42` — `file_name().expect(...)` panics on `..`/trailing-`/`; `to_str().expect(...)` panics on non-UTF-8). [ ] **#LN6 — no `<newline>` guard** (optional).

### Detailed conformance matrix
#### OPTIONS
| Opt | Status | Notes |
|---|---|---|
| `-f` | DIVERGES | parsed, never used (#LN1). |
| `-s` | CONFORMS | `fs::soft_link` (`:31-32`); no existence/type check on source, per spec. |
| `-L`/`-P` | MISSING | #LN3. |
| `-i` | N/A | POSIX.1-2024 RATIONALE explicitly omits `-i`; correctly absent. |

#### EXTENDED DESCRIPTION (per-source algorithm)
- [ ] Step 1 destination-exists: no `-f` unlink, no "same directory entry"/`ln a a` guard (#LN1) — relies on `link(2)` `EEXIST`.
- [x] Step 2 `-s`→symlink CONFORMS; [ ] Step 3 symlink-source `-L`/`-P` MISSING (#LN3); [x] Step 4 default hard link CONFORMS (`fs::hard_link`).
- [x] EXIT 0/`>0`, multi-source loop continues then exits non-zero (`:62-79`) — CONFORMS. [ ] STDERR missing `ln:` prefix (#LN4).

### Test coverage signal
**No test module exists** — nothing exercised. Highest priority: add the module + a `-f` test and a target-dir test (both fail today), plus `ln a a`, final-operand-not-a-directory, `-L`/`-P`.

### Suggested PR groupings
- **PR A — "ln force + directory-target semantics + tests"**: #LN1, #LN2, #LN5. **PR B — "ln -L/-P"**: #LN3. **PR C — "ln diagnostics"**: #LN4.

---

## `readlink`
**Implementation:** `tree/readlink.rs` (238)
**Tests:** `tree/tests/readlink/mod.rs` (99)
**Spec:** slice `readlink.md`
**Date:** 2026-06-17

### TL;DR
The POSIX-mandated surface (operand is a symlink → write its contents to stdout; `<newline>` unless
`-n`; non-zero exit when the operand is not a symlink) is correctly implemented. It adds GNU-style
`-f`/`-v`. The one notable strict gap — the spec's "shall write a diagnostic message" when the
operand is not a symlink is **suppressed by default** (only printed under `-v`) — was **demoted to
Minor** because GNU `readlink` is also silent on a non-symlink (verified: both exit 1, both silent),
and the load-bearing exit-status half conforms; the code even comments that it matches near-universal
practice.

### Priority issues
#### Minor
- [ ] **#RL1 — "Not a symbolic link" / error diagnostics suppressed unless `-v`.** `tree/readlink.rs:149-153` returns `Err(String::new())` (silent) when `!verbose`; same for `ENOENT`/`EACCES` (`:79-94`). POSIX 113034-113035 says it "shall write a diagnostic message to standard error and exit with non-zero status." Exit status conforms; the message is suppressed. ~~(Proposed Major; demoted — GNU `readlink` is also silent, verified.)~~ Fix (strict): emit the diagnostic unconditionally.
- [ ] **#RL2 — Hardcoded-English diagnostics** (`:80,89,95,146-153`).
- [ ] **#RL3 — Non-POSIX `-f`/`-v` extensions on the public surface** (`:27-31`); POSIX defines only `-n`. Documented extensions; no action.

### Detailed conformance matrix
- [x] `-n` suppresses trailing newline (`:67-71`, tested); operand one required; STDIN not used; no symlink-follow (`symlink_metadata`+`read_link`); STDOUT writes link contents (+optional newline); EXIT 0/`>0` — CONFORMS. [ ] STDERR mandated diagnostic suppressed by default (#RL1). Symlink-loop bound at 40 (`-f` extension path) — reasonable.

### Test coverage signal
- [ ] default (no `-v`) not-a-symlink asserting (non-)empty stderr (#RL1); `-f` canonicalize paths; relative vs absolute link contents.
- [x] valid symlink, `-n`, nonexistent-under-`-v`, not-a-symlink-under-`-v` covered.

### Suggested PR groupings
- **PR — "readlink i18n"**: #RL2. Optional: #RL1 (strict), document #RL3.

---

## `du`
**Implementation:** `tree/du.rs` (194)
**Tests:** `tree/tests/du/mod.rs` (392)
**Spec:** slice `du.md`
**Date:** 2026-06-17

### TL;DR
Mostly conformant: builds on race-safe `ftw`, prints `size<tab>path`, defaults to 512-byte units,
supports `-a -s -k -x -H -L`, dedups hard links by `(dev,ino)`, exits non-zero on errors. The one
real Major is the shared **abort-on-first-error** (#DU1, behaviorally reproduced — an unreadable
subdir drops the readable siblings). Two agent-proposed findings were **refuted on verification**:
`-x` actually anchors to the operand's device (the first ftw entry *is* the operand root) and does
**not** count foreign-device entries (the `-x` `return Ok(false)` precedes the size accounting); and
the `-H/-L` last-wins gap is real but rare (demoted to Minor). Block-size, hard-link, output-format,
and symlink-default semantics are correct.

### Priority issues
#### Major
- [ ] **#DU1 — First per-file error aborts the whole walk.** `tree/du.rs:57,69-71,158-160` — the error callback sets `terminate=true`; the entry callback then returns `Ok(false)` for every later entry, so one unreadable subdirectory silently drops all later siblings/operands from the totals. Verified: `du -a` over a tree with an unreadable subdir omits the readable sibling. Spec CONSEQUENCES "Default" + 93012-93013 intend report-and-continue. Fix: drop `terminate`; keep a `had_error` for exit status (shared with #CM1/#CO1/#CG1).

#### Minor
- [ ] **#DU2 — `-H`/`-L` "last specified wins" not honored.** `tree/du.rs:25-29,163-164` — both are plain `bool`s, so `du -L -H` behaves as `-L`. POSIX 93033-93034 mandates the trailing option govern. Rare (both rarely given). Fix: resolve by option order.
- [ ] **#DU3 — Cross-operand hard-link dedup fails (Austin Group Defect 539).** The `seen` set is created **inside** `du_impl` (`tree/du.rs:60`), which is called per operand (`:188`), so a file appearing under two operands is counted twice. POSIX 93013-93014: counted "for only one entry, even if the occurrences are under different file operands." Rare usage. Fix: hoist `seen` across operands.
- [ ] **#DU4 — hardcoded-English diagnostic body** (`:160`). [ ] **#DU5 — `-a`/`-s` not enforced mutually exclusive** (`-sa` silently behaves as `-s`; spec doesn't require rejecting). [ ] **#DU6 — no `<newline>` guard** (optional).

> **Refuted on verification:** an agent-proposed Major "`-x` anchors to the first walker entry, not the operand, and still counts foreign-device dirs" — both halves are wrong: ftw yields the operand root *first* (so `initial_dev` = operand device), and the `-x` mismatch `return Ok(false)` (`tree/du.rs:85`) executes **before** the size is pushed (`:92+`), so foreign entries are not counted. `-x` conforms.

### Detailed conformance matrix
#### OPTIONS
| Opt | Status | Notes |
|---|---|---|
| `-a` | CONFORMS | `:128-131`; operands always listed. |
| `-s` | CONFORMS | `:146-151`. |
| `-k` | CONFORMS | `:44-50` 512→1024; tested. |
| `-H`/`-L` | PARTIAL | `:163-164` correct in isolation; #DU2 order rule. |
| `-x` | CONFORMS | `:78-87` (refutation above). |

#### Other
- [x] no-operand → `.`; multiple operands in order; STDIN/INPUT FILES "None"; output `size<tab>path` (tab is a `<blank>`, conforms); 512-byte default, `-k` halves; directory size = subtree sum + dir; symlinks counted not followed (default); race-safe recursion; EXIT 0/`>0` — CONFORMS. [ ] CONSEQUENCES partial via #DU1; [ ] cross-operand dedup #DU3.

### Test coverage signal
- [ ] mid-walk error continuation (#DU1); `-H -L`/`-L -H` order (#DU2); same file under two operands (#DU3); `-x` device boundary (needs a mount).
- [x] `-a`,`-s`,`-k`(512 vs 1024), default, file-operand-listed, `-H`,`-L`, hard-link dedup, nonexistent→exit 1.

### Suggested PR groupings
- **PR A — "du continue-on-error + cross-operand dedup"**: #DU1, #DU3. **PR B — "du option semantics"**: #DU2, #DU5, #DU4.

---

## `touch`
**Implementation:** `tree/touch.rs` (277)
**Tests:** **(no dedicated test module — `mod touch;` absent from `tree/tests/tree-tests.rs`)**
**Spec:** slice `touch.md`
**Date:** 2026-06-17

### TL;DR
A **Critical** memory-safety bug (non-NUL-terminated `&str` to `libc::open`/`libc::utimes`) plus
several **Major** time-handling divergences, all behaviorally reproduced: `-d` rejects spec-mandated
date forms GNU accepts (the POSIX EXAMPLE `-d 2007-11-12T10:15:30`, the space-for-`T` form, and the
comma-fractional form are all **rejected** with a parse error); `-t` interprets its fields as **UTC**,
ignoring `TZ`/local (`-t 200701011200` under `TZ=America/New_York` lands at `07:00` local vs GNU's
`12:00`); sub-second precision is dropped; and `-c` on a missing file **prints a diagnostic and exits
1** instead of being silent with exit 0. The `-a`/`-m`/`-c`/`-r` wiring and umask-respecting creation
are otherwise correct. **No test module exists.**

### Priority issues
#### Critical
- [ ] **#TO1 — non-NUL-terminated `&str` to `libc::open`/`libc::utimes` (UB).** `tree/touch.rs:133,213` — `libc::open(filename.as_ptr() …)` / `libc::utimes(filename.as_ptr() …)` where `filename: &str` (from `files: Vec<String>`). `grep -c CString tree/touch.rs` → 0. Reads past the slice end → wrong/garbage pathname or OOB read. Fix: `CString::new(filename)?`.

#### Major
- [ ] **#TO2 — `-d date_time` rejects spec-mandated forms; the POSIX EXAMPLE fails.** `tree/touch.rs:40-43` uses `DateTime::parse_from_rfc3339`, which requires `T` + an explicit offset/`Z`. Verified rejected (GNU accepts): `-d 2007-11-12T10:15:30` (no tz → local; a POSIX EXAMPLE), `-d "2007-11-12 10:15:30"` (space for `T`), `-d 2007-11-12T10:15:30,002` (comma-fractional). Fix: hand-parse ISO-8601 extended (accept `T`/space, `.`/`,` fraction, optional `Z`; absent tz = local).
- [ ] **#TO3 — `-t`/`-d` ignore `TZ`, interpret fields as UTC.** `tree/touch.rs:114` (`Utc.with_ymd_and_hms`). POSIX 117765/117800: the `-t` time "shall be affected by the value of the TZ environment variable." Verified: `-t 200701011200` (`TZ=America/New_York`) → `07:00` local (posixutils) vs `12:00` (GNU). `grep -nE 'TZ|Local' tree/touch.rs` → 0. Fix: interpret naive fields in the local/`TZ` zone.
- [ ] **#TO4 — `-c` on a missing file emits a diagnostic + exit 1 (must be silent, exit 0).** `tree/touch.rs:231-234` returns `Err("File does not exist")` → `main` (`:270-273`) prints it and exits 1. Verified: posixutils `touch -c /nonexistent` → diagnostic + exit 1 vs GNU silent + exit 0. POSIX `-c`: "Do not create … Do not write any diagnostic message." Fix: with `-c` and a missing file, return success silently.
- [ ] **#TO5 — spec names `utimensat`/`futimens`; sub-second precision dropped.** `tree/touch.rs:151,213` use `utimes`/`futimes` (µs `timeval`) and hardcode `tv_usec: 0`, so the `.002`-second EXAMPLES lose their fraction. "Equivalent to" allows the syscall latitude, but dropping the fractional seconds diverges. Fix: `utimensat`/`futimens` with nanoseconds.
- [ ] **#TO6 — new-file creation always sets BOTH times, ignoring `-a`/`-m`.** `tree/touch.rs:130-161` (`touch_file_new`) writes `times[0]=times[1]=time` unconditionally. `touch -a -t <past> newfile` wrongly sets mtime to the past option time instead of "now." Fix: in `touch_file_new`, set only the requested field(s) to `time`, the other to current time.

#### Minor
- [ ] **#TO7 — `-r ref_file` copies only mtime, not the corresponding field** (`:124-128`); `touch -a -r` is wrong. [ ] **#TO8 — `-t` leap-second `SS=60` rejected by `with_ymd_and_hms`** (`:111-117`). [ ] **#TO9 — diagnostics hardcoded English and drop `errno`** (`:135,152,214`).

### Detailed conformance matrix
#### OPTIONS
| Opt | Status | Notes |
|---|---|---|
| `-a`/`-m` | PARTIAL | existing-file CONFORMS (`:170-198`); new-file path ignores them (#TO6). |
| `-c` | DIVERGES | should be silent/exit-0 on missing file (#TO4). |
| `-d` | DIVERGES | RFC3339-only (#TO2). |
| `-t` | PARTIAL | length 8/10/12 + century pivot 69 CONFORMS; UTC-only (#TO3), no leap/range (#TO8). |
| `-r` | PARTIAL | mtime-only (#TO7). |
| default → both times | CONFORMS | `:248-251`. |

#### Other
- [x] obsolete `MMDDhhmm[YY]` first-operand date form **correctly removed** (POSIX.1-2024 dropped it; every operand treated as a filename) — CONFORMS. operands iterated; STDIN/STDOUT not used; ASYNCHRONOUS "Default"; EXIT 0/`>0` multi-file accumulate — CONFORMS (modulo #TO4).

### Test coverage signal
**No test module exists.** Priority: NUL-termination / non-ASCII names (#TO1); the 5 spec `-d`
EXAMPLES (#TO2); `-t` local-vs-UTC under fixed `TZ` (#TO3); `-c` missing → silent/exit-0 (#TO4);
`-a -t past newfile` leaves mtime≈now (#TO6); sub-second preserved (#TO5).

### Suggested PR groupings
- **PR A — "touch path safety"**: #TO1 (Critical) + non-ASCII test. **PR B — "touch time parsing"**: #TO2, #TO3, #TO5, #TO8. **PR C — "touch option semantics"**: #TO4, #TO6, #TO7. **PR D — "touch tests + i18n"**: new test module, #TO9.

---

## `ls`
**Implementation:** `tree/ls.rs` (1272) + `tree/ls_util/entry.rs` (766) + `tree/ls_util/utf8_lossy.rs` (189)
**Tests:** `tree/tests/ls/mod.rs` (663)
**Spec:** slice `ls.md`
**Date:** 2026-06-17

### TL;DR
A broad, mostly-faithful `ls` covering every POSIX option letter, the full `-l` field set,
multi-column/stream layouts, the 6-month date rule, special-file `major,minor`, the mode-string
logic, and `-R` with infinite-loop detection over `ftw`. But it has a **Critical, golden-path
crash**: `ls -l` on **any** symbolic link — named on the command line *or* merely present inside a
listed directory — **panics** (two `.unwrap()` sites, both reproduced), so `ls -l /etc` aborts on
most systems. Beyond that: sorting is **byte-order, not `LC_COLLATE`** (no collation call anywhere);
the 6-month recent/old date decision is keyed off **mtime even under `-c`/`-u`**; the optional
alternate-access `+` flag is never emitted; and `-q` is not `LC_CTYPE`-driven. The infinite-loop
detector works.

### Priority issues
#### Critical
- [x] **#LS1 — `ls -l` on a command-line symlink panics.** `tree/ls.rs:916`: `buf.shrink_to(num_bytes)` (capacity, not length) left `buf.len()==PATH_MAX` of trailing NULs, so `CString::from_vec_with_nul(buf).unwrap()` hit `InteriorNul` and aborted. ✓ **Fixed (Phase 1):** a shared `read_link_target(dir_fd, name)` helper `truncate`s to the byte count and uses `ls_from_utf8_lossy`; a failed readlink now omits the target rather than erroring. Test: `test_ls_l_symlink_no_panic`.
- [x] **#LS2 — `ls -l` on a directory containing a symlink panics.** `tree/ls.rs:1110`: `dir_entry.read_link().unwrap()` was `None` for in-traversal symlinks. ✓ **Fixed (Phase 1):** falls back to `read_link_target(dir_entry.dir_fd(), dir_entry.file_name())` when ftw's cached readlink is `None`. Test: `test_ls_l_symlink_no_panic` (directory case).

#### Major
- [x] **#LS3 — Sort now uses `LC_COLLATE` collation.** Was raw `OsString::cmp` (byte order). ✓ **Fixed (Phase 2):** all three sort keys route through a `collate_names` helper using `plib::locale::strcoll` on the UTF-8 view, with a byte-order tiebreak / non-UTF-8 fallback. Verified locale-sensitive (case-insensitive grouping under a UTF-8 locale vs byte order under `C`).
- [x] **#LS4 — 6-month date decision now tracks the displayed timestamp.** ✓ **Fixed (Phase 2):** the recent-vs-old branch uses the displayed `time` (ctime/atime under `-c`/`-u`), not always-mtime. Test: `test_ls_u_recency_displayed_time`.
- [ ] **#LS5 — Alternate-access-method flag never emitted.** `tree/ls_util/entry.rs:577-658`. The spec's mode string ends with an optional trailing field (a single printable non-`<blank>`, conventionally `+`) when an alternate access method (e.g. an ACL) is present. The mode string is built to exactly 10 chars with no probe (`grep -n "acl|getxattr|'+'" tree/ls_util/entry.rs` → 0); ACL'd files render identically to plain ones. Fix: probe for ACLs/alternate access and append `+`.
- [ ] **#LS6 — `-q` non-printable handling diverges from the `LC_CTYPE` print class.** `tree/ls_util/entry.rs:128-134` only maps `is_control()`/`\t` to `?`; it misses non-control non-printable code points and is not `LC_CTYPE`-driven, and `-q` is not the terminal default. Fix: classify against the locale print class.

#### Minor
- [x] **#LS7 — Six-month window is now precise** (365.25/2 days, was `30*6`). ✓ Fixed (Phase 2).
- [x] **#LS8 — `LC_TIME` now honored.** ✓ **Fixed (Phase 2):** the date field is formatted via `plib::locale::strftime` (libc `strftime`, localized month names + `TZ`), replacing chrono’s English-only `%b`.
- [x] **#LS9 — Recent date now uses `%e` (blank-pad).** ✓ **Fixed (Phase 2):** format strings use `%b %e …`; `Jun  5` not `Jun 05`. Test: `test_ls_date_blank_padded_day`.
- [x] **#LS10 — socket type char `s` and `-F` socket `=`.** ✓ **Fixed (Phase 1):** added `FileType::Socket => 's'` to the mode-string type char and a `file_type.is_socket() => '='` arm to the `-F` classify. Test: `test_ls_socket_classification`.
- [ ] **#LS11 — owner/group lookup failure errors instead of printing the numeric id** (`entry.rs:667-686`): spec says a name that "cannot be determined" is "replaced with their associated numeric values."
- [ ] **#LS12 — wide (East-Asian) Unicode column width** uses `chars().count()` (every code point width 1) — CJK/zero-width misalign columns. [ ] **#LS13 — `-s`/no-`-k` uses 512-byte units** (GNU defaults to 1024) — conformant but surprising; the `total` line correctly uses 512.

### Detailed conformance matrix
#### OPTIONS (every letter)
| Opt | Status | Notes |
|---|---|---|
| `-a -A -C -d -f -g -H -i -k -L -m -n -o -p -r -R -s -S -t -x -1` | CONFORMS | see `tree/ls.rs` / `entry.rs`; `-1` re-enables long if it overrode `-C/-m/-x`; last-wins via `overrides_with_all` + a manual long-format resolver (`tree/ls.rs:307-372`). |
| `-l` | DIVERGES | **panics on symlinks** (#LS1/#LS2); mode `+` flag missing (#LS5); else format conforms. |
| `-c`/`-u` | PARTIAL | time used for sort/display, but drives the wrong 6-month branch (#LS4). |
| `-q` | PARTIAL | not `LC_CTYPE` print-class, not terminal default (#LS6). |
| `-F` | PARTIAL | `/ @ | *` emitted; socket `=` missing (#LS10). |

#### ENVIRONMENT VARIABLES
| Var | Status | Notes |
|---|---|---|
| `COLUMNS` | CONFORMS | parse → ioctl → 80 (`tree/ls.rs:486-514`). |
| `LC_COLLATE` | MISSING | byte-order sort (#LS3). |
| `LC_CTYPE` | PARTIAL | invalid UTF-8→`?`; `-q` not print-class (#LS6); no multibyte width (#LS12). |
| `LC_TIME` | MISSING | hardcoded `%b` (#LS8). |
| `LC_MESSAGES` | CONFORMS | `setlocale`+`gettext` on diagnostics. |
| `TZ` | CONFORMS | chrono `Local`. |

#### EXTENDED DESCRIPTION — long format
- [x] type char `d b c l p` + rwx triads + setuid/setgid/sticky case rules — CONFORMS (`entry.rs:583-655`), **except** socket → `-` (#LS10); [ ] trailing alternate-access flag never written (#LS5).
- [x] nlink/owner/group/size — CONFORMS, [ ] except owner/group numeric-fallback on lookup failure (#LS11).
- [ ] date-time: recent `%b %d %H:%M` / old `%b %d  %Y` implemented but `%d` not `%e` (#LS9), mtime-keyed under `-c`/`-u` (#LS4), 180-day window (#LS7); future-date → old format CONFORMS.
- [ ] symlink `-> target`: construction **panics** (#LS1/#LS2); the `@` link-type-indicator and the resolved-file type indicator are not implemented.
- [x] `total <blocks>` in 512-byte units (1024 w/ `-k`), dirs only, before entries — CONFORMS.
- [x] multi-column: `COLUMNS`/ioctl/80; constant column width; filenames never truncated; O(n²) guard at 1000 — CONFORMS (#LS12 width caveat).

#### Other
- [x] no-operand → `.`; non-dir operands first (sorted separately); missing file → stderr + exit ≥1; STDIN not used; directory header `\n%s:\n` (omitted for a single operand); diagnostics to stderr — CONFORMS. [ ] operand sort itself byte-order (#LS3). EXIT 0/`>0` (loop → 2, conformant); panics (#LS1/#LS2) bypass exit status entirely.

### Test coverage signal
- [ ] `ls -l` on a symlink (command-line *and* in-directory) — the crash is **entirely untested** (`test_ls_dangle` never uses `-l`) (#LS1/#LS2).
- [ ] `LC_COLLATE` sort (#LS3); old-file-recent-atime under `-u -l` (#LS4); ACL `+` (#LS5); `-q` non-control non-printables (#LS6); `%e` blank-pad day (#LS9); socket `s`/`=` (#LS10); owner/group numeric fallback (#LS11).
- [x] empty dir, dangle, file-type `-F`, infinite loop, inode, `-m`, recursive, `-rt`, size-align, time covered.

### Suggested PR groupings
- **PR 1 — "Stop `ls -l` crashing on symlinks"**: #LS1, #LS2 (+ `-l`-on-symlink tests; implement the `@`/resolved-type indicators while there).
- **PR 2 — "Locale-correct sorting & dates"**: #LS3, #LS4, #LS7, #LS9, #LS8.
- **PR 3 — "Mode-string fidelity"**: #LS5, #LS10, #LS11.
- **PR 4 — "Non-printable & width"**: #LS6, #LS12. **PR 5 (optional)**: #LS13.

---

## Part II summary

**Verified Criticals (3 utilities):**

| # | Util | One-liner |
|---|---|---|
| #MF1 | mkfifo | non-NUL-terminated `&str` → `libc::mkfifo` (UB / wrong path). |
| #TO1 | touch | non-NUL-terminated `&str` → `libc::open`/`libc::utimes` (UB). |
| #LS1/#LS2 | ls | `ls -l` on a symlink (CLI or in a listed dir) panics — `ls -l /etc` aborts. |

**Verified Majors:** #CM1/#CO1/#CG1/#DU1 (shared `-R`/recursion abort-on-first-error), #CO2
(chown/chgrp `-R` symlink-default asymmetry), #MK1/#MK2 (mkdir `-m` umask + `-p` intermediate
mode), #LN1/#LN2/#LN3 (ln dead `-f`, count-based synopsis form, missing `-L`/`-P`), #TO2/#TO3/#TO4/
#TO5/#TO6 (touch `-d`/`-t` parsing, `TZ`, `-c` exit, sub-second, new-file `-a`/`-m`), #LS3/#LS4/#LS5/
#LS6 (ls collation, date branch, access flag, `-q`).

**Refuted on verification:** rmdir `-p` non-empty-parent "should exit 0" (matches GNU — exits 1);
du `-x` "wrong anchor / counts foreign dirs" (operand is the first ftw entry; foreign entries aren't
counted). **Demoted to Minor:** readlink silent-on-non-symlink (GNU parity), du `-H/-L` last-wins (rare).

**Clean (no Critical/Major):** `link`, `unlink` (textbook thin wrappers; i18n-only nits); `rmdir`
(conforms; Minor diagnostic-path + lexical-parent nits); `chmod` (only the shared `-R` abort).

**Cross-cutting fixes that would close the most findings:** (1) the shared abort-latch→continue-flag
refactor (#CM1/#CO1/#CG1/#DU1); (2) `CString`-terminate every `libc` path call (#MF1/#TO1, and audit
the rest); (3) a shared `ln`/`-m`/time-parsing correctness pass. **No README "Stage" promotion** — these
13 remain pre-remediation; cp/mv/rm are still the only Stage-6 utilities.
