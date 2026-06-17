# POSIX.1-2024 Conformance & Race-Safety Audit — `tree/` cp, mv, rm

**Scope.** This is a *partial* audit of the `tree/` crate covering only the three
data-destroying, security-critical utilities **`cp`**, **`mv`**, and **`rm`**, plus the
**`ftw/`** dependency crate they rely on for race-free directory traversal. Unlike the
other crate audits, this one adds an explicit **filesystem-race / security-hardening
lens** on top of plain POSIX conformance, because these utilities are the canonical
targets of TOCTOU and symlink-swap attacks and the canonical cause of accidental data
loss. The remaining `tree/` utilities (`ls`, `chmod`, `chown`, `chgrp`, `du`, `link`,
`unlink`, …) are **not** covered here.

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
- [ ] **Affirmative responses are hardcoded `response.to_lowercase().starts_with('y')`** (`cp.rs:97`, `mv.rs:64`, `rm.rs:55`), ignoring the `LC_MESSAGES`/`LC_COLLATE`/`LC_CTYPE` `yesexpr` ERE the spec ties to "process affirmative responses" (cp 90758-90761, mv 108154-108157, rm 113438-113441). Minor. (#C8/#M5/#R6)
- [ ] **`prompt_user` does `io::stdin().read_line(...).unwrap()`** (`cp.rs:96`, `mv.rs:63`, `rm.rs:54`) — a hard read error panics the utility mid-operation. Minor.

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

#### Critical
- [x] **#C1 — `cp -p` does not clear `S_ISUID`/`S_ISGID` when ownership can't be duplicated (privilege leak).** `tree/common/copy.rs:793-803` ignored the `fchownat` result, then `:806-811` applied the **full source mode** via `fchmodat`, re-setting the setuid/setgid bits on a file now owned by the copying user. POSIX 90720-90721: *"If the user ID or the group ID cannot be duplicated, the file permission bits S_ISUID and S_ISGID shall be cleared."* Behaviorally: as non-root, `cp -p /usr/bin/passwd ./x` → `./x` ended up `-rwsr-xr-x caller:caller` (`4755`); GNU clears it to `0755`. ✓ **Fixed (Phase 2):** `copy_characteristics` now records whether `fchownat` succeeded and masks `~(S_ISUID|S_ISGID)` out of the mode before `fchmodat` when it did not. Re-verified: `cp -p /usr/bin/passwd` now yields `0755` (matches GNU); same-owner `cp -p` still preserves the bits. Tests: `test_cp_preserve_keeps_setuid_same_owner` (non-root) and root-gated `test_cp_preserve_clears_setuid_on_chown_fail` in `tree/tests/cp/mod.rs`.

#### Major
- [x] **#C2 — >2 operands with a non-directory target silently copies only the first source.** `tree/cp.rs`: when the target was not a directory, `main` took the single-file branch and dropped `sources[1..]`, exiting 0. POSIX 90605-90606: *"It shall be an error if target does not exist and more than two operands are specified, or if target exists and does not name a directory."* ✓ **Fixed (Phase 3):** `main` now diagnoses `target '…' is not a directory` and exits 1 when `!dir_exists && sources.len() > 1`, before any copy. Test: `test_cp_multi_source_nondir_target`.
- [ ] **#C3 — A per-file failure aborts the entire hierarchy copy instead of continuing with same-level siblings.** `tree/common/copy.rs:438-440` short-circuits every subsequent entry once `terminate` is set, and `:561-565` (plus the `postprocess`/error closures at `:596-606`) set `terminate = true` on the first `copy_file_impl` error or traversal error. POSIX 90829-90832: *"When a failure occurs during the copying of a file hierarchy, cp is required to attempt to copy files that are on the same level in the hierarchy or above the file where the failure occurred."* (Below the failure is unspecified.) The current code copies neither siblings nor ancestors after the first error. (Behavioral confirmation is readdir-order-dependent — a run where the unreadable file happened to sort last still copied its siblings — but the `terminate` flag unconditionally prunes all not-yet-processed entries, so the divergence is in the control flow regardless of order.) Fix: report the per-file error, leave `terminate` clear, and continue the traversal; only set the exit status.

#### Minor
- [ ] **#C4 — `source/.` trailing form is a non-POSIX extension (self-flagged).** `tree/common/copy.rs:637-660` special-cases a `…/.` source to copy a directory's *contents* (the code's own comment: *"This doesn't seem to be compliant with POSIX"*). Matches GNU `cp -R dir/.` behavior; harmless extension. Document or gate.
- [ ] **#C5 — Special-file destination mode is hardcoded `0o644`, ignoring umask.** `tree/common/copy.rs:697-706`: non-FIFO special files (`-R`) are created with `mknodat` mode `0o644`; FIFOs correctly use the source mode. POSIX leaves special-file permissions/owner/group **unspecified** (90683-90687, RATIONALE 90833-90839), so this conforms, but `0644` unmodified by umask is a deliberate choice worth noting; with `-p` the subsequent `copy_characteristics` overwrites it with the source mode anyway.
- [ ] **#C6 — `-H`/`-L` require `-R` (clap `requires = "recursive"`).** `tree/cp.rs:34,47`. POSIX lists `-H`/`-L`/`-P` only under the `-R` synopsis form, so requiring `-R` for `-H`/`-L` is defensible, but GNU accepts `-L`/`-H` without `-R`. `-P` correctly does **not** require `-R` (`cp.rs:53-63`, matching synopsis form 1/2 `[-Pfip]`). Low priority.

### Detailed conformance matrix

#### SYNOPSIS / operand dispatch
- [x] Three synopsis forms recognized via target-is-directory probe (`cp.rs:117-131`) + `copy_files`/`copy_file`.
- [ ] **>2 operands + non-dir target not an error** (#C2, Major) — `cp.rs:139-156`.
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
- [ ] (#C3) On error mid-hierarchy, cp stops rather than continuing same-level/above (90829-90832).

### Race-safety & security (cp)
- **Hardened:** all in-tree file creation/overwrite is dir-fd-relative (`openat`/`mkdirat`/`symlinkat`/`mknodat` against ftw `target_dirfd` and `source.dir_fd()`), so the recursive interior is not path-re-resolving.
- **Residual:** inherits ftw #F1 (descent open without `O_NOFOLLOW`) — under `-R` a swapped directory→symlink can redirect copies; under `-L`/`-H` symlink following compounds it. Top-level `fs::metadata(target)` (`cp.rs:118`) is a path probe.
- **Security:** #C1 (setuid/setgid leak) is the headline. The created-file just-overwritten guard (`copy.rs:243-250`) prevents a source from clobbering a file cp created this run.

### Test coverage signal
- [ ] No test asserts setuid/setgid are cleared under `cp -p` when `chown` fails (#C1).
- [ ] No test asserts the error for `cp a b non_dir_target` with >2 operands (#C2).
- [ ] No test asserts same-level siblings are still copied after a per-file error (#C3).
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

#### Critical
- [x] **#M1 — Cross-filesystem `mv` does not clear `S_ISUID`/`S_ISGID` when ownership can't be duplicated.** `tree/mv.rs:80` sets `preserve = true` and routes through the shared `copy_characteristics` (`copy.rs`), which had the #C1 defect. POSIX 108104-108105: *"If the user ID, group ID, or file mode of a regular file cannot be duplicated, the file mode bits S_ISUID and S_ISGID shall not be duplicated."* ✓ **Fixed (Phase 2):** closed by the shared `copy_characteristics` fix (#C1).

#### Major
- [x] **#M2 — >2 operands with a non-directory target silently moves only the first source.** `tree/mv.rs`: the single-move branch dropped `sources[1..]`. Behaviorally: `mv a b c` (c not a dir) → `a` renamed to `c` (**`a` gone**), `b` ignored, exit 0. More damaging than #C2 because the first source is actually removed. ✓ **Fixed (Phase 3):** `main` rejects `!dir_exists && sources.len() > 1` with `target '…' is not a directory` (exit 1) before moving; additionally the first-synopsis-form trailing-slash rule (108049-108050) now errors for a non-directory source. Tests: `test_mv_multi_source_nondir_target`, `test_mv_nondir_source_trailing_slash`.

#### Minor
- [ ] **#M3 — Cross-device source cleanup uses path-based `source.is_dir()` (follows symlinks).** `tree/mv.rs:352,420`: choosing `fs::remove_dir_all(source)` vs `fs::remove_file(source)` via `source.is_dir()` dereferences a symlink source; combined with the deferred-delete list this is a correctness/race edge for a symlink-to-directory source moved across filesystems. Fix: classify with `symlink_metadata`.
- [ ] **#M4 — `EXDEV` detection re-reads global errno.** `tree/mv.rs:232` uses `std::io::Error::last_os_error().raw_os_error()` rather than the captured `e` from `fs::rename`; fragile (relies on no intervening syscall touching errno). Fix: use `e.raw_os_error()`.

### Detailed conformance matrix

#### SYNOPSIS / operand dispatch
- [x] Two forms via target-is-existing-directory probe (`mv.rs:390-402`); destination = `target.join(file_name)` (108054-108056).
- [ ] **>2 operands + non-dir target not an error** (#M2, Major).
- [ ] **Trailing-slash on a non-directory source→target_file not enforced.** POSIX 108049-108050: a non-directory source with a `target_file` ending in `/` *"shall [be] treat[ed] as an error and no source_file operands shall be processed."* No such check exists. Minor.

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
- [x] Step 6 duplicate hierarchy, links-as-links — `mv.rs:68-92` (`dereference=false`); hard links preserved via `linkat` (`copy.rs:461-475`). The 108114-108115 rule "duplication-of-characteristics failure shall not modify exit status" is **not** honored (a `copy_characteristics` error sets `terminate` and propagates) — Minor sub-gap of #C3.
- [x] Step 7 remove source hierarchy — `mv.rs:350-365,418-425`. CONFORMS.

#### ENVIRONMENT / ASYNC / STDOUT / STDERR / EXIT
- Same as cp: catalog via `setlocale`; `yesexpr` hardcoded (#M5). STDOUT unused (108162); prompts+diagnostics to stderr (108164-108167). Exit 0/`>0` (108173-108176); `move_files` accumulates failures (`mv.rs:336,345,363`).
- [x] **#M6 — CONSEQUENCES OF ERRORS (108178-108181)** — *"shall not modify both source_file and the destination path simultaneously; termination at any point shall leave either source_file or the destination path complete."* Same-fs `rename` is atomic; cross-fs removes the old destination then copies then removes the source, so an interruption always leaves the **source** complete (it is removed last). CONFORMS.

### Race-safety & security (mv)
- **Hardened:** same-fs path is a single atomic `rename(2)`. Cross-fs duplication uses the dir-fd-relative cp engine.
- **Residual:** top-level stats use `ftw::Metadata::new(AT_FDCWD, full-path, …)` (`mv.rs:107,125,152-154`) and `fs::rename`/`fs::remove_*` are path-based — top-level TOCTOU window. Cross-fs interior inherits ftw #F1. `source.is_dir()` symlink-follow (#M3).
- **Security:** #M1 (setuid/setgid leak across filesystems).

### Test coverage signal
- [ ] No test for `mv a b non_dir_target` (>2 operands) erroring (#M2).
- [ ] No test for setuid/setgid clearing on cross-device move with failed `chown` (#M1).
- [ ] No test for a non-directory source with a trailing-slash `target_file` (108049-108050).
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

#### Major
- [ ] **#R1 — `-d` (remove empty directories) is unimplemented.** `grep -n "'d'" tree/rm.rs` → 0 matches; `Args` (`rm.rs:27-39`) declares only `-f`/`-i`/`-r`(`-R`). POSIX SYNOPSIS `rm [-diRrv] file...` (113356) and OPTIONS 113404 mandate `-d`; DESCRIPTION 113369-113370 gives it distinct semantics ("if `-d` is specified, rm shall proceed with step 3 for the current file" — i.e. `rmdir` an empty directory without recursion). RATIONALE 113532-113535 highlights that `-d` *"delete[s] either a file or an empty directory without … the inherent race between determining a file's type and deciding what action to attempt"* — directly relevant to this audit's race lens. Behaviorally: `rm -d emptydir` → `error: unexpected argument '-d'`, exit 2. Fix: add `-d`; for a directory operand with `-d` (and without `-r`/`-R`), `unlinkat(AT_REMOVEDIR)` after the standard prompt.
- [ ] **#R2 — `-v` (verbose) is unimplemented.** `grep -n "'v'" tree/rm.rs` → 0 matches. POSIX SYNOPSIS 113356, OPTIONS 113412-113413, STDOUT 113445-113447, EXIT-STATUS 113458-113461 all require `-v` to write each removed pathname to **stdout**. Behaviorally: `rm -v file` → `error: unexpected argument '-v'`, exit 2. Fix: add `-v`; on each successful removal write the pathname to stdout (and treat a stdout-write failure as affecting exit status per 113458-113461). Note the FUTURE-DIRECTIONS newline-in-pathname caveat (113538-113541) applies to `-v` output.

#### Minor
- [ ] **#R3 — No operands without `-f` exits 0 silently.** `tree/rm.rs:38` (`files: Vec<PathBuf>` is not `required`) + `main` loop (`rm.rs:438`). POSIX SYNOPSIS form 1 is `rm [-diRrv] file...` (≥1 operand required); only the `-f` form (113357) permits zero operands. Behaviorally: bare `rm` → exit 0; GNU → `rm: missing operand`, exit 1. The **`-f`** no-operand case is correctly silent/0 (113405-113407). Fix: if no operands and `-f` not set, diagnose and exit non-zero.
- [ ] **#R4 — Top-level single-file removal is path-based (not dir-fd-relative).** `tree/rm.rs:401-424`: `fs::symlink_metadata(filepath)` classifies, then `rm_file` (`:383-399`) re-stats via `ftw::Metadata::new(AT_FDCWD, full-path, …)` and removes via `fs::remove_file(filepath)` on the **full path**. There is a TOCTOU window between classify and unlink, and the removal is not anchored to a parent dir-fd as the recursive path is. Impact is bounded (`unlink` never follows the final symlink, so the worst case is `EISDIR`/`ENOTDIR`, not a wrong deletion), but it is the one rm path that doesn't use the hardened model. Fix: split the operand into dir-fd + basename and `fstatat`/`unlinkat` relative to the parent, mirroring the recursive interior.
- [ ] **#R5 — Process-aborting `unreachable!()` sites.** `tree/rm.rs:155` (`should_remove_file` Directory arm) and `:371` (`ReadLink` error arm). Both are unreachable given current call sites (directories are handled before `should_remove_file`; symlinks are never followed), but a future refactor or unexpected ftw error would panic the utility. Fix: degrade to a diagnostic + skip.

### Detailed conformance matrix

#### SYNOPSIS / OPTIONS

| Opt | Status | Notes (file:line) |
|---|---|---|
| `-d` | MISSING | (#R1, Major) not declared. |
| `-f` | CONFORMS | `rm.rs:28`; suppresses prompt; `NotFound` swallowed (`rm.rs:406`); `overrides_with_all` clears prior `-i` (113405-113407). |
| `-i` | CONFORMS | `rm.rs:31`; prompts even when stdin is not a terminal (113472-113474). |
| `-R` | CONFORMS | `rm.rs:34` (`-R` a visible alias of `-r`). |
| `-r` | CONFORMS | `rm.rs:34`; required for directory recursion (`rm.rs:227`). |
| `-v` | MISSING | (#R2, Major) not declared. |

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
- [ ] No test for `-d` or `-v` (don't exist — #R1, #R2).
- [ ] No test for bare `rm` (no operands, no `-f`) erroring (#R3).
- [ ] No adversarial test for the descent-open symlink-swap race (#F1) — would need a concurrent-rename harness.
- [x] cycle, dangling-symlink, deep tree, write-protected dir, dot-relative, empty/inaccessible, readdir-bug, `-i`/`-f`/`-r` combinations, root-link, EACCES paths are exercised (`tree/tests/rm/mod.rs`).

### Suggested PR groupings
- **PR F — "rm -d (Austin Group Defect 802)"**: #R1.
- **PR G — "rm -v (Austin Group Defects 1154/1365/1487)"**: #R2 (+ newline-in-pathname FUTURE-DIRECTIONS guard).
- **PR H — "rm operand & panic hygiene"**: #R3, #R5, and #R4 (dir-fd-relative single-file removal).
- **PR I — "ftw race hardening"**: #F1 (`O_NOFOLLOW|O_DIRECTORY` + `dev`/`ino` re-verify) — benefits cp/mv/rm; #F2/#F3/#F4 panic→error conversions; #F5 symlink-cycle set.

---

## Cross-utility summary

| # | Util | Sev | One-liner |
|---|---|---|---|
| #F1 | ftw | **Critical** | descent `openat` without `O_NOFOLLOW`/dev-ino re-check → leaf symlink-swap redirects `rm -r` to delete arbitrary dirs. |
| #C1 | cp | **Critical** | `cp -p` keeps `S_ISUID`/`S_ISGID` when `chown` fails (privilege leak). |
| #M1 | mv | **Critical** | cross-fs `mv` keeps setuid/setgid when ownership can't be duplicated (same root cause). |
| #C2 | cp | Major | `cp a b c` (c not a dir) copies only `a`, exit 0. |
| #M2 | mv | Major | `mv a b c` (c not a dir) moves only `a` (removing it), exit 0. |
| #C3 | cp | Major | first per-file error aborts the whole hierarchy copy (vs continue same-level). |
| #R1 | rm | Major | `-d` (remove empty dir) unimplemented. |
| #R2 | rm | Major | `-v` (verbose) unimplemented. |
| #F2–#F5, #C4–#C8, #M3–#M6, #R3–#R6 | — | Minor | panic surfaces, path-based seams, `yesexpr`, prompt `unwrap`, special-file mode, trailing-slash, errno hygiene. |

**Headline.** The recursive *interior* of all three utilities is built correctly on
ftw's dir-fd model, but three Critical issues cut across the family: a single ftw descent
race (#F1) that turns `rm -r` into an arbitrary-deletion primitive under an adversarial
rename, and a shared `copy_characteristics` privilege-leak (#C1/#M1) that re-applies
setuid/setgid after a failed `chown`. Two Major operand-validation bugs (#C2/#M2) silently
drop sources, and `rm` is missing two POSIX.1-2024-mandated options (#R1 `-d`, #R2 `-v`).
No source was changed by this audit.
