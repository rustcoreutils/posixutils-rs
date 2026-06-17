# POSIX.1-2024 Conformance Audits — `fs/` utilities

This file collects per-utility POSIX conformance audits for the filesystem
utilities crate. Each audit follows the playbook in `audits.md`.

**Scope note.** The `fs/` crate currently ships exactly **one** POSIX utility,
`df` (binary declared in `fs/Cargo.toml`), plus a Linux-only mount-table helper
module `mntent.rs`. The other "filesystem" utilities have been re-homed into
sibling crates and audited separately: `cp`/`mv`/`rm` live in `tree/` (see
[`tree/audit.md`](../tree/audit.md)); `basename`/`dirname`/`pathchk`/`realpath`
in `pathnames/` (see [`pathnames/audit.md`](../pathnames/audit.md)). This audit
therefore covers `df` only.

---

## `df`

**Implementation:** `fs/df.rs` (392 lines) + `fs/mntent.rs` (156 lines, Linux mount-table reader)
**Tests:** `fs/tests/df/mod.rs` (123 lines, 7 `#[test]`s)
**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 §3, pp. 2839–2842 (lines 92372–92524)
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/df.md`
**Date:** 2026-06-17

### TL;DR

The block-figure path (`-k`, `-P`, default 512-byte units, the `-P` data-line
format, and the size/used/available column values) is largely correct and the
`-P`/`-k` header strings match the spec in the POSIX locale. But `df` here is
materially incomplete and has three real defects: the **`-t` option is absent**,
the **default (and `-t`) output never reports free inodes / "file slots"** even
though the DESCRIPTION makes that a `shall`, and the **percentage-used
calculation uses the wrong denominator** (`f_bfree` instead of `f_bavail`), so
the Capacity column diverges from `df -P` everywhere a filesystem reserves
blocks and can never exceed 100% as the spec requires. Two robustness problems
compound this: a **non-UTF-8 mount path panics** the whole utility on the
golden no-operand path, and a **file operand that errors or matches no
filesystem silently falls back to printing every filesystem**. Locale init
(`setlocale`/`textdomain`) is wired, but runtime diagnostics are hardcoded
English.

### Priority issues

#### Critical

- [x] **#1 — Non-UTF-8 mount device/path aborts the whole utility (panic).** `fs/df.rs:282-283` (`MountList::push`) does `devname.to_str().unwrap()` and `dirname.to_str().unwrap()`; `fs/df.rs:325` does `mount.dir.to_str().unwrap()` in the statfs error path. Mount `fsname`/`dir` come straight from the kernel mount table (`getmntent`) and may contain arbitrary, non-UTF-8 bytes (e.g. removable media auto-mounted under a label with high bytes). Any such entry makes the bare `df` invocation `panic!` and print nothing. Fix: carry device/mount names as `OsString`/`Vec<u8>` (or `to_string_lossy()`), never `unwrap()` on `to_str()`. **✓ fixed Phase 1** — `Mount.devname`/`dir` are now `OsString` built from the raw `CStr` bytes; all display goes through `to_string_lossy()`; the enumeration error path uses `Path::display()`.

#### Major

- [ ] **#2 — `-t` option is unimplemented.** `fs/df.rs:22-43` (`Args`) declares only `-k` and `-P`; `grep -niE '\-t|total' df.rs` finds nothing. SYNOPSIS (92375) is `df [-k] [-P|-t] [file...]` and OPTIONS (92391, XSI) mandates `-t` ("Include total allocated-space figures in the output"). `df -t` currently errors as an unknown option. Fix: add a `total` flag; in `-t` mode print total allocated space alongside the (inode-bearing) default output, and enforce the `-P|-t` mutual exclusion from the synopsis.
- [ ] **#3 — Default and `-t` output omit free inodes / file slots.** `fs/df.rs:75-185` define only block columns (size/used/avail/pcent); `f_files`/`f_ffree` are never read. DESCRIPTION (92377-92384): the output "shall contain at least the file system names, amount of available space … and, **if no options other than −t are specified, the number of free file slots, or inodes, available**". The opening sentence (92377) likewise says df reports "available space **and file slots**". The default (no-option) and `-t`-only invocations therefore violate a `shall`. (`-k`/`-P` relax this, so the fixed `-P` format is fine.) Fix: in default/`-t` modes add IFree/IUsed/etc. columns sourced from `statfs.f_ffree`/`f_files`.
- [ ] **#4 — Percentage-used uses the wrong denominator (`f_bfree` not `f_bavail`).** `fs/df.rs:229-230`: `percentage_used = used / (used + free)` where `free = f_bfree` (`fs/df.rs:224`). Per STDOUT (92450-92456) the percentage is over "normally available space" and "may be greater than 100 if `<space free>` is less than zero" — and `<space free>` (92445-92449) is the space available to **unprivileged** users, i.e. `f_bavail`, which the code already computes as `avail` (`fs/df.rs:223`). With `used + free == total`, the result is `used/total`, which (a) disagrees with `df -P`/coreutils on any filesystem that reserves blocks (reserved blocks inflate the denominator, understating Capacity) and (b) can never exceed 100%, contradicting the spec note. Fix: `percentage_used = used as f64 / (used + avail) as f64` (the `f_bavail`-based figure).
- [x] **#5 — A failed or unmatched `file` operand falls back to printing all filesystems.** `fs/df.rs:263-268` (`ensure_masked`) masks **every** mount whenever `has_masks` is still false, and `mask_fs_by_file` (`fs/df.rs:338-355`) only sets `has_masks` when a mount's `dev` matches. So `df /nonexistent` prints the error to stderr, sets exit 1 — **and still dumps every filesystem to stdout**; `df /valid/path` whose device fails to match any mount likewise prints all filesystems with exit 0. Fix: track masking per-invocation; if operands were given, never auto-mask the whole list — print only matched filesystems (and nothing on total failure). **✓ fixed Phase 1** — removed `ensure_masked`/`has_masks`; `mask_all()` runs only when `files.is_empty()`, else only `dev`-matched mounts print. Regression tests `test_df_operand_does_not_dump_all_filesystems` + `test_df_bad_operand_prints_no_filesystem_rows`.

#### Minor

- [ ] **#6 — Runtime diagnostics are hardcoded English.** `fs/df.rs:323-327` and `342` use `eprintln!("{}: {}", …)` with no `gettext()`. `setlocale`/`textdomain` are initialized (`fs/df.rs:358-360`) and column captions are translated, but LC_MESSAGES has no effect on error text. Fix: route the two diagnostic sites through `gettext()` (low payload — mostly an OS error string + path).
- [ ] **#7 — `-P|-t` mutual exclusion not enforced.** `fs/df.rs:22-43`. The synopsis groups `-P` and `-t` as alternatives. Currently moot (`-t` is absent, #2), but the fix for #2 must add `conflicts_with`.
- [x] **#8 — Newline in a displayed pathname is not treated as an error.** FUTURE DIRECTIONS (92502-92505) / Austin Group Defect 251 "encourages" implementations to error when an output pathname contains a `<newline>` (a separator in the `-P` format). Not yet mandatory; track only. Fix: when a `devname`/`dir` to be printed contains `\n`, diagnose and set non-zero exit. **✓ fixed Phase 1** — the print loop checks the raw `OsString` bytes of `devname`/`dir`; a `\n` skips the row with a stderr diagnostic and sets exit 1.
- [x] **#9 — File/device names and operands are forced through UTF-8 `String`.** `Args.files: Vec<String>` (`fs/df.rs:42`) and the `String` fields of `Mount` (`fs/df.rs:208-210`) mangle/refuse non-UTF-8 bytes. Subsumes #1's crash but is broader (operands too). Fix: use `OsString`/`PathBuf` end-to-end. **✓ fixed Phase 1** — `Args.files: Vec<PathBuf>`, `Mount` names `OsString`, `mask_fs_by_file(&Path)` stats via `CString::new(path.as_os_str().as_bytes())` (no `unwrap`/`expect` panic on internal NUL either).
- [ ] **#10 — `f_blocks * f_bsize` can overflow `u64`.** `fs/df.rs:222-224` multiplies before dividing by the unit; overflows only at ~16 EiB-scale filesystems. Fix: divide `f_bsize/block_size` first, or use `u128` intermediate.
- [ ] **#11 — Zero-block pseudo-filesystems print `0%` via a NaN path.** `fs/df.rs:229-230`: for `used+free == 0` (e.g. `/proc`, `/sys`), `0.0/0.0 = NaN`, `ceil(NaN) as u32 == 0`. No panic (saturating cast), but cosmetically diverges from coreutils. Fix: special-case a zero denominator.
- [ ] **#12 — statfs enumeration failures don't affect exit status.** `fs/df.rs:322-329`: a failed `statfs` on a system mount warns and `continue`s, leaving `exit_code == 0`. EXIT STATUS (92467-92469) returns `>0` when "an error occurred". Debatable (these are auto-enumerated, not user-requested), but arguably should propagate. Fix: set `exit_code = 1` on enumeration errors, or document the policy.

### Detailed conformance matrix

#### SYNOPSIS / argv parsing

- [x] `-k` and `-P` accepted, bundling/`--`/`-h`/`-V` handled by clap — `fs/df.rs:22-43`.
- [ ] **`-t` unsupported** (#2 Major) — `fs/df.rs:22-43`.
- [ ] **`-P|-t` not mutually exclusive** (#7 Minor; moot until #2).
- [x] `df` conforms to XBD 12.2 via clap default parsing.
- Note: `--kilo`/`--portable` long aliases are non-POSIX extensions (harmless).

#### OPTIONS

| Opt | Status | Notes (file:line) |
|---|---|---|
| `-k` | CONFORMS | `fs/df.rs:57-72`, `OutputMode` → 1024-byte units + `1024-blocks` header. |
| `-P` | CONFORMS | `fs/df.rs:167-185` emits the mandated `%s %d %d %d %d%% %s` data line (padding allowed per 92429). |
| `-t` | MISSING | (#2 Major) Not declared. |

#### OPERANDS / STDIN / INPUT FILES

- [x] `file` operand stats the path and masks the containing filesystem — `fs/df.rs:338-355`.
- [x] No operands → all filesystems — `fs/df.rs:367-368`.
- [x] STDIN "Not used"; no `stdin()` call anywhere.
- [x] **Unmatched/failed operand no longer prints all filesystems** (#5 ✓ Phase 1) — `ensure_masked` removed; operands mask only `dev`-matched mounts.
- [x] Non-special file operands accepted (results "unspecified" per 92394-92399 — legal). N/A.
- Note: operand→filesystem matching compares `file.st_dev` against a per-mount `dev` derived as `stat(devname).st_rdev` else `stat(dirname).st_dev` else `-1`; works for real block devices and via the dirname fallback for pseudo-filesystems.

#### ENVIRONMENT VARIABLES

| Var | Status | Notes |
|---|---|---|
| `LANG` | CONFORMS | `setlocale(LcAll, "")` at `fs/df.rs:358`. |
| `LC_ALL` | CONFORMS | same. |
| `LC_CTYPE` | CONFORMS (init) | `setlocale` honors it; names are now byte-faithful `OsString` (#9 ✓ Phase 1). |
| `LC_MESSAGES` | PARTIAL | (#6 Minor) Captions translated; runtime diagnostics hardcoded English. |
| `NLSPATH` (XSI) | PARTIAL | `textdomain`/`bind_textdomain_codeset` wired (`fs/df.rs:359-360`); no explicit catalog path handling. |

#### ASYNCHRONOUS EVENTS

- [x] Default — `df` is non-interactive; no handlers required. `grep -nE 'SIGCONT|SIGWINCH|signal' df.rs` → 0 matches (expected).

#### STDOUT / STDERR

- [x] `-k -P` header `Filesystem 1024-blocks Used Available Capacity Mounted on` — `fs/df.rs:131-155` (POSIX-locale captions match; column padding allowed per 92429).
- [x] `-P` (no `-k`) header `512-blocks` variant — same, via `OutputMode::PosixLegacy`.
- [x] `-P` data line format `%s %d %d %d %d%% %s` — `fs/df.rs:167-185`.
- [x] Diagnostics go to stderr — `fs/df.rs:323`, `342`.
- [ ] **Default (non-`-P`) output omits free inodes** (#3 Major) — uses the same 6 block columns instead of the spec-required file-slot count.
- [ ] **`-t` total-space output absent** (#2 Major).

#### OUTPUT FILES / EXTENDED DESCRIPTION

- [x] OUTPUT FILES "None"; EXTENDED DESCRIPTION "None" — nothing to verify.

#### Column value computation

- [x] `<total space>` = `f_blocks` scaled to unit — `fs/df.rs:222`. CONFORMS.
- [x] `<space used>` = `f_blocks − f_bfree` scaled — `fs/df.rs:224-225`. CONFORMS (allocated space).
- [x] `<space free>` (avail column) = `f_bavail` scaled — `fs/df.rs:223`. CONFORMS (unprivileged-available).
- [ ] **`<percentage used>` denominator wrong** (#4 Major) — `fs/df.rs:229-230` uses `f_bfree`, not `f_bavail`.
- [x] Percentage rounded up to next integer (`.ceil()`) — `fs/df.rs:230`. CONFORMS (modulo #4 denominator and #11 NaN).
- [ ] **Unit scaling can overflow** (#10 Minor) — `fs/df.rs:222`.

#### EXIT STATUS / CONSEQUENCES OF ERRORS

- [x] 0 on success — `fs/df.rs:365`, `391`.
- [x] Non-zero when a `file` operand cannot be stat'd — `fs/df.rs:371-373`.
- [ ] **statfs enumeration errors don't set exit status** (#12 Minor) — `fs/df.rs:322-329`.
- [x] CONSEQUENCES OF ERRORS = Default — no special policy required.

### `mntent.rs` notes (Linux mount-table helper)

- [x] RAII `setmntent`/`getmntent`/`endmntent` wrapper with a mutex around the
  non-thread-safe `getmntent` — `fs/mntent.rs:44-96`. Sound.
- [ ] Reads `/etc/mtab` (`_PATH_MOUNTED`, `fs/mntent.rs:17`). On modern systems
  this is usually a symlink to `/proc/self/mounts`, so it works, but a stale or
  absent `/etc/mtab` would make `df` fail where `/proc/mounts` would succeed.
  Track as Minor; consider `/proc/self/mounts` directly. (Not a spec item.)
- Feeds #1: the `CStr`→`String` conversion happens in `df.rs`, not here, so the
  helper itself is byte-faithful; the panic is on the consumer side.

### Test coverage signal

Existing tests (`fs/tests/df/mod.rs`) cover the header strings for default/`-k`/`-P`/`-k -P`, a `/` operand, a nonexistent-operand exit code, and `--help`. Gaps that map to findings:

- [ ] No test for `-t` (#2) — currently would assert an error.
- [ ] No test that default output contains a free-inode column (#3).
- [ ] No test pinning the Capacity value against a known reserved-block filesystem (#4).
- [ ] No test that `df /nonexistent` (or an unmatched valid path) prints **no** filesystem rows (#5).
- [ ] No test for a non-UTF-8 mount path (#1) — hard to stage portably; at minimum unit-test the name-conversion helper once it returns `OsString`.
- [ ] No test that diagnostics honor `LC_MESSAGES` (#6).

### Suggested PR groupings

- **PR A — "df: robustness on the enumeration path"**: #1 (non-UTF-8 panic), #9 (OsString end-to-end), #5 (no all-filesystems fallback on operand failure). All touch `MountList`/`push`/`ensure_masked` and the operand loop.
- **PR B — "df: correct Capacity percentage"**: #4 (use `f_bavail` denominator), #11 (zero-denominator pseudo-fs). Small, high-value, behaviorally checkable against `df -P`/coreutils.
- **PR C — "df: free inodes + `-t`"**: #3 (file-slot columns in default/`-t` mode), #2 (`-t` option), #7 (`-P|-t` exclusion). The largest feature unit; closes the two big DESCRIPTION `shall`s.
- **PR D — "df: diagnostics + exit-status hygiene"**: #6 (gettext diagnostics), #12 (exit status on enumeration error), #8 (newline-in-pathname guard), #10 (overflow-safe scaling).
