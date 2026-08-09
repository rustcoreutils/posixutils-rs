
# posixutils-rs

This is a suite of Rust-native core command line utilities (cp, mv,
awk, make, vi, ...) using POSIX.2024 as the baseline specification.

## Goals

The goal is to create clean, race-free userland utilities that are POSIX
compliant, maximizing compatibility with existing shell scripts while
minimizing bloat.

Implementation goals include clean, safe Rust code and minimal deps beyond
std and libc.  This project's utilities should "look like normal Rust programs."

Core POSIX specification: https://pubs.opengroup.org/onlinepubs/9699919799/   (Old, free edition.  POSIX.2024 was just released.)

## Volunteers and contributors welcome!

Contributions are welcome!  Developers and non-developers alike, humans and agents, please read [CONTRIBUTING](CONTRIBUTING.md) for details.

**Note:** This project accepts contributions via **GitHub Issues only** (not Pull Requests). See CONTRIBUTING.md for details on this policy.

### Non-goals

It is _not_ a goal to be compatible with GNU utilities, which are
sometimes viewed as bloated and overloaded with rarely-used options.

Popular GNU options will be supported by virtue of the "don't break
scripts" rule.  Unpopular options will not be implemented, to prevent
bloat.

## Similar projects

A project with more narrow scope, with the aim of GNU coreutils compatibility, is **uutils**: https://github.com/uutils/coreutils

Because it is a FAQ, the major differences between this project and uutils are:
1. Wider scope:  posixutils is far more ambitious than uutils from a breadth standpoint:  posixutils will include bc, m4, c99 compiler, a cron daemon etc.   uutils is far more limited in the scope of programs covered, mimicing GNU coreutils.
2. More minimalist:  Each posixutils utility _implementation_ is intentionally more minimalist, intending to avoid the bloat of supporting rarely-used, non-POSIX features.  Our common denominator and baseline is the POSIX spec, then add non-POSIX features that users cannot live without.
3. Transportable:  Each posixutils utility should look like normal Rust code, easily stand alone with little-or-no deps, and be used in another project.   This project is MIT-licensed, not GPL licensed, to aid in that transportability goal.

A similar project from the author, written in C++ and the genesis of this project, is https://github.com/jgarzik/posixutils

## Utilities

140+ utilities, grouped by category and sorted alphabetically within each.

`lib/utils.tsv` maps each utility to the source file or directory that
implements it.

| Category | Utilities |
|---|---|
| Archive | cpio, pax, tar |
| Calculation | bc, expr |
| Cron | at, batch, crond, crontab |
| Date and time | cal, date, sleep, time |
| Development | ar, c17, cflow, ctags, cxref, lex, m4, make, nm, strings, strip, yacc |
| Editors | ed, ex, vi |
| Files and directories | cat, chgrp, chmod, chown, cmp, cp, dd, df, du, file, find, link, ln, ls, mkdir, mkfifo, mv, od, readlink, rm, rmdir, split, tee, touch, unlink |
| Internationalization | gencat, gettext, iconv, locale, localedef, msgfmt, ngettext, xgettext |
| Mail and messaging | mailx, mesg, talk, talkd, write |
| Pathnames | basename, dirname, pathchk, realpath |
| Printing | lp |
| Process management | env, fuser, kill, nice, nohup, ps, renice, timeout, xargs |
| SCCS | admin, delta, get, prs, rmdel, sact, sccs, unget, val, what |
| Shell | echo, false, printf, sh, test, true |
| System and users | getconf, id, ipcrm, ipcs, logger, logname, man, newgrp, pwd, tty, uname, who |
| Terminal | more, stty, tabs, tput |
| Text processing | asa, awk, comm, csplit, cut, diff, expand, fold, grep, head, join, nl, paste, patch, pr, sed, sort, tail, tr, tsort, unexpand, uniq, wc |
| Transform and encoding | cksum, compress, uncompress, uudecode, uuencode, zcat |
| UUCP | uucp, uustat, uux |

## Non-POSIX extensions

Utilities provided beyond those POSIX specifies.

 * tar - pax compatibility front-end, forcing the ustar format (Archive)
 * cpio - pax compatibility front-end, forcing the cpio format (Archive)
 * crond - cron daemon executing crontab, at and batch jobs (cron cat.)
 * talkd - local-only talk daemon, Unix socket rather than UDP port 518 (Users)

`tar` and `cpio` are installed as symlinks to `pax`, which picks its
command-line parser from `argv[0]`.  Three further symlinks name
POSIX-specified utilities and are not extensions: `ex` -> vi, and
`zcat` / `uncompress` -> compress.

See [NONPOSIX.md](NONPOSIX.md) for the full inventory of non-POSIX extensions:
extra options, language and syntax extensions, environment variables and file
formats, plus the places where we deviate from what POSIX specifies.

## Installation

These are "core" utilities of any operating system.  Production packaging in the future will be done on a per-distro basis in a distro-specific way.

As such, Dockerfiles, rpm and deb packaging are welcome, but currently considered a secondary priority to finishing, bugfixing and tuning the utilities.  Packaging contributions are welcome...  if done right.

The standard `cargo install` should work, for those interested in testing.  Care should be taken with PATH to point to the correct `cp` or `awk`, when mixing with standard system utilities on an already-shipped operating system.

## Testing

A few tests require additional setup such as a case-insensitive filesystem or the use of another command like [script](https://www.man7.org/linux/man-pages/man1/script.1.html). They are locked under the `posixutils_test_all` feature flag to exclude them from GitHub CI. These tests can be run by passing the feature flag to `cargo`:

```sh
cargo test --release --features posixutils_test_all
```

A further subset of `posixutils_test_all` tests are marked as `requires_root`. Running as root would override Unix permissions and thus give false failures on tests where such permissions are expected to be upheld so it is recommended to run these tests individually. There are currently 4 such tests:

- test_cp_special_bits
- test_mv_sticky_to_xpart
- test_rm_fail_2eperm
- test_rm_no_give_up

```sh
sudo -E cargo test --release --features posixutils_test_all,requires_root <test_name>
```

Integration tests may generate test data under `CARGO_TARGET_TMPDIR` (usually resolves to `target/tmp`) and `/dev/shm` (Linux only).
