
# posixutils-rs

This is a suite of Rust-native core command line utilities (cp, mv,
awk, make, vi, ...) using POSIX.2024 as the baseline specification.

## Goals

The goal is to create clean, race-free userland utilities that are POSIX
compliant, maximizing compatibility with existing shell scripts while
minimizing bloat.

Implementation goals include clean, safe Rust code and maximal use of
small Rust community crates.  This project's utilities should "look like
normal Rust programs."

Core POSIX specification: https://pubs.opengroup.org/onlinepubs/9699919799/   (Old, free edition.  POSIX.2024 was just released.)

## WANTED:  Volunteers!

Contributions are welcome!  Developers and non-developers alike, please read [CONTRIBUTING](CONTRIBUTING.md) for details.

**Note:** This project accepts contributions via **GitHub Issues only** (not Pull Requests). See CONTRIBUTING.md for details on this policy.

### Non-goals

It is _not_ a goal to be compatible with GNU utilities, which are
sometimes viewed as bloated and overloaded with rarely-used options.

Popular GNU options will be supported by virtue of the "don't break
scripts" rule.  Unpopular options will not be implemented, to prevent
bloat.

## Similar projects

A similar project from the author, written in C++, is
https://github.com/jgarzik/posixutils

A project with more narrow scope, with the aim of GNU coreutils compatibility, is uutils: https://github.com/uutils/coreutils

Because it is a FAQ, the major differences between this project and uutils are:
1. Wider scope:  posixutils is far more ambitious than uutils from a breadth standpoint:  posixutils will include bc, m4, c99 compiler, a cron daemon etc.   uutils is far more limited in the scope of programs covered, mimicing GNU coreutils.
2. More minimalist:  Each posixutils utility _implementation_ is intentionally more minimalist, intending to avoid the bloat of supporting rarely-used, non-POSIX features.  Our common denominator and baseline is the POSIX spec, then add non-POSIX features that users cannot live without.
3. Transportable:  Each posixutils utility should look like normal Rust code, easily stand alone with little-or-no deps, and be used in another project.   This project is MIT-licensed, not GPL licensed, to aid in that transportability goal.

## Utility status

## Stage 6 - Audited

 - [x] admin (SCCS)
 - [x] ar (Development)
 - [x] at (cron cat.)
 - [x] awk
 - [x] basename
 - [x] batch (cron cat.)
 - [x] bc
 - [x] cat
 - [x] chgrp
 - [x] chmod
 - [x] chown
 - [x] cmp
 - [x] cp
 - [x] crontab (cron cat.)
 - [x] dd
 - [x] delta (SCCS)
 - [x] df
 - [x] dirname
 - [x] du
 - [x] ed (Editors)
 - [x] env
 - [x] ex (Editors)
 - [x] expr
 - [x] false
 - [x] file
 - [x] find
 - [x] fuser
 - [x] gencat (i18n)
 - [x] get (SCCS)
 - [x] gettext (i18n)
 - [x] iconv (i18n)
 - [x] kill
 - [x] lex (Development)
 - [x] link
 - [x] ln
 - [x] locale (i18n)
 - [x] ls
 - [x] lp
 - [x] m4
 - [x] mailx
 - [x] make
 - [x] man
 - [x] mkdir
 - [x] mkfifo
 - [x] more
 - [x] msgfmt (i18n)
 - [x] mv
 - [x] ngettext (i18n)
 - [x] nice
 - [x] nm (Development)
 - [x] nohup
 - [x] od
 - [x] pathchk
 - [x] pax
 - [x] prs (SCCS)
 - [x] readlink
 - [x] realpath
 - [x] renice
 - [x] rm
 - [x] rmdel (SCCS)
 - [x] rmdir
 - [x] sact (SCCS)
 - [x] sccs (SCCS)
 - [x] sh
 - [x] split
 - [x] strings
 - [x] strip (Development)
 - [x] stty
 - [x] tabs
 - [x] tee
 - [x] test
 - [x] timeout
 - [x] touch
 - [x] tput
 - [x] true
 - [x] unget (SCCS)
 - [x] unlink
 - [x] uucp (UUCP)
 - [x] uustat (UUCP)
 - [x] uux (UUCP)
 - [x] val (SCCS)
 - [x] vi (Editors)
 - [x] what (SCCS)
 - [x] xargs
 - [x] xgettext (i18n)
 - [x] yacc (Development)

## Stage 5 - Fully Translated to 2+ languages

(none)

## Stage 4 - Code coverage

(none)

## Stage 3 - Test coverage

 - [x] asa
 - [x] c99 (Development)
 - [x] cal
 - [x] cflow (Development)
 - [x] cksum
 - [x] comm
 - [x] compress (compress cat.)
 - [x] csplit
 - [x] ctags (Development)
 - [x] cut
 - [x] cxref (Development)
 - [x] diff
 - [x] echo
 - [x] expand
 - [x] fold
 - [x] getconf
 - [x] grep
 - [x] head
 - [x] join
 - [x] localedef (i18n)
 - [x] logger
 - [x] nl
 - [x] paste
 - [x] patch
 - [x] pr
 - [x] printf
 - [x] ps
 - [x] sed
 - [x] sort
 - [x] talk (with talkd local daemon)
 - [x] tail
 - [x] time
 - [x] tr
 - [x] tsort
 - [x] tty
 - [x] uname
 - [x] uncompress (compress cat.)
 - [x] uudecode (uue)
 - [x] uuencode (uue)
 - [x] unexpand
 - [x] uniq
 - [x] wc
 - [x] who
 - [x] write
 - [x] id
 - [x] ipcs (IPC)
 - [x] zcat (compress cat.)

## Stage 2 - Feature-complete and POSIX compliant

 - [x] date
 - [x] ipcrm (IPC)
 - [x] logname
 - [x] mesg
 - [x] newgrp
 - [x] pwd
 - [x] sleep

## Stage 1 - Rough draft

(none)

## Stage 0 - Not started

(none)

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
