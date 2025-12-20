
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

Contributions are welcome.  Developers and non-developers alike, please read [CONTRIBUTING](CONTRIBUTING.md) for details.

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

(none)

## Stage 5 - Fully Translated to 2+ languages

(none)

## Stage 4 - Code coverage

(none)

## Stage 3 - Test coverage

 - [x] admin (SCCS)
 - [x] ar (Development)
 - [x] asa
 - [x] at (cron cat.)
 - [x] awk
 - [x] basename
 - [x] bc
 - [x] c99 (Development)
 - [x] cksum
 - [x] cmp
 - [x] comm
 - [x] compress (compress cat.)
 - [x] cp
 - [x] crontab (cron cat.)
 - [x] csplit
 - [x] cut
 - [x] delta (SCCS)
 - [x] diff
 - [x] dirname
 - [x] du
 - [x] echo
 - [x] ex (Editors)
 - [x] expand
 - [x] expr
 - [x] false
 - [x] file
 - [x] find
 - [x] fold
 - [x] fuser
 - [x] gencat (i18n)
 - [x] get (SCCS)
 - [x] grep
 - [x] head
 - [x] iconv (i18n)
 - [x] join
 - [x] lex (Development)
 - [x] link
 - [x] ls
 - [x] m4
 - [x] mailx
 - [x] make
 - [x] mkdir
 - [x] more
 - [x] mv
 - [x] nl
 - [x] nm (Development)
 - [x] od
 - [x] paste
 - [x] pax
 - [x] pr
 - [x] printf
 - [x] prs (SCCS)
 - [x] readlink
 - [x] realpath
 - [x] rm
 - [x] rmdel (SCCS)
 - [x] rmdir
 - [x] sact (SCCS)
 - [x] sccs (SCCS)
 - [x] sed
 - [x] sh
 - [x] sort
 - [x] split
 - [x] strings
 - [x] strip (Development)
 - [x] tail
 - [x] test
 - [x] time
 - [x] timeout
 - [x] tr
 - [x] true
 - [x] uncompress (compress cat.)
 - [x] uudecode (uue)
 - [x] uuencode (uue)
 - [x] unexpand
 - [x] unget (SCCS)
 - [x] uniq
 - [x] unlink
 - [x] uucp (UUCP)
 - [x] uustat (UUCP)
 - [x] uux (UUCP)
 - [x] val (SCCS)
 - [x] vi (Editors)
 - [x] wc
 - [x] what (SCCS)
 - [x] yacc (Development)
 - [x] zcat (compress cat.)

## Stage 2 - Feature-complete and POSIX compliant

 - [x] batch (cron cat.)
 - [x] cat
 - [x] chgrp
 - [x] chmod
 - [x] chown
 - [x] date
 - [x] env
 - [x] ipcrm (IPC)
 - [x] ln
 - [x] logname
 - [x] man
 - [x] mesg
 - [x] mkfifo
 - [x] newgrp
 - [x] nice
 - [x] nohup
 - [x] pathchk
 - [x] pwd
 - [x] renice
 - [x] sleep
 - [x] talk
 - [x] tee
 - [x] touch
 - [x] tty
 - [x] uname
 - [x] write

## Stage 1 - Rough draft

 - [x] cal
 - [x] df
 - [x] dd
 - [x] getconf
 - [x] id
 - [x] ipcs (IPC)
 - [x] kill
 - [x] logger
 - [x] patch
 - [x] ps
 - [x] stty
 - [x] tabs
 - [x] tput
 - [x] tsort
 - [x] who
 - [x] xargs
 - [x] xgettext (i18n)

## Stage 0 - Not started

### Development category
 - [ ] cflow (Development)
 - [ ] ctags (Development)
 - [ ] cxref (Development)

### i18n category
 - [ ] gettext (i18n)
 - [ ] locale (i18n)
 - [ ] localedef (i18n)
 - [ ] msgfmt (i18n)
 - [ ] ngettext (i18n)

### Editors category
 - [ ] ed (Editors)

### Misc. category
 - [ ] lp

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
