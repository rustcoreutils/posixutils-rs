
# posixutils-rs

This is a suite of Rust-native core command line utilties (cp, mv,
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

Contributions are welcome.  Developers and non-developers alike, please read [CONTRIBUTING](CONTRIBUTING.md) for details, and [WANTED](WANTED.md) for recommended coding tasks for newcomers.

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

 - [x] ar (Development)
 - [x] awk
 - [x] basename
 - [x] bc
 - [x] cksum
 - [x] cmp
 - [x] comm
 - [x] compress (compress cat.)
 - [x] cp
 - [x] csplit
 - [x] cut
 - [x] diff
 - [x] dirname
 - [x] expand
 - [x] expr
 - [x] false
 - [x] file
 - [x] find
 - [x] fold
 - [x] gencat (i18n)
 - [x] grep
 - [x] head
 - [x] join
 - [x] link
 - [x] ls
 - [x] m4
 - [x] mkdir
 - [x] mv
 - [x] nl
 - [x] nm (Development)
 - [x] od
 - [x] paste
 - [x] pr
 - [x] readlink
 - [x] realpath
 - [x] rm
 - [x] rmdir
 - [x] sort
 - [x] split
 - [x] strings
 - [x] strip (Development)
 - [x] tail
 - [x] time
 - [x] tr
 - [x] true
 - [x] uncompress (compress cat.)
 - [x] uudecode (uue)
 - [x] uuencode (uue)
 - [x] unexpand
 - [x] uniq
 - [x] unlink
 - [x] wc
 - [x] what (SCCS)
 - [x] zcat (compress cat.)

## Stage 2 - Feature-complete and POSIX compliant

 - [x] cat
 - [x] chgrp
 - [x] chmod
 - [x] chown
 - [x] date
 - [x] env
 - [x] ipcrm (IPC)
 - [x] ln
 - [x] logname
 - [x] mesg
 - [x] mkfifo
 - [x] nice
 - [x] nohup
 - [x] pathchk
 - [x] pwd
 - [x] renice
 - [x] sleep
 - [x] tee
 - [x] touch
 - [x] tty
 - [x] uname
 - [x] write

## Stage 1 - Rough draft

 - [x] asa
 - [x] cal
 - [x] df
 - [x] du
 - [x] echo
 - [x] dd
 - [x] getconf
 - [x] id
 - [x] ipcs (IPC)
 - [x] kill
 - [x] logger
 - [x] printf
 - [x] ps
 - [x] stty
 - [x] tabs
 - [x] test
 - [x] tput
 - [x] tsort
 - [x] who
 - [x] xargs

## Stage 0 - Not started

### Cron category
 - [ ] at (cron cat.)
 - [ ] batch (cron cat.)
 - [ ] crontab (cron cat.)

### Development category
 - [ ] c17 (Development) -- Volunteer starting point at https://github.com/rustcoreutils/posixutils-rs/tree/c99
 - [ ] cflow (Development)
 - [ ] ctags (Development)
 - [ ] cxref (Development)
 - [ ] lex (Development) -- Volunteer starting point at https://github.com/rustcoreutils/posixutils-rs/tree/lex
 - [ ] yacc (Development) -- Volunteer starting point at https://github.com/rustcoreutils/posixutils-rs/tree/yacc

### SCCS category
 - [ ] admin (SCCS)
 - [ ] delta (SCCS)
 - [ ] get (SCCS)
 - [ ] prs (SCCS)
 - [ ] rmdel (SCCS)
 - [ ] sact (SCCS)
 - [ ] sccs (SCCS)
 - [ ] unget (SCCS)
 - [ ] val (SCCS)

 ### i18n category
 - [ ] gettext (i18n)
 - [ ] iconv (i18n) (status: in progress)
 - [ ] locale (i18n)
 - [ ] localedef (i18n)
 - [ ] msgfmt (i18n)
 - [ ] ngettext (i18n)
 - [ ] xgettext (i18n)

### UUCP category 
 - [ ] uucp (UUCP)
 - [ ] uustat (UUCP)
 - [ ] uux (UUCP)

### Editors category
 - [ ] ed (Editors)
 - [ ] ex (Editors) -- Volunteer starting point at https://github.com/rustcoreutils/posixutils-rs/tree/ed
 - [ ] vi (Editors)

### Misc. category
 - [ ] fuser (status: in progress)
 - [ ] lp
 - [ ] mailx
 - [ ] make (status: in progress)
 - [ ] man (status: in progress)
 - [ ] more
 - [ ] newgrp
 - [ ] patch (status: in progress)
 - [ ] pax
 - [ ] sed
 - [ ] sh -- Volunteer starting point at https://github.com/rustcoreutils/posixutils-rs/tree/shell
 - [ ] talk (status: in progress)
 - [ ] timeout (status: in progress)

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
