
# posixutils-rs

This is a suite of Rust-native core command line utilties (cp, mv, c99,
m4, make, ...) using SuSv3 as the base POSIX specification.

## Goals

The goal is to create clean, race-free userland utilities that are POSIX
compliant, maximizing compatibility with existing shell scripts while
minimizing bloat.

Implementation goals include clean, safe Rust code and maximal use of
small Rust community crates.  This project's utilities should "look like
normal Rust programs."

Contributions are welcome.  Developers and helpers, please read [CONTRIBUTING](CONTRIBUTING.md) for details.

Core specification: https://pubs.opengroup.org/onlinepubs/9699919799/

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
1. Wider scope:  posixutils is far more ambitious than uutils from a breadth standpoint:  posixutils will include bc, m4, c99 compiler, fort77 compiler, a cron daemon etc.   uutils is far more limited in the scope of programs covered, mimicing GNU coreutils.
2. More minimalist:  Each posixutils utility _implementation_ is intentionally more minimalist, intending to avoid the bloat of supporting rarely-used, non-POSIX features.  Our common denominator and baseline is the POSIX spec, then add non-POSIX features that users cannot live without.
3. Transportable:  Each posixutils utility should look like normal Rust code, easily stand alone with little-or-no deps, and be used in another project.   This project is MIT-licensed, not GPL licensed, to aid in that transportability goal.

## Checklist of utilities

 - [ ] admin (SCCS)
 - [x] ar (Development)
 - [x] asa
 - [ ] at (cron cat.)
 - [ ] awk
 - [x] basename
 - [ ] batch (cron cat.)
 - [x] bc
 - [ ] c99 (Development)
 - [x] cal
 - [x] cat
 - [ ] cflow (Development)
 - [x] chgrp
 - [x] chmod
 - [x] chown
 - [x] cksum
 - [x] cmp
 - [x] comm
 - [x] compress (compress cat.)
 - [x] cp
 - [ ] crontab (cron cat.)
 - [x] csplit
 - [ ] ctags (Development)
 - [x] cut
 - [ ] cxref (Development)
 - [x] date
 - [x] dd
 - [ ] delta (SCCS)
 - [x] df
 - [x] diff
 - [x] dirname
 - [x] du
 - [x] echo
 - [ ] ed
 - [x] env
 - [ ] ex
 - [x] expand
 - [x] expr
 - [x] false
 - [x] file
 - [ ] find
 - [x] fold
 - [ ] fort77 (Development)
 - [ ] fuser
 - [ ] gencat (i18n)
 - [ ] get (SCCS)
 - [ ] getconf
 - [ ] grep
 - [x] head
 - [ ] iconv (i18n)
 - [x] id
 - [x] ipcrm (IPC)
 - [ ] ipcs (IPC)
 - [ ] join
 - [x] kill
 - [ ] lex (Development)
 - [x] link
 - [x] ln
 - [ ] locale
 - [ ] localedef
 - [x] logger
 - [x] logname
 - [ ] lp
 - [x] ls
 - [ ] m4
 - [ ] mailx
 - [ ] make
 - [ ] man
 - [x] mesg
 - [x] mkdir
 - [x] mkfifo
 - [ ] more
 - [x] mv
 - [ ] newgrp
 - [x] nice
 - [x] nl
 - [x] nm (Development)
 - [x] nohup
 - [ ] od
 - [x] paste
 - [ ] patch
 - [x] pathchk
 - [ ] pax
 - [x] pr
 - [x] printf
 - [ ] prs (SCCS)
 - [ ] ps
 - [x] pwd
 - [ ] qalter (Batch cat.)
 - [ ] qdel (Batch cat.)
 - [ ] qhold (Batch cat.)
 - [ ] qmove (Batch cat.)
 - [ ] qmsg (Batch cat.)
 - [ ] qrerun (Batch cat.)
 - [ ] qrls (Batch cat.)
 - [ ] qselect (Batch cat.)
 - [ ] qsig (Batch cat.)
 - [ ] qstat (Batch cat.)
 - [ ] qsub (Batch cat.)
 - [x] renice
 - [x] rm
 - [ ] rmdel (SCCS)
 - [x] rmdir
 - [ ] sact (SCCS)
 - [ ] sccs (SCCS)
 - [ ] sed
 - [ ] sh
 - [x] sleep
 - [x] sort
 - [x] split
 - [x] strings
 - [x] strip (Development)
 - [x] stty
 - [x] tabs
 - [ ] tail
 - [ ] talk
 - [x] tee
 - [ ] test
 - [ ] time
 - [x] touch
 - [x] tput
 - [x] tr
 - [x] true
 - [x] tsort
 - [x] tty
 - [x] uname
 - [x] uncompress (compress cat.)
 - [x] unexpand
 - [ ] unget (SCCS)
 - [x] uniq
 - [x] unlink
 - [ ] uucp (UUCP)
 - [x] uudecode (uue)
 - [x] uuencode (uue)
 - [ ] uustat (UUCP)
 - [ ] uux (UUCP)
 - [ ] val (SCCS)
 - [ ] vi
 - [x] wc
 - [ ] what (SCCS)
 - [x] who
 - [ ] write
 - [x] xargs
 - [ ] yacc (Development)
 - [x] zcat (compress cat.)

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
