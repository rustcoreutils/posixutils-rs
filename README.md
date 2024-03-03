
# posixutils-rs

This is a suite of Rust-native core command line utilties (cp, mv, rm,
ls, ...) using SuSv3 as the base POSIX specification.

## Goals

The goal is to create clean, race-free userland utilities that are POSIX
compliant, maximizing compatibility with existing shell scripts while
minimizing bloat.

Implementation goals include clean, safe Rust code and maximal use of
Rust community crates.  This project's utilities should "look like
normal Rust programs."

Core specification: https://pubs.opengroup.org/onlinepubs/9699919799/

### Non-goals

It is _not_ a goal to be compatible with GNU utilities, which are
sometimes viewed as bloated and overloaded with rarely-used options.

Popular GNU options will be supported by virtue of the "don't break
scripts" rule.  Unpopular options will not be implemented, to prevent
bloat.

## Similar projects

A similar project with the aim of GNU compatibility is
https://github.com/uutils/coreutils

A similar project from the author, written in C++, is
https://github.com/jgarzik/posixutils

## Checklist of utilities

 - [ ] admin (SCCS)
 - [ ] ar (Development)
 - [x] asa
 - [ ] at (cron cat.)
 - [ ] awk
 - [x] basename
 - [ ] batch (cron cat.)
 - [ ] bc
 - [ ] c99 (Development)
 - [ ] cal
 - [x] cat
 - [ ] cflow (Development)
 - [ ] chgrp
 - [ ] chmod
 - [ ] chown
 - [x] cksum
 - [ ] cmp
 - [ ] comm
 - [ ] compress (compress cat.)
 - [ ] cp
 - [ ] crontab (cron cat.)
 - [ ] csplit
 - [ ] ctags (Development)
 - [ ] cut
 - [ ] cxref (Development)
 - [ ] date
 - [ ] dd
 - [ ] delta (SCCS)
 - [ ] df
 - [ ] diff
 - [x] dirname
 - [ ] du
 - [x] echo
 - [ ] ed
 - [x] env
 - [ ] ex
 - [ ] expand
 - [ ] expr
 - [x] false
 - [ ] file
 - [ ] find
 - [ ] fold
 - [ ] fort77 (Development)
 - [ ] fuser
 - [ ] gencat (i18n)
 - [ ] get (SCCS)
 - [ ] getconf
 - [ ] getopts
 - [ ] grep
 - [x] head
 - [ ] iconv (i18n)
 - [ ] id
 - [x] ipcrm (IPC)
 - [ ] ipcs (IPC)
 - [ ] join
 - [ ] kill
 - [ ] lex (Development)
 - [x] link
 - [x] ln
 - [ ] locale
 - [ ] localedef
 - [x] logger
 - [x] logname
 - [ ] lp
 - [ ] ls
 - [ ] m4
 - [ ] mailx
 - [ ] make
 - [ ] man
 - [ ] mesg
 - [ ] mkdir
 - [ ] mkfifo
 - [ ] more
 - [ ] mv
 - [ ] newgrp
 - [x] nice
 - [ ] nl
 - [ ] nm (Development)
 - [ ] nohup
 - [ ] od
 - [x] paste
 - [ ] patch
 - [ ] pathchk
 - [ ] pax
 - [ ] pr
 - [ ] printf
 - [ ] prs (SCCS)
 - [ ] ps
 - [ ] pwd
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
 - [ ] rm
 - [ ] rmdel (SCCS)
 - [x] rmdir
 - [ ] sact
 - [ ] sccs (SCCS)
 - [ ] sed
 - [ ] sh
 - [x] sleep
 - [ ] sort
 - [x] split
 - [ ] strings
 - [ ] strip (Development)
 - [ ] stty
 - [ ] tabs
 - [ ] tail
 - [ ] talk
 - [x] tee
 - [ ] test
 - [ ] time
 - [ ] touch
 - [ ] tput
 - [ ] tr
 - [x] true
 - [ ] tsort
 - [x] tty
 - [ ] ulimit
 - [x] uname
 - [ ] uncompress (compress cat.)
 - [ ] unexpand
 - [ ] unget
 - [ ] uniq
 - [x] unlink
 - [ ] uucp (UUCP)
 - [x] uudecode (uue)
 - [x] uuencode (uue)
 - [ ] uustat (UUCP)
 - [ ] uux (UUCP)
 - [ ] val (SCCS)
 - [ ] vi
 - [ ] wait
 - [x] wc
 - [ ] what (SCCS)
 - [ ] who
 - [ ] write
 - [ ] xargs
 - [ ] yacc (Development)
 - [ ] zcat (compress cat.)

