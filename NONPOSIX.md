# Non-POSIX Extensions

posixutils-rs takes POSIX.1-2024 (IEEE Std 1003.1-2024) as its baseline
specification, then adds only the non-POSIX behavior that users cannot live
without.  It is *not* a goal to be compatible with GNU utilities.

This document covers two things: **additions** — utilities, options,
environment variables, syntax and formats that POSIX does not define at all —
and **deviations** — places where we behave differently from what POSIX
specifies.

It does not record our choices in the areas POSIX leaves unspecified or
implementation-defined, nor unimplemented corners of the standard.  Those are
tracked per utility in the `audit.md` file of each crate — see the list in
`audits.md`.  A crate carries one only while it still has an open item; the
closed findings are in git history.

## Utilities beyond POSIX

Four binaries have no POSIX.1-2024 specification at all:

 * **tar** - `pax` compatibility front-end forcing the ustar format (Archive)
 * **cpio** - `pax` compatibility front-end forcing the cpio format (Archive)
 * **crond** - cron daemon executing `crontab`, `at` and `batch` jobs
 * **talkd** - local-only `talk` daemon (Unix-domain socket, not UDP port 518)

`tar` and `cpio` are installed as symlinks to `pax`, which selects its
command-line parser from `argv[0]`.  Four further symlinks exist for
convenience, but name POSIX-specified utilities and are not extensions:
`ex` -> `vi`, `zcat` / `uncompress` -> `compress`, and `[` -> `test`.
POSIX gives `test` both names and allows one binary to serve them by reading
`argv[0]`, which is what we do.

POSIX utilities that have no binary of their own — `cd`, `read`, `umask`,
`getopts`, `wait` and the rest — are shell built-ins provided by `sh`.

## Project-wide conventions

Two additions apply across nearly the whole suite and are not repeated per
utility below:

 * **Long-option aliases.**  Most utilities accept a GNU-style long option as
   an alias for each POSIX short option — `cut --fields` for `-f`,
   `ls --recursive` for `-R`, `df --portable` for `-P`, and so on.  These are
   aliases only: they never add behavior, and the POSIX short form is always
   accepted.  Long options that are *not* aliases for a POSIX short option are
   listed individually below.
 * **`--help` / `-h` and `--version` / `-V`.**  Accepted by nearly every
   utility, including those whose spec says `OPTIONS: None`.  The deliberate
   exceptions are `true` and `false`, which take no options at all.

## Extensions by utility

### ar

 * The key may be written without its leading `-`, in the traditional form
   Makefiles, libtool and automake's archiver probe use: `ar cr lib.a x.o`
   means `ar -cr lib.a x.o`.
 * `-s` is accepted with `-r` and `-q` (`ar rcs`), where POSIX allows it only
   with `-p`, `-t` and `-x`.  It changes nothing: `ar` writes the symbol table
   whenever it writes the archive.

### at

 * `AT_ALLOW`, `AT_DENY` — override the `at.allow` / `at.deny` pathnames.
 * `AT_JOB_DIR` — override the job spool directory.

   All three are honored only when the real and effective user IDs match.

### awk

 * C-style floating-point literal suffixes `f`, `l`, `F`, `L` are accepted in
   awk source.
 * `RS` accepts a multi-character string or a regular expression.  POSIX
   defines only the first character.

### bc

 * Interactive line editing and command history.
 * A single operation may not build more than one million decimal digits;
   beyond that it fails with `number too large` or `exponent is too large`.
   POSIX calls bc an arbitrary precision calculator, and the limits it does
   grant by name — `{BC_SCALE_MAX}`, `{BC_BASE_MAX}`, `{BC_DIM_MAX}`,
   `{BC_STRING_MAX}` — do not include a ceiling on a value's digit count.

### c17

The compiler accepts a large GCC/Clang-compatible surface so that real-world
source trees build unmodified.  None of it changes the meaning of
strictly-conforming C.

Options beyond the POSIX set (`-B -c -D -E -G -g -I -L -l -O -o -R -s -U`):

 * `-S` — emit assembly.
 * `-v` / `--verbose`, `--stats`, `--pedantic`, `-W <warning>`.
 * `--nostdinc`, `--nobuiltininc`, `--fno-builtin`, `--fno-unwind-tables`.
 * `-fpermissive` — accept two constructs C99 removed, as warnings rather
   than errors: implicit `int` in a declaration naming no type, and the
   implicit declaration of a function called before it is declared.  It is
   not a dialect switch; the language is still C17 and `-std=` stays inert.
   gcc draws the same line, rejecting both by default.
 * `--target <triple>`, `--shared`, `--rtlib`, `--print-targets`.
 * `--trigraphs` — enable trigraph replacement (off by default, since it
   would alter string literals).
 * `--dump-tokens`, `--dump-ast`, `--dump-ir`, `--dump-ir-func` — developer
   diagnostics.
 * A GCC-compatibility argument rewriter accepts and maps `-std=`, `-f*`,
   `-m*`, the `-fpic` family (`-fPIC`/`-fpic`/`-fPIE`/`-fpie` and their
   `-fno-` forms, last one wins), `-ftls-model=`, `-pie`/`-no-pie`,
   `-shared`, `-Wl,`, `-Xlinker`, `-pthread`, `-rdynamic`, `-pipe`, `-p`/`-pg`,
   and `-ffreestanding`/`-fhosted`.  The `-f*` options it knows are
   classified in `cc/f_options.rs`; one gcc would do something with that
   c17 does not (`-fsanitize=`, `-ftrapv`, ...) draws the warning
   group `-Wc17-unsupported-option`, which plain `-Werror` leaves a warning,
   and an unknown one is refused as gcc refuses it.
 * A bare `-` operand is accepted as a pathname.  POSIX says standard input is
   "Not used".

Language and preprocessor additions:

 * `__attribute__` (and `__attribute`), with `noreturn unused aligned packed
   deprecated weak section visibility constructor destructor used noinline
   always_inline hot cold warn_unused_result format fallthrough nonstring
   malloc pure sentinel no_sanitize_*` and their `__name__` spellings.
 * Statement expressions `({ ... })`.
 * `typeof` / `__typeof__` / `__typeof`, and the `__const`, `__volatile`,
   `__restrict`, `__inline`, `__signed`, `__extension__`, `__thread`,
   `__alignof__` keyword aliases.
 * GCC extended inline assembly, including `asm goto`; `asm` / `__asm__` /
   `__asm`.
 * `__int128`, `__int128_t`, `__uint128_t`, `_Float16`, `_Float32`, `_Float64`,
   `__builtin_va_list`.
 * GNU imaginary constants — `1.0i`, `2.2if`, `1.0fi`, `2.2iL`, `1.j`.  The
   marker may sit on either side of the floating suffix.  C spells this
   `_Imaginary`, which Annex G makes optional and neither c17 nor gcc
   provides; both give the constant a complex type with a zero real part.
   The integer form `2i`, which gcc types `_Complex int`, is not accepted.
 * Clang nullability qualifiers `_Nonnull`, `_Nullable`, `_Null_unspecified`
   and their `__` spellings.
 * Roughly 145 `__builtin_*` and `__c11_atomic_*` intrinsics, including the
   `__builtin___*_chk` FORTIFY family and the libc aliases (`__builtin_abort`,
   `__builtin_printf`, `__builtin_strcpy`, ...) that let a translation unit
   call one without having included the header that declares it.
 * `__FUNCTION__`, `__PRETTY_FUNCTION__`.
 * `#include_next`, `#warning`, `#pragma once`, `#pragma push_macro` and
   `#pragma pop_macro`.
 * `__has_attribute`, `__has_builtin`, `__has_feature`, `__has_extension`,
   `__has_include`, `__has_include_next`.
 * Named variadic macro parameters (`#define F(args...)`), an empty variadic
   tail, and the `, ## __VA_ARGS__` comma-swallow.
 * Empty `enum { }` (diagnosed under `--pedantic`).
 * Digraphs, and the C23 one-argument `_Static_assert`.
 * `__GNUC__`, `__GNUC_MINOR__`, `__GNUC_PATCHLEVEL__`, `__VERSION__` and
   `__GNUC_STDC_INLINE__` are predefined.
 * On Linux, `_GNU_SOURCE`, `_XOPEN_SOURCE=800` and `_XOPEN_SOURCE_EXTENDED`
   are predefined unconditionally.

Deviation: `-std=` selects nothing — the language is C17 and
`__STDC_VERSION__` is `201710L` whatever it names.  A C99, C11 or C17
spelling is taken in silence; C90 (`-ansi` included) draws a warning that
`-Wno-c17-dialect` silences; a revision after C17 is an error.

### cat

 * `-v` / `--show-nonprinting` — write nonprinting bytes visibly, as GNU and
   BSD cat do: a control character as `^X`, DEL as `^?`, and a byte above
   127 as `M-` and the form of the byte 128 below it (`M-^I`, `M-a`).  Tab
   and newline are written as they are.  Bytes, not characters: a UTF-8
   sequence shows as its bytes.
 * `-e` — `-v`, and a `$` at the end of each line.

### cflow

 * `.S` operands — assembler source that is preprocessed before it is
   assembled, following the GCC convention.  POSIX names only `.s` among the
   assembler suffixes, and says such files "may have more limited information
   extracted from them"; `-D`, `-U` and `-I` reach a `.S` exactly as they
   reach C source.  Accepted because `c17` accepts it, so the two tools read
   the same files.

### cmp

 * `-n count` — compare at most `count` bytes.
 * `skip1` and `skip2` operands — decimal byte counts to skip in the first and
   second file before comparing; byte and line numbers count from the first
   byte compared.  Both forced by util-linux's mkswap test
   (`cmp -n OFFSET IMG /dev/zero`, `cmp IMG IMG.offset 0 OFFSET`).

### chown

 * An `owner:` operand with an empty group resolves the group to the owner's
   login group.

### compress / uncompress / zcat

 * A `-` operand names standard input.  The `compress` spec never mentions it.
 * `uncompress` looks for `file`, then `file.Z`, then `file.gz`.

### cp

 * `-r` is accepted as a short alias for `-R`.  POSIX.1-2024 removed `-r`.
 * A `source/.` operand copies the contents of `source` rather than the
   directory itself.

The rest are the GNU options debhelper passes for nearly every Debian
package (`cp -an --reflink=auto` in Dh_Lib's file restore, `cp -a` in
dh_install, dh_installdocs, dh_installexamples and dh_strip,
`cp --parents -dp` and `cp --parents -a` in dh_install, dh_installdocs and
dh_installexamples), with GNU cp's meaning:

 * `-a` / `--archive` — `-R -P -p`, and files hard-linked to each other in
   the source are hard-linked in the copy, and extended attributes are
   copied (see below).
 * `-d` — `-P`, with hard links kept as for `-a`.
 * `-n` / `--no-clobber` — an existing destination (other than a directory
   being merged into) is left alone, silently and without affecting the exit
   status.
 * `--reflink=auto` — accepted, and files are copied normally: `auto` asks
   for a copy-on-write clone only where one is available, so an ordinary copy
   is always a correct result.  Any other `--reflink` form is refused.
 * `--parents` — the destination of each source is the target directory
   followed by the source's path, and missing directories on that path are
   made from the source's (with `-p`, their owner, mode and times too).  The
   target must be an existing directory.  Unlike GNU, a source ending in `..`
   is refused with a diagnostic and exit status 1: its destination ends in
   `..` too, which names no entry inside the target but whatever directory
   that reaches -- for `..` itself the target's parent -- and GNU cp copies
   onto that.  A source of `.` copies its contents into the target itself.
 * `-l` — each non-directory is hard-linked to its source instead of copied;
   with `-R`, directories are made and the files in them linked.  An existing
   destination is replaced only under `-f` (or `-i` answered yes); one that is
   already the source is left as it is.  gcc-defaults' rules run
   `cp -l debian/substvars.native debian/$p.substvars`.  Unlike GNU, which
   with `-l` follows every symbolic link unless `-P` is given, `-R -l` follows
   only what `-H` or `-L` asks for, as `-R` does without `-l`: a link found
   in the walk is itself given the new name.
 * `-v` / `--verbose` — write `'source' -> 'dest'` to standard output for
   each file copied or linked and each directory made (not one copied into),
   and `source -> dest` unquoted for each directory `--parents` makes, names
   quoted as GNU coreutils quotes them.  sysvinit installs with
   `cp -afv etc/* $(DESTDIR)...`.

#### ACLs under `-p`

POSIX leaves the effect of `-p` on additional and alternate access controls
implementation-defined.  `cp -p` (and `-a`) and `mv` across filesystems give
the copy the source's ACLs in place of the destination's own: access and,
for a directory, default ACL; a source without one leaves the copy without
one, removing what an existing destination had or a new one inherited.  GNU
cp keeps the destination's named entries, masked by the new mode, which
gives them the access the source grants its group class -- more than the
source grants them.  An ACL that cannot be set is diagnosed (cp exits 1, mv
completes with status 0) unless the source had none beyond its mode, and
the copy then keeps a mode granting no more than the source did.

#### Extended attributes under `-a`

`cp -a` and `mv` across filesystems copy each extended attribute of a file,
directory, special file or symbolic link, as GNU does; `-p` alone copies
none.  An attribute already on an existing destination is kept unless the
source has one of the same name.  Not copied: the ACLs (`system.posix_acl_*`,
the NFSv4 and CIFS ones, macOS's), which go with the mode, and what libattr's
`/etc/xattr.conf` skips: XFS's `trusted.SGI_ACL_FILE`, `SGI_ACL_DEFAULT`,
`SGI_CAP_FILE`, `SGI_MAC_FILE` and `SGI_DMI_*`, `xfsroot.*`,
`user.Beagle.*`, `security.evm` and `afs.*`.  `security.selinux` and
`security.capability` are copied (setting either takes privilege).  One the
destination's filesystem cannot hold is lost silently; any other failure is
silent under `cp -a` and diagnosed by `mv`, and neither changes the exit
status.  `cp --parents` gives the directories it makes none, as GNU's does.
On macOS a special file's or symbolic link's are not copied.

#### Existing directories under `-p`

`cp -p` (and `-a`) and `pax -p` give a directory that already existed in the
destination what `-p` asks for -- its mode and owner from the source -- only
where nobody but the user could have created its name: in its parent and in
every directory above it, up to the destination the user named.  Elsewhere it
keeps its mode, owner and times, its contents are still copied, a diagnostic
names it, and the exit status is 1.  Without a mode or owner asked for it gets
its times only, as by default.  GNU cp and libarchive do no such check.

A directory counts as one nobody else can create entries in when it is the
user's, grants no other write permission (sticky or not), grants group write
permission only for the user's private group -- its primary group, named as
the user, listing no other member -- and carries no ACL that lets others
write.  This fails closed, with the diagnostic and exit 1, wherever it cannot
tell:

 * an NFSv4 or CIFS ACL (`system.nfs4_acl`, `system.nfs4_acl_xdr`,
   `system.cifs_acl`), whose entries are not evaluated;
 * group write permission for a group the user shares -- a primary group that
   is not a user-private group, as under a umask of 002 without user private
   groups;
 * an ACL attribute, or the user and group databases, that cannot be read.

On Linux, root working in another user's tree -- extracting into
`/home/alice`, which alice owns -- also trusts that one user's directories, as
the user's own: every directory below root's own must be alice's alone, and
so must every directory above where her tree starts, up to one of root's, and
each directory found there must itself be hers.  Elsewhere, where ACLs are
not read, root trusts only its own directories.

What is not seen: groups granted outside the user and group databases
(`pam_group`, systemd `SupplementaryGroups=`); another account given the
user's primary gid (accounts are not enumerated); a second entry for the same
gid, in a later NSS source or a second `/etc/group` line; a group password;
a macOS ACL; and, in another user's tree, a move of that user's own
directories timed against the lazy check above it.

### cpio

The whole utility is an addition: a compatibility front-end over `pax`
accepting the historic cpio command line.

 * Modes `-o`, `-i`, `-p`; options `-t -v -d -m -u -a -l -L -f -r -A -0 -B -E
   -F -I -O -C -H -R`, plus long spellings and `--quiet` and
   `--no-absolute-filenames`.
 * `-o` defaults to the old binary header and a 512-byte block size.
 * `-d` is a permissive no-op — directories are always created as needed.
 * `-s`, `-S`, `-b`, `-V`, `-R`, `-M` and `-H hpbin` / `-H hpodc` are refused
   with a diagnostic rather than silently ignored.

### crond

The whole daemon is an addition; POSIX specifies `crontab`, `at` and `batch`
but no daemon to run them.  Behavior follows Vixie cron:

 * `@reboot`, `@yearly`, `@annually`, `@monthly`, `@weekly`, `@daily`,
   `@midnight` and `@hourly` schedule shorthands.
 * Step syntax `*/N` and `min-max/N` in any field.
 * `NAME=value` environment assignments inside crontab files.
 * A six-field system crontab at `/etc/crontab` carrying a user-name column.
 * `-f` / `--foreground` — do not fork into the background.  Without it the
   parent returns as soon as the child is forked, so a supervisor that tracks
   the process it started — systemd `Type=simple`, a container — sees the
   daemon exit immediately; and because the fork precedes the PID-file lock,
   a refusal to start is reported to a standard error that is already closed,
   behind an exit status of 0.

### crontab

 * A `-` operand reads the crontab from standard input.
 * `CRON_ALLOW`, `CRON_DENY` — override the `cron.allow` / `cron.deny`
   pathnames.  Honored only when the real and effective user IDs match.

### date

 * `-d STRING` / `--date=STRING` — write the time `STRING` names instead of
   the current time.  `STRING` is what `touch -d` takes (see touch below):
   the ISO 8601 date-time, the RFC 5322 date `date -R` prints, or
   `@SECONDS`, never GNU's free-form dates.  A zone-less time is local time,
   or UTC under `-u`.  With `-d` an operand must be a `+format`.  guile's
   build runs `date -u +FORMAT -d @SECONDS`; perl's passes `--utc -d` its
   changelog date.
 * `-I[FMT]` / `--iso-8601[=FMT]` — write the time in GNU date's ISO 8601
   form, to the precision `FMT` names: `date` (the default, `2026-10-09`),
   `hours` (`2026-10-09T01+00:00`), `minutes`, `seconds`
   (`2026-10-09T01:02:03+00:00`) or `ns` (`...T01:02:03,000000000+00:00`),
   or any unambiguous prefix of one.  As with `-i` in sed, `FMT` is only
   ever attached.  The offset is the local zone's, `+00:00` under `-u`.  A
   second `-I` or a `+format` operand is an error.  binutils' debian/rules
   runs `date -Idate -u -d` its changelog date.

### dd

 * Block-size suffixes `c`, `K`, `m`, `M`, `g` and `G`.  POSIX defines `b`
   (512), `k` (1024) and `x` products.
 * `conv=fsync` — after the last block is written, `fsync` the output, data
   and metadata.  Standard output is synced too, as GNU does, so a pipe or
   `/dev/null` there fails: `fsync failed for 'NAME'`, then the statistics,
   then exit status 1.  Forced by util-linux's test suite
   (`tests/ts/fadvise/drop`).
 * `status=none` — write no statistics; errors are still reported.  Any
   other level is an invalid status level.  Forced by util-linux's test
   suite (`tests/ts/lsfd/error-eperm`).

### df

 * Failure to enumerate a mounted filesystem does not set a non-zero exit
   status.
 * `-T` / `--print-type` — a `Type` column after `Filesystem`, with each file
   system's type from the mount table (Linux) or `f_fstypename` (macOS), in
   every output format including `-P`.  guile's build reads it with
   `df -T PATH | awk 'END{print $2}'`.

### diff

 * `--label` and `--label2` set the header names used in `-c` and `-u` output.
 * `-q` / `--brief` — report only `Files A and B differ` for a differing
   pair, text or binary, with no `diff ...` header under `-r`.  The exit
   status is unchanged.
 * `-N` / `--new-file` — a directory entry missing on one side is compared as
   an empty file dated the Epoch, or as an empty directory.  It applies to
   directory entries only: a missing file operand is still an error, where
   GNU diff compares it as empty too.
 * `-w` / `--ignore-all-space` — ignore all white space, wherever it is in
   the line; `-w` wins over `-b`.
 * `-s` / `--report-identical-files` — report `Files A and B are identical`
   for a pair with no differences under the options given, in any output
   format, with `-q`, and for each such pair a directory comparison meets.
   libzstd's tests run `$DIFF -s tmp1 tmp`.

### echo

 * A first operand of `-n` suppresses the trailing newline.  XSI backslash
   escapes are still processed.  POSIX gives `echo` no options.

### ed

 * `-` — the historic spelling of `-s`, withdrawn from POSIX, as in
   `ed - file`, which GNU patch runs to apply an ed-style diff.  It is the
   option only before the file operand and `--`, and never as the
   option-argument of `-p`.
 * `x` — synonym for `wq`.
 * `z` — scroll.
 * `#` — null command / comment.
 * `&` — repeat the last substitution.

### file

 * `-b` / `--brief` — print the type without the `file: ` prefix.
 * `-e testname` — exclude a default system test.  Only the names
   `apptype`, `ascii`, `encoding`, `cdf`, `compress` and `tar` are accepted.
   `ascii` turns off the text recognition (scripts, `c program text`,
   `fortran program text`); the others name GNU file built-ins this `file`
   does not have, so excluding them changes nothing.
 * **A `#!` script is not reported as `commands text`.**  POSIX has a file of
   shell commands contain `commands text`; we print libmagic's wording
   instead, `<interpreter> script, <encoding> executable`, because Debian's
   binutils build tells scripts from binaries by matching `file` output
   against /script/.  `sh`, `bash`, `perl` and `python` (also after
   `env`) are named — `POSIX shell script, ASCII text executable`,
   `Perl script text executable` — and any other interpreter is
   `a <command> script`, its control characters and invalid UTF-8 bytes
   shown as `\ooo`.  The encoding is `ASCII text` or
   `Unicode text, UTF-8 text`, otherwise left out; libmagic's other
   interpreter names, encodings and line-terminator notes are not
   reproduced.

Both are forced by debhelper: dh_strip and dh_shlibdeps run
`file --brief -e apptype -e ascii -e encoding -e cdf -e compress -e tar -- FILE`.
They read the result through `ELF.*shared`, `ELF.*(executable|shared)`,
`not stripped` and `statically linked`, which is why the built-in ELF test
reports the class, byte order, object type, linking and whether a symbol
table is present.

### find

 * `-ipath pattern` — case-insensitive `-path`.  POSIX.1-2024 added `-iname`
   only.
 * `-newermt date` — true if the file was modified after `date`, read as
   `touch -d` and `date -d` read one (ISO 8601, RFC 5322 as in a Debian
   changelog, `@SECONDS`); not GNU's free-form dates.  The other `-newerXY`
   forms are refused.  binutils' Debian rules run
   `find ... -depth -newermt '$(BUILD_DATE)' -print0`.
 * With no path operand, `.` is searched.  POSIX requires at least one path.
 * `-mindepth n` / `-maxdepth n` — global options, as in GNU find: wherever
   they appear, entries shallower than `n` are walked but not evaluated, and
   entries deeper than `n` are not walked.  The path operand is depth 0.
   Forced by debhelper (`dh_update_autotools_config`, `dh_movelibkdeinit`).
 * `-printf format` — an action writing `format` for each file, with only
   the directives `%p`, `%P` (path without its starting point), `%f` (path
   without its leading directories), `%s`, `%T@` and `%%` and the escapes
   `\n`, `\\` and `\NNN` (octal, so `\0` is NUL).  Any other directive or
   escape is an error.  Forced by debhelper (`dh_autoreconf`,
   `dh_installdeb`, `dh_md5sums`, `dh_installgsettings`); `%f` by Debian's
   perl packaging (`debian/perl.install`, `debian/perl-doc.install`).
 * `-or` / `-and` — spellings of `-o` / `-a`.  Forced by debhelper
   (`dh_install`, `dh_installdocs`, `dh_shlibdeps`, and the `-X` exclusions
   of every dh_* tool).
 * `-not` — a spelling of `!`.  Forced by Debian's perl packaging
   (`debian/perl.install`, `debian/perl-doc.install`).
 * `-true` / `-false` — primaries that are always true / always false.
   Forced by debhelper (`dh_fixperms` joins every walk with `-a -true`,
   `dh_compress` prunes with `-prune -false`).
 * `-size nk` — the size in KiB, rounded up.  No other GNU unit is
   accepted.  Forced by debhelper (`dh_compress` `-size +4k`).
 * `-delete` — an action removing the entry (`rmdir` for a directory); it
   implies `-depth`, never removes a starting point such as `.`, and is an
   error next to `-prune` unless `-depth` is given.  Forced by debhelper
   (`dh_doxygen`, `dh_autotools-dev_restoreconfig`).
 * `-empty` — true for an empty regular file or a directory with no
   entries.  Forced by debhelper (`dh_install`, `dh_installdocs`).
 * `-executable` — true if `access(2)` grants the user execute (search, for
   a directory) permission.  Forced by debhelper (`dh_movelibkdeinit`).
 * `-perm /mode` — true if any of the bits in `mode` is set (or `mode` has
   none).  Forced by debhelper (`dh_shlibdeps` `-perm /111`).
 * `-regex pattern` — true if the pattern matches the whole pathname, in
   the Emacs syntax that is GNU find's default: `\(`, `\)`, `\|` group and
   alternate, `+` and `?` are operators, a bare `(`, `)`, `|`, `{`, `}` is a
   literal, and so is an operator with nothing before it.  Emacs-only
   escapes (`\w`, `\b`, `\<`, backreferences, ...) and `[:class:]`-style
   bracket terms are an error.  No `-regextype` or `-iregex`.  Forced by
   debhelper (`dh_md5sums`, `dh_fixperms`, and the `-X` exclusions of every
   dh_* tool).

### gettext / ngettext

 * `LANGUAGE` — a colon-separated locale priority list, honored ahead of the
   `LC_*` variables.

### grep

 * `-H` / `--with-filename` — precede every output line, and each `-c`
   count, by the file name, even for a single input.
 * `-h` / `--no-filename` — never precede them by the file name, even for
   several inputs.  Of `-H` and `-h`, the last one given wins.  Help is
   therefore `--help` only.
 * `--label=LABEL` — the name standard input goes by in those prefixes and in
   `-l` and `-c` output, in place of `(standard input)`.
 * `-w` / `--word-regexp` (BSD and GNU) — a match counts only with no word
   character (a letter or digit of the locale, or `_`) just before or after
   it; a match that fails is tried shorter from the same start, then later in
   the line, as GNU does.  `-x` wins over it.  binutils runs
   `grep --word-regexp --silent`.
 * `-A NUM` / `--after-context`, `-B NUM` / `--before-context`,
   `-C NUM` / `--context`, and `-NUM` — write NUM lines after, before, or
   around each selected line, marking them with `-` where a selected line has
   `:`, and `--` between groups that do not touch (in a later file too).
   `-A` and `-B` win over `-C`.  Only the output of lines changes, not `-c`,
   `-l` or `-q`.  gzip's zgrep tests run `grep -15`.
 * `-o` / `--only-matching` — write each non-empty matched part of a
   selected line on a line of its own, after the line's prefixes: leftmost
   first, the longest of those starting there, without overlap.  An empty
   match is not written but still selects its line.  `-w`, `-x`, `-i` and
   `-F` decide what matches as they do for lines.  Under `-v` a selected line
   writes nothing; with context options, context lines write nothing except
   under `-v`, where their matches are written marked with `-`, and `--`
   still separates groups, all as in GNU grep.  `-c`, `-l` and `-q` are
   unchanged.  Forced by util-linux's test suite
   (`tests/ts/libmount/debug`).

### head

 * `-number` — the historical form of `-n number`, withdrawn from POSIX in
   Issue 6.  `number` is one or more decimal digits.  It is accepted wherever
   an option may appear (never after `--`), and the last `-n` or `-number`
   given wins.  `-c` has no historical form.

### kill

 * `IOT` is accepted as a name for signal 6.  `kill -l 6` still prints `ABRT`.

### lex

 * `-o file` / `--outfile file` — name the output file.
 * `%option noinput` and `%option nounput`.
 * `<<EOF>>` rules (without start-condition prefixes).
 * A "Output written to <file>" notice on standard error.

### ln

 * `-r` / `--relative` (with `-s` only) — write each link's text as the
   source's path relative to the link's directory.  The source is taken from
   the current directory; both paths are resolved through any symbolic links
   that exist, and need not exist themselves (`realpath -m`).  libselinux
   runs `ln -sf --relative`.
 * A single operand links into the current directory under the operand's last
   component, as `ln SOURCE .` would.  POSIX requires two.  perl's build runs
   `ln -s regen-configure/U`.

### localedef

 * `-v` — verbose.
 * `-u code_set_name` is accepted but only partly acted on. Its specified job
   is to map character and collating-element symbols whose encoding values are
   given as ISO/IEC 10646 position constants into the named codeset, which is
   part of compiling a locale — and this implementation validates a locale
   source rather than compiling one, so there is no such mapping to perform.
   What it does do is check the name against the `-f` charmap's own
   `<code_set_name>` and reject a disagreement, and say so when there is no
   charmap to check against. Honouring it fully waits on locale creation.

### lp

 * `lp` is a thin IPP client.  There is no CUPS integration, no local spool
   directory and no `/dev/lp` device.  A destination given to `-d`, `LPDEST`
   or `PRINTER` is either an `ipp://` URI used verbatim, or a bare printer
   name resolved against the local IPP server as
   `ipp://localhost/printers/<name>`; with none of the three set, the
   destination is `ipp://localhost/printers/default`.
 * `USER`, `LOGNAME` — used for the job originator.
 * `LP_SENDMAIL` — the program invoked for `-m`.

### m4

 * `__file__` — expands to the current input file name.
 * `maketemp` — accepted as an alias for `mkstemp`.  POSIX.1-2024 removed
   `maketemp`; here it creates and closes a file, unlike the historical macro.
 * `eval` accepts `0b` / `0B` binary literals.
 * Diversion numbers greater than 9.

### make

 * `-C dir` / `--directory dir` — change directory before reading the makefile.
 * `-include file` — include, ignoring a missing file.  Plain `include` is
   POSIX.
 * `export` directive.  POSIX mentions it only in the rationale.
 * `:=` assignment.  POSIX defines `=`, `::=`, `:::=`, `!=`, `?=` and `+=`.

### man

POSIX specifies only `-k`.  Every other option is an addition:

 * `-a` / `--all`, `-C` / `--config-file`, `-c` / `--copy`, `-f` / `--whatis`,
   `-h` / `--synopsis`, `-l` / `--local-file`, `-M` (replace the search path),
   `-m` (augment it), `-S` (architecture), `-s` (section), `-w` (list
   pathnames), and `--apropos` as a long form of `-k`.
 * `MANPATH`, `MACHINE`, `COLUMNS`.
 * `MAN_BLESS` — regenerates test snapshots; test tooling only.

### more

 * `-d` / `--test` — hidden test hook.
 * Input is decoded as UTF-8 regardless of `LC_CTYPE`, so text stays readable
   under `LC_ALL=C`.

### msgfmt

The two GNU checks po4a runs on every PO file
(`msgfmt --check-format --check-domain -o /dev/null FILE`):

 * `--check-format` — each `c-format` translation must use the same
   conversions as its original, the check `-c -v` makes among others; a
   mismatch is an error.
 * `--check-domain` — with `-o`, which ignores `domain` directives, each
   domain a file names is reported as an error.

Deviation: GNU's check of plural forms.  POSIX gives `-c -v` no rule for
a `msgid_plural` entry; as in GNU msgfmt, which PO files are written for:

 * every `msgstr[N]` of a `c-format` entry is checked against
   `msgid_plural`, not `msgid`;
 * a plural form that the header's `Plural-Forms` expression gives to
   fewer than five of n = 0..=1000 (the singular of most languages) may
   leave out trailing arguments: `msgstr[0] "one file"` for
   `msgid_plural "%d files"`.  It may not add any or change their types.

 * `--statistics` — print the translated / fuzzy / untranslated counts to
   standard error, as `-v` does, in GNU's wording.  gettext's `po.m4` keeps a
   msgfmt only if `msgfmt --statistics /dev/null` succeeds.

### mv

 * `-v` / `--verbose` — write `renamed 'source' -> 'dest'` to standard
   output for each operand renamed.  A move across filesystems writes, in
   GNU's wording, `created directory 'dest'` for each directory made,
   `copied 'source' -> 'dest'` for each file copied, then `removed 'source'`
   and `removed directory 'source'` for each one removed.  Names are quoted
   as GNU coreutils quotes them.  findutils' build runs
   `mv -v bin/$i bin/$i.findutils`.

### newgrp

 * `SHELL` is consulted for the shell to exec.  POSIX derives it from the user
   database.

### nm

 * Symbol type letters `C` (common) and `r` (read-only data), beyond the
   letters POSIX names.

### od

 * **`-t fL` is converted through `double`.**  POSIX 109155-109157 requires the
   `f` conversion to support `long double`, and it does — `-t fL` selects the
   target's `long double` (the x87 80-bit format on x86-64, IEEE binary128 on
   aarch64, and `double` on Apple's aarch64, matching `c17`).  The *value* is
   then converted to a `double` to be printed, because Rust has neither an
   `f80` nor an `f128`.

   Within `double`'s range and precision the output is exact.  Outside it, a
   long double prints as `0`, `inf`, or with fewer significant digits than
   another `od` would show: `3.3e-4949` is a representable `long double` and
   prints here as `0`.  Printing it faithfully needs arbitrary-precision
   decimal conversion from an 80- or 128-bit significand, which is a larger
   undertaking than the rest of `od` put together.

   `-t fF` and `-t fD` are unaffected, as are the sizes and the notation.

### patch

 * `-f` — force; assume answers rather than prompting.

The rest are the GNU options and behaviors `dpkg-source` relies on to unpack
Debian source packages, with GNU patch's meaning:

 * `-t` — batch: ask nothing; a patch that looks reversed is applied
   reversed, a file that cannot be named is skipped.
 * `-F num` — at most `num` lines of fuzz (default 2, as POSIX describes).
 * `-V never` / `-V simple` — the simple backup method, the only one there is;
   other methods are refused.
 * `-E` — remove a file the patch leaves empty.
 * `-B prefix`, `-z suffix` — backup names `prefix`+FILE, FILE+`suffix`, or
   both; either implies `-b`.  Directories in the prefix are created.
 * `--reject-file=file` — long form of `-r`; `-r -` discards the rejects.
 * With a backup option, a file the patch creates gets an empty backup, the
   placeholder dpkg-source and quilt read as "did not exist".
 * A single hunk inserting into an empty old file (`@@ -0,0 +1,n @@`) creates
   the file when it does not exist, as `diff -N` output requires.
 * Removing a file also removes the directories it leaves empty, up to the
   working directory.

### pax

 * `-z` / `--gzip` — gzip the archive on write.  On read, gzip is detected from
   the archive's magic number and decompressed transparently, with or without
   the option.  Incompatible with `-a`.
 * `-M` / `--multi-volume`, `--tape-length`, `--new-volume-script` — GNU
   tar-style multivolume archives (`archive`, `archive.2`, ...): only the last
   volume has the end-of-archive indicator, and a volume that is missing is
   asked for (script or `/dev/tty`) rather than taken for the end.  Reads GNU
   volume labels and `'M'` continuation headers; writes whole members per
   volume.  Written in ustar format only, and incompatible with `-z`.
 * `-p` on a directory that already existed follows the rule under
   [cp, existing directories under `-p`](#existing-directories-under--p).
 * POSIX.1e ACLs.  `-x pax` always records a file's access ACL, where it
   says more than the mode, and a directory's default ACL, as the
   `SCHILY.acl.access` and `SCHILY.acl.default` records star, bsdtar and GNU
   tar `--acls` write, in star's text form (`user:alice:r--:1000`: the name,
   then the number).  ustar and cpio have no place for one, and get none.
   Reading, a name is looked up first and the number used only where the
   name is unknown, as for a member's owner.
 * `-p p` and `-p e` give each member its archived mode and ACLs, and no ACL
   beyond them, as GNU tar `--acls -p` does: the ACL a new member inherited
   from its directory's default ACL is replaced, and so are the access and
   default ACLs of an existing directory pax is allowed to give its mode --
   the rule of [cp, ACLs under `-p`](#acls-under--p).  An ACL that cannot be
   set, or a record that is not one, is diagnosed and the exit status is 1;
   the member keeps a mode granting no more than the ACL did.  A
   directory's archived default ACL is applied as recorded, not bounded by
   its mode, and from then on governs what is created in that directory,
   whatever the umask, as with GNU tar, bsdtar and star.  Copy mode copies
   the source's ACLs under the same options, as cp `-p` does.
   Without `-p p` no ACL is applied: a member is made by the normal
   file-creation action, so on Linux under a default ACL it takes the ACL
   inherited from that, masked by its archived mode, and the umask plays no
   part.
 * NFSv4-style ACLs (macOS, Linux NFSv4 mounts).  `-x pax` records one that
   says more than the mode as libarchive's `SCHILY.acl.ace` record, in its
   compact text form (`user:alice:raRcs::allow:1000`).  `-p p` and `-p e`
   restore it only where the filesystem holds that kind of ACL; anywhere
   else, as for a member carrying both kinds where only one can be kept, the
   loss is diagnosed, the exit status is 1, and the mode is narrowed.  The
   set-user-ID and set-group-ID bits are withheld, and that reported, from a
   file whose ACL lets anyone but its owner write it; on macOS an `owner@`,
   `group@` or `everyone@` entry the mode does not say is a loss, never
   dropped silently.
 * Extended attributes.  `-x pax` always records each extended attribute of
   a file, directory, special file or symbolic link that cp `-a` copies (see
   [cp, extended attributes under `-a`](#extended-attributes-under--a)) as a
   `SCHILY.xattr.<name>` record holding the value as it stands, as GNU tar
   `--xattrs` writes it, a `%` in the name spelled `%25` and a `=` `%3D`.
   ustar and cpio have no place for one, and get none.  Reading takes those
   records and libarchive's `LIBARCHIVE.xattr.<name>` ones (the name
   %-encoded, the value base64), the latter where both name one attribute;
   one in a global `g` header is not applied, as neither GNU tar nor
   libarchive applies it.  Only `-p e` restores them, in copy mode too: on
   each member pax made or may give its mode, after its owner and before its
   mode and ACLs, and on a symbolic link the link itself.  From an archive
   only `user.` attributes are restored, even by root, as GNU tar `--xattrs`
   restores them; the rest (`security.capability`, `security.selinux`,
   `trusted.*`) are dropped without a word, as GNU drops them.  Copy mode
   reads files, not an archive, and copies every attribute cp `-a` copies, a
   file capability only to a copy given the source's owner.  One the
   destination cannot hold is lost silently; any other failure is a warning
   naming the member and leaves the exit status alone, as with GNU tar.  A
   record that does not decode, a value over 64 KiB, or names together over
   64 KiB, is diagnosed, the member gets none, and the exit status is 1.
 * `-x bcpio`, `-x sv4cpio`, `-x sv4crc` — the historic pax names for the old
   binary cpio header and the SVR4 "newc" headers without and with a data
   checksum.  POSIX names only `cpio` (odc), `pax` and `ustar`.  All three are
   readable and writable, and the cpio reader auto-detects any of them.

### pr

 * `-N number` / `--first-line-number number` — start line counting at
   `number`.
 * `--prettify-headers`.

### printf

 * The `%a`, `%A`, `%e`, `%E`, `%f`, `%F`, `%g` and `%G` conversions.

### ps

 * procps' dashless BSD options `a`, `u` and `x`, in one word or several
   (`ps aux`, `ps ax`, `ps u`), when the first argument is such a word; it
   cannot be mixed with dash options.  `a` lists every user's processes, not
   only the invoker's; `x` lists processes without a controlling terminal
   too; `u` selects procps' user format (`USER PID %CPU %MEM VSZ RSS TTY
   STAT START TIME COMMAND`), otherwise the format is `PID TTY STAT TIME
   COMMAND`.  Columns, `STAT` flags, `%CPU` (CPU time over lifetime) and
   `%MEM` follow procps; a control character in the command is shown as `?`.
   binutils' `debian/rules` runs `ps aux`.
 * procps' `--no-headers` (no header line), the `-o stat` field (the `STAT`
   column above), and process IDs given as operands, which select as `-p`
   does.  util-linux's tests wait with
   `until [[ $(ps --no-headers -ostat PID) =~ S.* ]]`.

### prs

 * The `:KV:` dataspec keyword, removed from POSIX by Austin Group Defect 1452.

### readlink

 * `-f` — canonicalize the whole path, resolving every component.  POSIX
   defines only `-n`.
 * `-v` — accepted, no effect.

### realpath

 * `-q` / `--quiet` — suppress error messages.  POSIX defines only `-E` and
   `-e`.
 * More than one `file` operand.  The SYNOPSIS allows exactly one.
 * With no operand, the current working directory is printed.
 * `-s` / `--no-symlinks` — make the path absolute and remove `.` and `..`
   by name, without following symbolic links.  As in GNU, a name followed by
   more of the path must be a directory, and with `-e` the last name must
   exist.  Unlike GNU, a missing directory before the last name is an error,
   as it is without `-s`.  perl's `Configure` runs `realpath --no-symlinks`.

### rm

Deviation: `rm -r link/`, where `link` is a symbolic link to a directory,
is refused ("Not a directory") and removes nothing.  POSIX resolves `link/`
to the directory, so `rm` would remove everything in it and then fail to
remove the directory by that name.  Refusing keeps a recursive removal from
being redirected through a directory operand swapped for a symbolic link.
`find link/ -delete` is refused the same way.

### rmdir

 * `--ignore-fail-on-non-empty` — a directory that cannot be removed only
   because it is not empty is kept silently and does not affect the exit
   status; with `-p`, the walk up the parents stops there.  As in GNU, a
   permission, read-only or busy error on a directory that holds an entry
   counts as "not empty".  debhelper's `dh_strip` runs it.

### sccs

 * `-p` takes its historic BSD meaning (the SCCS subdirectory name).
 * `sccs create` creates the `SCCS/` directory if it is missing.

### sed

 * The `I` command — a non-POSIX variant of `l`.
 * `-r` / `--regexp-extended` — GNU synonyms for `-E`.
 * `-i[SUFFIX]` / `--in-place[=SUFFIX]` — edit each file in place, as GNU
   sed.  The suffix is only ever attached (`-i.bak`; `-ie` is a suffix of
   `e`), and with one the original is kept under its name plus the suffix,
   or under the suffix with each `*` replaced by the name.  Each file is a
   stream of its own: line numbers restart, `$` is its last line, the hold
   space starts empty and a range left open by the file before is closed, so
   it must select its first line again.  All output, `=` and `i` included,
   goes into the file; `q` ends the run once its file is written.  The new
   version is created exclusively beside the original, given its owner (when
   root) or group, and mode, and renamed over the name, so a symbolic link
   operand is replaced by a regular file, not written through.  Unlike GNU
   sed, a FIFO is refused rather than read, and a suffix that names another
   directory is refused.
 * `-s` / `--separate` — each file is a stream of its own, as under `-i`, but
   the output goes to standard output: line numbers restart, `$` is each
   file's last line, the hold space starts empty and no range is open.  A
   file that cannot be read is reported and skipped; `q` ends the run.
 * One-line `a`, `i` and `c`, as GNU sed: blanks after the letter are
   skipped, and text on the letter's own line (`a text`, `$i #define X 1`,
   `a\text`) runs to the end of that line, `;`, `}` and `#` included.  In it
   a `\` before the <newline> continues the text on the next line, `\n`,
   `\t`, `\r`, `\a`, `\f` and `\v` are controls, and a `\` before any other
   character is removed.  The POSIX `a\` <newline> form keeps POSIX's rule
   (GNU applies the controls there too), and an empty text is still an
   error where GNU appends nothing.  perl's and binutils' Debian builds use
   it.
 * In an RE, outside a bracket expression, `\t`, `\r`, `\a`, `\f` and `\v`
   match those controls, as `\n` matches a <newline>; POSIX leaves `\c`
   there unspecified.  Inside a bracket expression a `\` stays an ordinary
   character (but for `\n`), as POSIX requires.  util-linux's ipcs test
   cuts at a tab with `s/\t.*//`.
 * `PROJECT_NAME` — selects the gettext text domain.

### sh

 * `PS1`, `PS2` and `PS4` undergo full expansion.  POSIX specifies parameter
   expansion only.
 * `PS1` defaults to `\$ `, using the bash-style prompt escape rather than the
   literal `$ ` POSIX specifies.
 * `$(( x ))` recursively evaluates the *value* of `x` as an arithmetic
   expression, matching bash.  With `x=1+2` the result is `3`.
 * `break` and `continue` outside any loop are non-fatal no-ops, matching dash
   and bash, rather than the shell-aborting special-builtin error POSIX
   requires.

### sleep

 * The `time` operand may have a fraction — `0.01`, `.5`, `1.` — as GNU and
   BSD sleep allow; POSIX requires a decimal integer.  No sign, exponent,
   unit suffix or `inf`.

### sort

 * `-z` / `--zero-terminated` — lines end with NUL, not newline, in the input
   and the output (and in a `-c` disorder diagnostic); a newline is then an
   ordinary character, which separates fields as a blank does, as in GNU
   sort.  binutils runs
   `find ... -print0 | LC_ALL=C sort -z | tar --null -T -`.
 * `-V` / `--version-sort`, and the `V` key modifier — compare keys as
   version strings, in GNU sort's (gnulib `filevercmp`) order: runs of
   digits by value, `~` before everything (even the end of the key),
   letters before other bytes, trailing file suffixes such as `.tar.gz`
   compared last, and `.`, `..` and other names beginning with `.` first.
   Combined with `-n` it is refused.  `-V` is therefore not a spelling of
   `--version`.  util-linux runs `sort --check --version-sort`.

### split

 * A `g` suffix on the `-b` argument.  POSIX defines `k` and `m`.

### tail

 * The historical forms withdrawn from POSIX in Issue 6, as the first
   argument only: `-number` and `+number` mean `-n -number` and
   `-n +number`; `-numberc` and `+numberc` mean `-c -number` and
   `-c +number`.  The historical `b`, `l` and `f` suffixes are not accepted.
 * `-v` / `--verbose` — write GNU's `==> NAME <==` header (`standard input`
   for standard input) before the output, once the file is open.  Unlike GNU
   tail, the header is written for `-n 0` too.

### talk

 * `--local` — use the local Unix-domain `talkd` socket instead of the network
   ntalk protocol.  This materially changes the transport.
 * `TALKD_SOCKET` — path to that socket.

### talkd

The whole daemon is an addition.

 * It serves the BSD ntalk control protocol over a Unix-domain datagram socket
   (default `/var/run/talkd.sock`), not UDP port 518.  It is therefore *not*
   interoperable with stock or remote `talk` clients.
 * `-s` / `--socket`, `-f` / `--foreground`, `--invite-timeout`.

### tar

The whole utility is an addition: a compatibility front-end over `pax`
accepting the historic tar command line, deliberately a smaller subset than
GNU tar.

 * Modes `-c -x -t -r -u`; options `-f -v -z -C -b -p -m -h -k -O -T -X -P`,
   `--format=`, `--exclude=`, `--exclude-from=`, `--strip-components=`,
   `--null`, `--no-recursion`, `--same-owner` / `--no-same-owner`, plus the
   long spellings and the old-style bundled first operand.
 * `-j`, `-J` and `-Z` are refused; `-P` / `--absolute-names` is refused on
   extract; only a single `-C` is accepted; `-A`, `-w`, `-S` and `-W` are
   refused.  All refusals are diagnosed rather than ignored.

### test / [

 * `-ef`, `-nt` and `-ot` binary primaries.
 * The `-a` and `-o` operators and `(` / `)` grouping, which POSIX.1-2024
   removed.

### touch

 * `-d` also takes the RFC 5322 date `date -R` prints, such as
   `Fri, 17 Jul 2026 19:05:00 +0200`.  Debian's base-files sets its files'
   times by passing `touch -d` the date `dpkg-parsechangelog -SDate` prints,
   which is this form, so a Debian build cannot do without it.  Only that one form is
   added, not GNU's free-form date parser: `[Day, ]D Mon YYYY HH:MM[:SS]`
   and a numeric `+hhmm` / `-hhmm` zone, single spaces, the English
   abbreviations spelled as `date -R` spells them, and a day name that
   matches the date.  In this form, zone names such as `GMT` are refused.
 * `-d` also takes the POSIX date-time followed by a single space and the
   word `UTC` or `GMT`, meaning exactly what a trailing `Z` means:
   `1999-08-26 12:06:20 UTC`.  base-files' debian/timestamps sets its
   license files' times in this form.  Only those two words, in upper
   case, after one space; other zone words, `UTC` combined with `Z`, and
   any other spacing are refused.
 * `-d` also takes the POSIX date-time without its seconds
   (`1990-06-22T12:00Z`), and `@SECONDS`, a signed whole number of seconds
   since the Epoch.  `--date` is a long form of `-d`.  perl's build runs
   `touch --date=@SECONDS`.
 * `-h` / `--no-dereference` — change a symbolic link's own times, never
   those of the file it names, as GNU touch does; with `-r`, a link gives its
   own times too.  Nothing is created: a file that does not exist is an
   error, passed over in silence under `-c`.  `--help` has only its long
   form.  binutils' debian/rules runs `touch --no-dereference --date=...`.

### tr

 * **Characters are UTF-8, in every locale.**  A multi-byte character can be a
   set member, a range endpoint, or a translation target: `tr 'α-γ' 'A-C'`
   works, where a byte-oriented `tr` produces mojibake.

   This is a deliberate deviation, not an oversight.  POSIX has `LC_CTYPE`
   decide how bytes become characters, which in the C locale makes every byte
   its own character.  But `tr`'s operands reach it as text, so `string1` and
   `string2` are UTF-8 however `LC_CTYPE` is set; reading the *input* by
   `LC_CTYPE` instead would make the two disagree, and under the default C
   locale a set holding `é` would stop matching the `é` in its input —
   `tr -d 'ᛆᚠ'` would delete nothing.  One model applied to both sides is worth
   more here than a literal reading that only agrees with itself.

   Character *class* membership and case conversion do follow `LC_CTYPE`, since
   which characters are letters is a locale question rather than an encoding
   one.

 * **`-c` and `-C` differ**, as POSIX 118153-118158 specifies and as most
   implementations do not: `-c` complements the set of *values*, so a
   non-member multi-byte character is acted on once per byte, while `-C`
   complements the set of *characters* and acts on it once.

 * `[=c=]` accepts only a single-byte character.  POSIX does not require more,
   and its members come from `LC_COLLATE` by way of the system's regular
   expression engine, so in a locale where the class is larger than the
   character itself it will be larger here too.

 * Adjacent octal escapes are separate bytes, so `\303\251` is two members and
   not `é`.  The spec contradicts itself: 118095-118096 says a multi-byte
   character "require[s] multiple, concatenated escape sequences", while the
   RATIONALE at 118258-118263 records that this was found ambiguous and settles
   on octal escapes naming single byte values.  The RATIONALE's reading is the
   one implemented, and it is what other implementations do.

### uname

 * `-i` / `--hardware-platform` — write the machine hardware name (`-m`'s),
   as Debian's coreutils does, after the `-m` field.  `-a` stays POSIX's
   `-mnrsv` and does not include it.  gprofng's testsuite runs `uname -i`.

### uucp / uux / uustat

 * Transport is SSH.  The legacy UUCP protocol, configuration files and
   handshake are intentionally absent.
 * `UUCP_SPOOL` — override the spool directory.
 * A remote `~user` path is not expanded: remote paths are single-quoted before
   being handed to SSH, which also defeats remote globbing.
 * Multi-hop `a!b!path` routes are diagnosed and refused.

No `uucp`, `uux` or `uustat` *options* are extensions.

### vi / ex

 * `+command` — an initial ex command.
 * `-s` and `-v` are accepted by `vi`; POSIX defines them for `ex` only.
 * `set expandtab` / `et`, and `set backup`.
 * The ex commands `:pwd`, `:prev` / `:previous`, `:red` / `:redo`, and
   `:h` / `:help`.
 * A tag stack.  `:ta` / `:tag` and `^]` record the position they left;
   `:po` / `:pop` and `^T` in command mode return to it, and `:tags` lists what
   is outstanding.  POSIX specifies `:tag` and `^]` but nothing that goes back,
   and the `tags` it defines is the `:set tags=` edit option naming the files
   `:tag` searches, not a command.  POSIX gives `^T` a meaning in text input
   mode only, where it shifts the autoindent; that is unaffected.
 * `COLUMNS`, `LINES` and `TMPDIR` are consulted.  POSIX names `EXINIT`,
   `HOME`, `SHELL` and `TERM`.

### who

 * `--userproc` — hidden internal selection flag.

### xargs

 * `-P maxprocs` — run up to *maxprocs* invocations of the utility at once;
   `0` means no limit.  util-linux's test runner passes it.

### xgettext

 * Rust (`.rs`) source files are parsed for translatable strings in addition to
   C.

### yacc

 * `--strict` — disable an internal table-packing optimization.
 * `%expect N` and `%expect-rr N` conflict-count declarations, from Bison.
 * `yynerrs` is emitted as an external symbol.  It is not among the names POSIX
   says `-p` renames.
