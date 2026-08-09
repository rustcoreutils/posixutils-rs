# Non-POSIX Extensions

posixutils-rs takes POSIX.1-2024 (IEEE Std 1003.1-2024) as its baseline
specification, then adds only the non-POSIX behavior that users cannot live
without.  It is *not* a goal to be compatible with GNU utilities.

This document is the complete inventory of what we ship beyond the standard:
extra utilities, extra options, language and syntax extensions, extra
environment variables, extra file formats, and the notable choices we have made
where POSIX leaves behavior unspecified.

Anything not listed here is intended to be plain POSIX.  Per-utility
conformance detail lives in the `audit.md` file of each crate
(`text/audit.md`, `tree/audit.md`, and so on).

## Utilities beyond POSIX

Four binaries have no POSIX.1-2024 specification at all:

 * **tar** - `pax` compatibility front-end forcing the ustar format (Archive)
 * **cpio** - `pax` compatibility front-end forcing the cpio format (Archive)
 * **crond** - cron daemon executing `crontab`, `at` and `batch` jobs
 * **talkd** - local-only `talk` daemon (Unix-domain socket, not UDP port 518)

`tar` and `cpio` are installed as symlinks to `pax`, which selects its
command-line parser from `argv[0]`.  Three further symlinks exist for
convenience, but name POSIX-specified utilities and are not extensions:
`ex` -> `vi`, and `zcat` / `uncompress` -> `compress`.

POSIX utilities that have no binary of their own — `cd`, `read`, `umask`,
`getopts`, `wait` and the rest — are shell built-ins provided by `sh`.

## Project-wide conventions

Two extensions apply across nearly the whole suite and are not repeated per
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

### asa

 * A `<hyphen>` in the carriage-control column triples the line spacing.  POSIX
   leaves unrecognized control characters unspecified.

### at

 * `AT_ALLOW`, `AT_DENY` — override the `at.allow` / `at.deny` pathnames.
 * `AT_JOB_DIR` — override the job spool directory.

   All three are honored only when the real and effective user IDs match.

### awk

 * C-style floating-point literal suffixes `f`, `l`, `F`, `L` are accepted in
   awk source.
 * `RS` accepts a multi-character string or a regular expression.  POSIX
   defines only the first character (and `""` for paragraph mode).
 * `FS=""` and `split(s, a, "")` split into individual characters.
 * `cmd | getline` advances `NR`.
 * Division or modulo by zero yields an IEEE `inf` / `nan` rather than an
   error.

### bc

 * Interactive line editing and command history.
 * Numbers are 128-bit fixed-width integers, not arbitrary precision;
   overflow is an error.  `BC_SCALE_MAX` and `BC_BASE_MAX` report `2147483647`.

### c17

The compiler accepts a large GCC/Clang-compatible surface so that real-world
source trees build unmodified.  None of it changes the meaning of
strictly-conforming C.

Options beyond the POSIX set (`-B -c -D -E -G -g -I -L -l -O -o -R -s -U`):

 * `-S` — emit assembly.
 * `-v` / `--verbose`, `--stats`, `--pedantic`, `-W <warning>`.
 * `--nostdinc`, `--nobuiltininc`, `--fno-builtin`, `--fno-unwind-tables`.
 * `--target <triple>`, `--shared`, `--rtlib`, `--print-targets`.
 * `--trigraphs` — enable trigraph replacement (off by default, since it
   would alter string literals).
 * `--dump-tokens`, `--dump-ast`, `--dump-ir`, `--dump-ir-func` — developer
   diagnostics.
 * A GCC-compatibility argument rewriter accepts and maps `-std=`, `-f*`,
   `-m*`, `-fPIC`/`-fpic`/`-fPIE`/`-fpie`/`-fno-pie`, `-pie`/`-no-pie`,
   `-shared`, `-Wl,`, `-Xlinker`, `-pthread`, `-rdynamic`, `-pipe`, `-p`/`-pg`,
   `-fsanitize*` and `-ffreestanding`/`-fhosted`.
 * A bare `-` operand is accepted as a pathname.  POSIX says standard input is
   "Not used".

Language and preprocessor extensions:

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
 * Clang nullability qualifiers `_Nonnull`, `_Nullable`, `_Null_unspecified`
   and their `__` spellings.
 * Roughly 90 `__builtin_*` and `__c11_atomic_*` intrinsics, including the
   `__builtin___*_chk` FORTIFY family.
 * `__FUNCTION__`, `__PRETTY_FUNCTION__`.
 * `#include_next`, `#warning`, `#pragma once`.
 * `__has_attribute`, `__has_builtin`, `__has_feature`, `__has_extension`,
   `__has_include`, `__has_include_next`.
 * Named variadic macro parameters (`#define F(args...)`), an empty variadic
   tail, and the `, ## __VA_ARGS__` comma-swallow.
 * Empty `enum { }` (diagnosed under `--pedantic`).
 * Digraphs, and the C23 one-argument `_Static_assert`.
 * `__GNUC__`, `__GNUC_MINOR__`, `__GNUC_PATCHLEVEL__`, `__VERSION__` and
   `__GNUC_STDC_INLINE__` are predefined.
 * On Linux, `_GNU_SOURCE`, `_XOPEN_SOURCE=800` and `_XOPEN_SOURCE_EXTENDED`
   are predefined unconditionally.  POSIX only encourages restricting symbol
   visibility.

Deliberate limits, for completeness: `-std=` is accepted and discarded —
`__STDC_VERSION__` is always `201112L` — and `__STDC_IEC_559_COMPLEX__` is left
undefined rather than falsely claiming Annex G.

### chmod

 * A symbolic or fewer-than-five-digit octal mode preserves the set-user-ID and
   set-group-ID bits on directories.  POSIX leaves set-ID handling on
   non-regular files implementation-defined.

### chown

 * An `owner:` operand with an empty group resolves the group to the owner's
   login group.

### compress / uncompress / zcat

 * A `-` operand names standard input.  The `compress` spec never mentions it.
 * `uncompress` looks for `file`, then `file.Z`, then `file.gz`.  POSIX permits
   additional suffixes.

### cp

 * `-r` is accepted as a short alias for `-R`.  POSIX.1-2024 removed `-r`.
 * A `source/.` operand copies the contents of `source` rather than the
   directory itself.
 * `-H` and `-L` require `-R`.
 * Destination special files are created mode `0644`.

### cpio

The whole utility is an extension: a compatibility front-end over `pax`
accepting the historic cpio command line.

 * Modes `-o`, `-i`, `-p`; options `-t -v -d -m -u -a -l -L -f -r -A -0 -B -E
   -F -I -O -C -H -R`, plus long spellings and `--quiet` and
   `--no-absolute-filenames`.
 * `-o` defaults to the old binary header and a 512-byte block size.
 * `-d` is a permissive no-op — directories are always created as needed.
 * `-s`, `-S`, `-b`, `-V`, `-R`, `-M` and `-H hpbin` / `-H hpodc` are refused
   with a diagnostic rather than silently ignored.

### crond

The whole daemon is an extension; POSIX specifies `crontab`, `at` and `batch`
but no daemon to run them.  Behavior follows Vixie cron:

 * `@reboot`, `@yearly`, `@annually`, `@monthly`, `@weekly`, `@daily`,
   `@midnight` and `@hourly` schedule shorthands.
 * Step syntax `*/N` and `min-max/N` in any field.
 * `NAME=value` environment assignments inside crontab files.
 * A six-field system crontab at `/etc/crontab` carrying a user-name column.

Note that day-of-week `7` for Sunday and the `sun` / `mon` name forms are
deliberately *not* accepted.

### crontab

 * A `-` operand reads the crontab from standard input.
 * `CRON_ALLOW`, `CRON_DENY` — override the `cron.allow` / `cron.deny`
   pathnames.  Honored only when the real and effective user IDs match.

### dd

 * Block-size suffixes `c`, `K`, `m`, `M`, `g` and `G`.  POSIX defines `b`
   (512), `k` (1024) and `x` products.

### df

 * Failure to enumerate a mounted filesystem does not set a non-zero exit
   status, matching GNU coreutils.

### diff

 * `--label` and `--label2` set the header names used in `-c` and `-u` output.

### echo

 * A first operand of `-n` suppresses the trailing newline *and* XSI backslash
   escapes are still processed — the BSD and System V behaviors combined.
   POSIX makes a leading `-n` implementation-defined.

### ed

 * `x` — synonym for `wq`.
 * `z` — scroll.
 * `#` — null command / comment.
 * `&` — repeat the last substitution.

### find

 * `-ipath pattern` — case-insensitive `-path`.  POSIX.1-2024 added `-iname`
   only.
 * With no path operand, `.` is searched.  POSIX requires at least one path.

### gettext / ngettext

 * `LANGUAGE` — a colon-separated locale priority list, honored ahead of the
   `LC_*` variables.  A GNU gettext extension.

### kill

 * `IOT` is accepted as a name for signal 6.  `kill -l 6` still prints `ABRT`.

### lex

 * `-o file` / `--outfile file` — name the output file.
 * `%option noinput` and `%option nounput`.
 * `<<EOF>>` rules (without start-condition prefixes).
 * A "Output written to <file>" notice on standard error.

### localedef

 * `-v` — verbose.
 * `-u code_set_name` — accepted but has no effect.

### lp

 * The destination is an IPP URI.  There is no CUPS integration, no local
   spool directory, no `/dev/lp` device and no default-printer concept; POSIX
   leaves the destination format, queuing and device unspecified.
 * `ipps://` (IPP over TLS) is not supported, to avoid a TLS dependency.
 * `USER`, `LOGNAME` — used for the job originator.
 * `LP_SENDMAIL` — the program invoked for `-m`.

### m4

 * `__file__` — expands to the current input file name.
 * `maketemp` — accepted as an alias for `mkstemp`.  POSIX.1-2024 removed
   `maketemp`; here it creates and closes a file, unlike the historical macro.
 * `eval` accepts `0b` / `0B` binary literals.
 * Diversion numbers greater than 9 are supported.  POSIX calls these
   implementation-defined.
 * Diversions are buffered in memory rather than in temporary files.
 * Errors exit non-zero.  GNU m4 exits 0.

### make

 * `-C dir` / `--directory dir` — change directory before reading the makefile.
 * `-include file` — include, ignoring a missing file.  Plain `include` is
   POSIX.
 * `export` directive.  POSIX mentions it only in the rationale.
 * `:=` assignment.  POSIX defines `=`, `::=`, `:::=`, `!=`, `?=` and `+=`.
 * `-p` output is a structured debug dump; POSIX leaves the format
   unspecified.

`PROJECTDIR` and runtime `.SCCS_GET` retrieval — both optional XSI features —
are not implemented.

### man

POSIX specifies only `-k`.  Every other option is an extension:

 * `-a` / `--all`, `-C` / `--config-file`, `-c` / `--copy`, `-f` / `--whatis`,
   `-h` / `--synopsis`, `-l` / `--local-file`, `-M` (replace the search path),
   `-m` (augment it), `-S` (architecture), `-s` (section), `-w` (list
   pathnames), and `--apropos` as a long form of `-k`.
 * `MANPATH`, `MACHINE`, `COLUMNS` — the rationale explicitly excludes
   `MANPATH` from POSIX.
 * `MAN_BLESS` — regenerates test snapshots; test tooling only.

roff programmability (`.if`, `.ie`, `.de`, `.nr`, `.ds`, `.so`) and tbl
spanning / box drawing are deliberately out of scope.

### more

 * `-d` / `--test` — hidden test hook.
 * Input is decoded as UTF-8 regardless of `LC_CTYPE`, so text stays readable
   under `LC_ALL=C`.
 * A literal `<backslash>` is not escaped in the non-printable display; POSIX
   scopes escaping to non-printable characters.
 * `<tab>` expands to 8-column stops.

### newgrp

 * `SHELL` is consulted for the shell to exec.  POSIX derives it from the user
   database.
 * A `-` operand reports "no such group" and then execs the shell anyway.
   POSIX leaves `-` unspecified here.

### nm

 * Symbol type letters `C` (common) and `r` (read-only data), beyond the
   letters POSIX names.

### od

 * `-c` and `-t c` render each byte as a C escape rather than attempting
   multibyte character rendering.
 * `-t f` uses Rust's default floating-point field width.  POSIX leaves the
   width implementation-defined.

### patch

 * `-f` — force; assume answers rather than prompting.

### pax

 * `-z` / `--gzip` — gzip the archive on write.  On read, gzip is detected from
   the archive's magic number and decompressed transparently, with or without
   the option.  Incompatible with `-a`.
 * `-M` / `--multi-volume`, `--tape-length`, `--new-volume-script` — GNU
   tar-style multivolume archives, using the GNU `'M'` continuation header.
   ustar format only, and incompatible with `-z`.
 * `-x bcpio`, `-x sv4cpio`, `-x sv4crc` — the historic pax names for the old
   binary cpio header and the SVR4 "newc" headers without and with a data
   checksum.  POSIX names only `cpio` (odc), `pax` and `ustar`.  All three are
   readable and writable, and the cpio reader auto-detects any of them.
 * The cpio `TRAILER!!!` record carries `c_mode=100644`; GNU cpio writes
   `000000`.  Readers key off the name, not the mode.

### pr

 * `-N number` / `--first-line-number number` — start line counting at
   `number`.
 * `--prettify-headers`.
 * `-i` is not implicitly assumed for multi-column output, matching GNU `pr`
   byte for byte.

### printf

 * The `%a`, `%A`, `%e`, `%E`, `%f`, `%F`, `%g` and `%G` conversions.  POSIX
   encourages but does not require them.

### prs

 * The `:KV:` dataspec keyword, removed from POSIX by Austin Group Defect 1452.

### readlink

 * `-f` — canonicalize the whole path, resolving every component.  POSIX
   defines only `-n`.  Symlink chains are bounded at 40 links.
 * `-v` — accepted, no effect.

### realpath

 * `-q` / `--quiet` — suppress error messages.  POSIX defines only `-E` and
   `-e`.
 * More than one `file` operand.  The SYNOPSIS allows exactly one.
 * With no operand, the current working directory is printed.

### sccs

 * `-p` takes its historic BSD meaning (the SCCS subdirectory name).
 * `sccs create` creates the `SCCS/` directory if it is missing.

### sed

 * The `I` command — a non-POSIX variant of `l`, sharing the same octal escape
   renderer.
 * `PROJECT_NAME` — selects the gettext text domain.

Note that address `0` is rejected: the GNU `0,/re/` form is not implemented.

### sh

 * `PS1`, `PS2` and `PS4` undergo full expansion.  POSIX specifies parameter
   expansion only.
 * `PS1` defaults to `\$ `, using the bash-style prompt escape rather than a
   literal `$ `.
 * `$(( x ))` recursively evaluates the *value* of `x` as an arithmetic
   expression, matching bash.  With `x=1+2` the result is `3`.
 * `break` and `continue` outside any loop are non-fatal no-ops, matching dash
   and bash, rather than a shell-aborting special-builtin error.
 * `cd ""` is an error — stricter than bash and dash (Austin Group Defect
   1047).
 * `set -u` on an unset parameter exits 1, not bash's 127.

### sort

 * `-d`, `-i` and `-n` may be combined freely.

### split

 * A `g` suffix on the `-b` argument.  POSIX defines `k` and `m`.

### tabs

 * `--rep-0`, `--a2`, `--c2` and `--c3` are the spellings used for the POSIX
   `-0`, `-a2`, `-c2` and `-c3` options.

### talk

 * `--local` — use the local Unix-domain `talkd` socket instead of the network
   ntalk protocol.  This materially changes the transport.
 * `TALKD_SOCKET` — path to that socket.

### talkd

The whole daemon is an extension.  Notable properties:

 * It serves the BSD ntalk control protocol over a Unix-domain datagram socket
   (default `/var/run/talkd.sock`), not UDP port 518.  It is therefore *not*
   interoperable with stock or remote `talk` clients.
 * `-s` / `--socket`, `-f` / `--foreground`, `--invite-timeout`.
 * It performs no privileged operation and is not meant to run as root, so it
   contains no `setuid` / `setgid` privilege drop.
 * It honors the recipient's `mesg n` setting even when running as root.
 * Error replies are rate-limited globally (burst 8, 8/s).

### tar

The whole utility is an extension: a compatibility front-end over `pax`
accepting the historic tar command line, deliberately a smaller subset than
GNU tar.

 * Modes `-c -x -t -r -u`; options `-f -v -z -C -b -p -m -h -k -O -T -X -P`,
   `--format=`, `--exclude=`, `--exclude-from=`, `--strip-components=`,
   `--null`, `--no-recursion`, `--same-owner` / `--no-same-owner`, plus the
   long spellings and the old-style bundled first operand.
 * `-t -v` uses the `ls -l` listing layout.
 * `-j`, `-J` and `-Z` are refused; `-P` / `--absolute-names` is refused on
   extract; only a single `-C` is accepted; `-A`, `-w`, `-S` and `-W` are
   refused.  All refusals are diagnosed rather than ignored.

### test / [

 * `-ef`, `-nt` and `-ot` binary primaries.
 * The `-a` and `-o` operators and `(` / `)` grouping, which POSIX.1-2024
   removed.  These work uniformly across the three-, four- and
   more-than-four-argument forms; POSIX leaves behavior above four arguments
   unspecified.

### uniq

 * `-c` combined with `-d` or `-u` is a hard error rather than an unspecified
   combination.

### uucp / uux / uustat

 * Transport is SSH.  The legacy UUCP protocol, configuration files and
   handshake are intentionally absent.
 * `UUCP_SPOOL` — override the spool directory.
 * A remote `~user` path is not expanded: remote paths are single-quoted before
   being handed to SSH, which also defeats remote globbing.
 * Multi-hop `a!b!path` routes are diagnosed and refused; store-and-forward
   routing is obsolete.

No `uucp`, `uux` or `uustat` *options* are extensions.

### vi / ex

 * `+command` — an initial ex command.  Historical, not in the SYNOPSIS.
 * `-s` and `-v` are accepted by `vi`; POSIX defines them for `ex` only.
 * `set expandtab` / `et`, and `set backup`.
 * The ex commands `:pwd`, `:prev` / `:previous`, `:red` / `:redo`,
   `:po` / `:pop`, `:tags`, and `:h` / `:help`.
 * `COLUMNS`, `LINES` and `TMPDIR` are consulted.  POSIX names `EXINIT`,
   `HOME`, `SHELL` and `TERM`.
 * `TERM` is read but no terminfo lookup is performed; the editor drives the
   terminal itself.

### who

 * `--userproc` — hidden internal selection flag.

### xgettext

 * Rust (`.rs`) source files are parsed for translatable strings in addition to
   C.  The spec covers C-language source only.  This is what builds this
   project's own message catalogs.

### yacc

 * `--strict` — disable an internal table-packing optimization.
 * `%expect N` and `%expect-rr N` conflict-count declarations, from Bison.
 * `yynerrs` is emitted as an external symbol.  It is not among the names POSIX
   says `-p` renames.
