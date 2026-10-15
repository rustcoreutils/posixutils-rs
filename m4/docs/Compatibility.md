# Compatibility

This document outlines notes about compatibility of this software with other software that uses m4.

## [`autoconf`](https://www.gnu.org/software/autoconf/)

autoconf, automake and libtoolize drive m4 through GNU extensions, and this
m4 has the subset they use, listed in the m4 section of `NONPOSIX.md`:
autom4te's `--gnu --nesting-limit --include --debug=aflq --fatal-warning
--debugfile --trace --reload-state --undefine` command line and its
`m4trace:` output, libtoolize's `-daq --prefix --trace`, and the built-ins
m4sugar is written in.  autom4te's run on a package's `configure.ac`, from
autoconf's sources or from the frozen files Debian ships, gives the same
output and traces as GNU m4 1.4.19.

Where GNU m4 1.4 departs from POSIX in a way autotools output depends on,
this m4 does as GNU m4 does by default, and as POSIX says when
`POSIXLY_CORRECT` is set in the environment (to any value):

* `len`, `index`, `substr` and `translit` count bytes, as GNU m4 does in
  every locale; under `POSIXLY_CORRECT` they count the characters of
  `LC_CTYPE`.  m4sugar measures and cuts text with `m4_len` and
  `m4_substr`: tar's testsuite, which autotest generates from `.at` files
  containing UTF-8 text, came out with a test group that no shell could
  parse when m4 counted characters under `LANG=C.UTF-8`.
* `m4wrap` text runs last in, first out, as in GNU m4 1.4; under
  `POSIXLY_CORRECT`, in the order the `m4wrap` calls were made.  GNU
  autoconf's m4sugar calls `m4wrap` once and orders its own hooks, but
  autoconf-dickey (the autoconf 2.52 fork ncurses' `configure` is generated
  with) does not: its `m4_init` wraps a check that every `m4_divert_push`
  was popped, then pushes `KILL` and wraps the matching pop, which must run
  first.  It does not pass `--gnu`, so this has to be the default.

`regexp` and `patsubst` take the longest of the matches that start at the
leftmost position, as GNU regex does (`regexp(abcd, a\|ab, \&)` is `ab`),
with the groups of the first path to that match.  One difference remains:
under a multibyte locale GNU regex matches `.` and bracket expressions
against whole characters, while this m4 matches them against bytes in
every locale.

## [`sendmail`](https://www.proofpoint.com/us/products/email-protection/open-source-email-solution)

- [x] Checked generating default configuration.

### Argument expansion order

This section used to record a plan to rewrite the evaluation engine, on the
grounds that argument expansion was breadth-first where BSD and GNU m4 are
depth-first. **That is no longer the case, and the plan was carried out.** The
note is kept, rewritten, because "we are incompatible with sendmail" is the
kind of claim that gets repeated for years after it stops being true.

The engine is a pushback/rescan design, the classic m4 shape:

* `main_loop::process` reads one byte at a time, from a pushback buffer before
  the input file.
* Seeing `name(` pushes a `StackFrame`, which reroutes `Output::write_all` into
  the frame's current argument buffer. So **collecting an argument and
  expanding it are the same pass** — a nested call encountered while gathering
  an outer call's arguments is applied as soon as its `)` is seen, before the
  outer macro runs.
* Every macro's result is pushed back onto the *input* (`pushback_string`), not
  written to the output, so it is re-lexed by the same loop and lands in the
  enclosing frame's argument buffer.

That is depth-first by construction. No code anywhere expands arguments in a
separate pass; there is no "for each argument, expand it" loop to find.

The remark about `dnl` is also inverted now: the builtin consumes raw input
directly, including the pushback buffer, rather than working at the parsing
level. The nested-divert test the note was waiting on passes, with four
committed fixtures.

### What backs that up

`gnu_m4_differential` in `tests/integration.rs` runs every stdin-driven
fixture through both this m4 and the system's GNU m4 and requires the stdout to
agree. At the time of writing that is **80 of 80 fixtures, byte for byte**,
against GNU M4 1.4.19.

It checks that the reference *is* GNU m4, and is at least 1.4.19, by reading
its `--version`; otherwise it skips loudly and compares nothing. That check is
not ceremony. macOS's `/usr/bin/m4` is a different implementation, and it
disagrees with GNU M4 1.4.19 — on `eval`'s `!` and `~`, on quote handling in
`define_hanging_quotes`, and on `define_nested_first_arg` — while this m4's
output is byte-identical on both platforms. An earlier version of the gate took
whatever sat at that path and called it GNU m4, which said nothing about this
implementation and turned macOS CI red for an environment difference. The
version floor is there for the same reason: these fixtures encode one
reference's answers, and GNU m4 has changed some of them between releases.

Before that gate existed, the committed `.out` files were frozen snapshots of
what *this* implementation produced — they could not have caught the
documentation drifting away from the code, and did not.

`maketemp`/`mkstemp` are excluded: they generate random filenames by design, so
no two runs agree. Only stdout is compared; diagnostic wording is deliberately
ours.

### `m4wrap` order

`m4wrap` is LIFO by default, as in GNU m4 1.4.x, and FIFO under
`POSIXLY_CORRECT`:

```
m4wrap(`W1 ')m4wrap(`W2 ')body
  default, and GNU m4 1.4:  body / W2 W1
  POSIXLY_CORRECT:          body / W1 W2
```

POSIX says the wrapped text is "processed in the order in which the `m4wrap`
macros were processed", which is FIFO, but autoconf-dickey depends on GNU's
order (see the autoconf section above).  Neither is a sendmail-compatibility
question.

### Unverified

Whether sendmail's own `cf/` macro set builds correctly has **not** been
re-checked since the engine was reworked. The original "currently it's
incompatible" note gave argument expansion order as the reason, and that reason
is gone — but the absence of a cause is not evidence of success. Anyone with a
sendmail source tree to hand should run it and replace this paragraph with a
result.

### FreeBSD m4 reference links

Kept because they are useful reading for anyone working on the engine.

- [1]: https://github.com/freebsd/freebsd-src/blob/main/usr.bin/m4/eval.c#L123
- [2]: https://github.com/freebsd/freebsd-src/blob/main/usr.bin/m4/eval.c#L207-L217
- [3]: https://github.com/freebsd/freebsd-src/blob/main/usr.bin/m4/misc.c#L94-L109
- [4]: https://github.com/freebsd/freebsd-src/blob/main/usr.bin/m4/misc.c#L199-L212
- [5]: https://github.com/freebsd/freebsd-src/blob/main/usr.bin/m4/NOTES#L32
- [6]: https://github.com/freebsd/freebsd-src/blob/main/usr.bin/m4/main.c#L434
- [7]: https://github.com/freebsd/freebsd-src/blob/main/usr.bin/m4/main.c#L480
- [8]: https://github.com/freebsd/freebsd-src/blob/main/usr.bin/m4/misc.c#L406-L413
- [9]: https://github.com/freebsd/freebsd-src/blob/main/usr.bin/m4/main.c#L341
