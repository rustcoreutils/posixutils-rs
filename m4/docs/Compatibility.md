# Compatibility

This document outlines notes about compatibility of this software with other software that uses m4.

## [`autoconf`](https://www.gnu.org/software/autoconf/)

[GNU m4](https://www.gnu.org/software/m4/) is probably the most widely used version of m4. The place where it perhaps receives the most use is in [autoconf](https://www.gnu.org/software/autoconf/). `autoconf` is a very popular tool for building software and perhaps the place where m4 is used the most in practice today. As such priority has been placed on establishing what would be needed for this version of m4 to provide compatibility with the [GNU specific m4 features/behaviours](https://www.gnu.org/software/m4/manual/html_node/Extensions.html#Extensions) to allow it to be used as a drop-in replacement to use with autoconf. The first goal was to run autoconf with the GNU [hello project](https://www.gnu.org/software/hello/)

At least the following CLI arguments (at a minimum) still need to be added and supported:

```rust
 /// Freeze state into a file.
    #[arg(short = 'F', long)]
    pub freeze_state: Option<PathBuf>,
    /// Reload a frozen state from a file.
    #[arg(short = 'R', long)]
    pub reload_state: Option<PathBuf>,
    /// Change nesting limit. 0 for unlimited.
    #[arg(short = 'L', long)]
    pub nesting_limit: Option<usize>,
    /// Override --traditional to re-enable GNU extensions
    #[arg(short = 'g', long, default_value_t = default_gnu())]
    pub gnu: bool,
    /// Suppress all GNU extensions.
    #[arg(short = 'G', long, default_value_t = default_traditional())]
    pub traditional: bool,
    // Append DIRECTORY to include path.
    #[arg(short = 'I', long)]
    pub include: Option<PathBuf>,
    /// Set debug level (no FLAGS implies `aeq')
    #[arg(short = 'd', long, default_value = "aeq")]
    pub debug: Option<String>,
    /// Redirect debug and trace output to FILE
    /// (default stderr, discard if empty string).
    #[arg(long)]
    pub debugfile: Option<PathBuf>,
    /// once: warnings become errors, twice: stop
    /// execution at first error.
    #[arg(short = 'E', long)]
    pub fatal_warning: bool,
    /// Trace specified macro name when it is defined.
    #[arg(short = 't', long)]
    pub trace: Vec<String>,
```

GNU m4 specific builtin macros likely to be necessary (there are probably more) were found by grepping the `.m4` files for `m4_*` macros, because all autoconf m4 macros have been renamed as such, which makes them easy to find:

* [copy](https://www.gnu.org/software/m4/manual/m4.html#index-copy)
* [patsubstr](https://www.gnu.org/software/m4/manual/m4.html#index-patsubst)
* [fatal_error](https://www.gnu.org/software/m4/manual/m4.html#index-fatal_005ferror)
* [esyscmd](https://www.gnu.org/software/m4/manual/m4.html#index-esyscmd)
* [foreach](https://www.gnu.org/software/m4/manual/m4.html#index-foreach)

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
against GNU M4 1.4.19. It skips loudly when GNU m4 is not installed, so CI
stays green without it.

Before that gate existed, the committed `.out` files were frozen snapshots of
what *this* implementation produced — they could not have caught the
documentation drifting away from the code, and did not.

`maketemp`/`mkstemp` are excluded: they generate random filenames by design, so
no two runs agree. Only stdout is compared; diagnostic wording is deliberately
ours.

### Known intentional divergence

`m4wrap` is FIFO here and LIFO in GNU m4 1.4.x:

```
m4wrap(`W1 ')m4wrap(`W2 ')body
  ours:  body / W1 W2
  GNU:   body / W2 W1
```

POSIX says the wrapped text is "processed in the order in which the `m4wrap`
macros were processed", which is FIFO. GNU 2.0 changed to match. This one is
deliberate and is not a sendmail-compatibility question.

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
