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

### Notes

Currently it's incompatible due to problems with the way this m4 works.

The BSD implementation of m4, when calling macros their arguments are already fully expanded[1][1]
It actually makes them very simple to test and reason about, and that's something I'd like to aim for with this implementation.

`pbstr` in their impelementation is used push argument strings onto the input stack to be evaluated[2][2][3][3]. The implementation of this stack growth seems very similar to Rust's `Vec`[4][4].

There are some notes about how the macro call stack works [5][here], it can be seen in action [6][here] and [7][here]

The main question here is how can we mix our parsing implementation with this without completely rewriting it?

Okay something I learned is that the input keeps track of its line number, has a name [8][8].

Output is controlled by [active](https://github.com/freebsd/freebsd-src/blob/main/usr.bin/m4/main.c#L86) file pointer. outfile[] is used for diverts.

Summary of the main loop (not 100 percent precise, but close enough):

First get the next character (either from pushback buffer or current input file).

Next do one of the following:

* Unwrap Quotes if requred
  a) If we're at the top level output to the currently active output.
  b) Otherwise output onto the stack?
* If we're at the top level of the stack and this is a comment, output the comment to currently active output.
* Parse as macro
  a) If it's not a current macro name or there are no arguments when the macro requires it, then continue.
  b) Otherwise create a stack for the macro invocation, and push the first argument (name of macro). If it's macro without arguments then evaluate it.
* Do some stuff if it's end of file.
* If we're not in a macro, put the character in active output.
* If the character is left parenthesis, increase the `PARLEV`, put in active output.
* If the character is right parenthesis, decrease the `PARLEV`
  a) If `PARLEV > 0` Put the char to output.
  b) If the `PARLEV <= 0` we are at the end of argument list, evaluate macro.
* If the char is a comma
  a) If we're at `PARLEV == 1` (inside the first level of a macro invocation), skip all the spaces, then push the character onto the macro argument stack?
  b) ?
* If it's a comment, consume input until end comment and put on macro stack.
* Put the character on the macro stack.

To evaluate the macro:

1. If it's a recursive definition, exit with error?
2. Trace the macro if required.
3. Check if macro is user defined, and expand the macro (different implementation for builtin or user defined).
4. Finish tracing.

To evaluate user defined macro:

Go through the definition characters, if we find a match to replacement characters, then push the replacement to the input stack, otherwise just push definition character to the input stack.

Their builtin dnl macro actually just consumes input directly until newline, no external configuration! In contrast to ours which works on the parsing level.

So in the end I think we get a functionality that iteratively, depth-first evaluates macro arguments.

To make our approach similar I think we should probably go back to a stack based approach. Previously when I went with a stack approach I ended up doing it breadth first by mistake. But first I'll check exactly why our divert nested test is failing and see if there can't be some short term workaround.

- [1]: https://github.com/freebsd/freebsd-src/blob/main/usr.bin/m4/eval.c#L123
- [2]: https://github.com/freebsd/freebsd-src/blob/main/usr.bin/m4/eval.c#L207-L217
- [3]: https://github.com/freebsd/freebsd-src/blob/main/usr.bin/m4/misc.c#L94-L109
- [4]: https://github.com/freebsd/freebsd-src/blob/main/usr.bin/m4/misc.c#L199-L212
- [5]: https://github.com/freebsd/freebsd-src/blob/main/usr.bin/m4/NOTES#L32
- [6]: https://github.com/freebsd/freebsd-src/blob/main/usr.bin/m4/main.c#L434
- [7]: https://github.com/freebsd/freebsd-src/blob/main/usr.bin/m4/main.c#L480
- [8]: https://github.com/freebsd/freebsd-src/blob/main/usr.bin/m4/misc.c#L406-L413
- [9]: https://github.com/freebsd/freebsd-src/blob/main/usr.bin/m4/main.c#L341

