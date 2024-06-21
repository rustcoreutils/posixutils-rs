# Compatibility

This document outlines notes about compatibility of this software with other software that uses m4.

## autoconf

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