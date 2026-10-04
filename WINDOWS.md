# Windows

What builds on Windows (`x86_64-pc-windows-msvc`) and how it behaves there,
then how another workspace crate is ported.

## Using the utilities on Windows

These crates build and are tested on Windows, natively with MSVC (no MinGW or
Cygwin runtime):

| Crate | Utilities |
|---|---|
| `calc` | `bc`, `expr` |
| `datetime` | `cal`, `date`, `sleep`, `time` |
| `dev` | `ar`, `lex`, `nm`, `strings`, `strip`, `yacc` |
| `display` | `echo`, `more`, `printf` |
| `text` | `asa`, `comm`, `csplit`, `cut`, `diff`, `expand`, `fold`, `grep`, `head`, `join`, `nl`, `paste`, `patch`, `pr`, `sed`, `sort`, `tail`, `tr`, `tsort`, `unexpand`, `uniq`, `wc` |
| `xform` | `cksum`, `compress`, `uuencode`, `uudecode` |

```sh
cargo build --release -p posixutils-calc -p posixutils-datetime -p posixutils-dev -p posixutils-display \
    -p posixutils-text -p posixutils-xform
```

What behaves differently on Windows:

- a file's POSIX mode is its read-only attribute, read as the owner-write bit
  (`0444` or `0644`); setting a mode without owner write makes it read-only;
- `compress` restores permissions and times but not ownership, and does not
  warn about hard links; the `zcat` and `uncompress` aliases do not exist
  (use `compress -c -d` and `compress -d`);
- `LC_ALL`, `LC_*` and `LANG` set to `C` or `POSIX` select the C locale:
  ASCII-only character classes and case mapping, one byte per character, and
  byte-order collation; `C.UTF-8` (or any `C`/`POSIX` name with a codeset)
  collates in byte order but reads UTF-8 and classifies and case-maps by
  Unicode; any other value, or none, is the user's locale with UTF-8 input and
  Unicode characters;
- POSIX regular expressions are musl's (vendored in `plib/vendor/musl-regex`)
  and do not support characters above U+FFFF;
- `sort -n` always takes `.` as the decimal point, with no thousands
  separator;
- `diff` reports anything that is neither a file nor a directory as a
  special file, and recognises a directory loop by its canonical path;
- `pr -p` and `patch` prompt on the console; `csplit` removes its files on
  Ctrl-C, Ctrl-Break and termination, Windows having no hangup or quit
  signal;
- `date` and `cal` name months and days as the POSIX locale does, whatever
  `LC_TIME` says; `TZ` takes the C runtime's forms (`UTC0`, `EST5EDT`: a
  three-letter name, an offset, an optional daylight name) and not zoneinfo
  names such as `America/New_York`; unset, it is the system time zone;
- `date` sets the clock only with the system-time privilege, and reads the
  local time it is given in the system time zone, not `TZ`;
- `time` reports its own CPU time plus the utility's, Windows keeping no
  totals for a process's descendants;
- `expr`, `echo` and `printf` operands are text: a Windows command line
  cannot carry bytes that are not UTF-8;
- `ar` records user and group 0 for a member it adds, and the member's mode
  from the read-only attribute; a member name that is not UTF-8 is extracted
  with U+FFFD in place of the bytes that are not; `-x` refuses a member named
  for a device (`CON`, `NUL`, `COM1`...) or a stream (`file:stream`), and
  counts NAME_MAX in characters (UTF-16 units), not bytes;
- `more` needs Windows 10 or later (its console must take VT sequences); it
  reads commands from the console, redraws when the window has been resized
  the next time it looks for a key, and has no job control; `vi.exe`,
  `vi.cmd` and the like count as `vi` for `v`'s `-c` line; `:e` expands a
  leading `~` (`HOME`, else `USERPROFILE`) and `$NAME` or `${NAME}` and
  removes quotes, but does no field splitting, pathname expansion or command
  substitution, and `\` is a path separator, not an escape.

## Porting a crate

How a workspace crate is made to build, and its tests to pass, on Windows,
and how it then joins CI. Follow it in order; each step names what to check
before going on.

### Rules

- **A whole crate at a time.** `cargo test -p <crate>` builds every binary in
  the crate, so a crate is ported when *all* of its binaries and tests compile
  and pass. A crate joins `WINDOWS_CRATES` in `.github/workflows/TestingCI.yml`
  only then.
- **Gate, never stub.** Code with no Windows meaning is compiled out with
  `#[cfg(unix)]`. Nothing is replaced by a stand-in that pretends to work: a
  binary that exists on Windows does its job there.
- **Unix behaviour does not change.** Every Unix code path stays as it was,
  unless one portable std API now serves both platforms (then it is one rule
  for both, and the Unix suite proves nothing moved).
- **The Windows meaning of the same thing.** Where Unix has a concept Windows
  expresses differently, implement that meaning rather than dropping the
  feature (see the table below).
- **One rule, one helper.** A platform difference lives in one small
  `#[cfg(unix)]` / `#[cfg(windows)]` pair of helpers named for what they mean
  (`mode_of`, `set_mode`, `remove_file`, `link_count`), called from shared
  code. Never scatter `cfg` through a function body twice for the same rule.

### Windows meaning of Unix concepts

| Unix | Windows |
|---|---|
| permission bits | the read-only attribute is the owner-write bit: a file reads as `0444` or `0644`; setting a mode without owner write sets read-only |
| umask | none; a new file is `0644` |
| uid, gid, `chown` | none: keep Unix-only |
| `access(W_OK)` | "not read-only" |
| `nlink` | not exposed by stable Rust: a file counts as its only link |
| `pathconf(_PC_NAME_MAX)`, `PATH_MAX` | 255 UTF-16 units (an NTFS component), counted as such, not as bytes; no path-length check |
| a plain file name (one component, not `.` or `..`) | also no `:` (a drive or a stream) and no device name (`CON`, `PRN`, `AUX`, `NUL`, `COM1`-`COM9`, `LPT1`-`LPT9`, with any extension and trailing dots or spaces) |
| `utimensat` | `File::set_times`, portable (set through an open write handle: a reopen can be refused) |
| `isatty` | `std::io::IsTerminal`, portable |
| `SIGPIPE` | none: a write to a closed pipe is an error; `restore_sigpipe` does nothing |
| `SIGINT`, `SIGTERM` handlers | the C runtime's `signal()` has both; `SIGHUP`, `SIGQUIT` do not exist |
| `strerror_r` | Rust's own error text (already the system's) |
| `LC_MESSAGES` | none: `LC_ALL` |
| path lists (`:`) | `std::env::split_paths` (`;` on Windows) |
| argv[0] symlink aliases (`zcat`, `[`) | not created (`build.rs` is Unix-only); use the main name's flags |
| deleting a read-only file | refused under Wine and older Windows: clear the attribute first, restore it on failure |
| `mkstemp`, `mkdtemp`, unnamed temporaries | `plib::tmp`: `CREATE_NEW` under a random name; an unnamed temporary is a delete-on-close file, named until its last handle closes |
| `/dev/tty` in raw mode (termios) | the console, `CONIN$` and `CONOUT$` opened read and write: input without line editing, echo or Ctrl-C processing, with `ENABLE_VIRTUAL_TERMINAL_INPUT`; output with `ENABLE_VIRTUAL_TERMINAL_PROCESSING` |
| `TIOCGWINSZ`, `SIGWINCH` | `GetConsoleScreenBufferInfo`'s window; no resize signal, so ask again when the size matters |
| `SIGTSTP`, `SIGCONT` | none: no job control |
| `SIGHUP`, `SIGTERM` handlers that restore the terminal | `SetConsoleCtrlHandler`, which also sees Ctrl-Break, console close, logoff and shutdown |
| set-user-ID, set-group-ID | none: keep Unix-only |
| `wordexp` | none in the C runtime: `~` and `$NAME`/`${NAME}`, quotes removed, one word |
| a program's name (`basename "$EDITOR"`) | the last component less a program extension (`.exe`, `.com`, `.bat`, `.cmd`) |
| `/dev/tty` | the console, `CONIN$` / `CONOUT$`: `plib::io::open_terminal_input` / `open_terminal_output` |
| `LC_ALL`, `LC_*`, `LANG` | read per category by `plib::diag::init_locale`: `C` or `POSIX` selects the C locale (the C runtime's `"C"`, and ASCII-only, byte-per-character `plib::locale`); `C.UTF-8` and other `C`/`POSIX` names with a codeset select the C runtime's `"C"` except for `LC_CTYPE`, which stays UTF-8; anything else, or unset, the user's locale in UTF-8 |
| characters, case, multibyte | `plib::locale`: outside the C locale, Rust's Unicode rules with input decoded as UTF-8; before `init_locale`, the C locale |
| POSIX regex | vendored musl regex (`plib::regex`); no characters above U+FFFF |
| `localeconv` (decimal point, grouping) | `.` and no grouping |
| `dev`/`ino` file identity | the canonical path (stable Rust has no file ID) |
| FIFOs, devices, sockets | none: neither a file nor a directory is "special" |
| `SIGQUIT` | `SIGBREAK` (Ctrl-Break) where a quit key is meant |
| absolute paths | a root (`\x`), a drive prefix (`C:x`) or `..` must all be refused where only relative names are allowed |
| `localtime_r`, `gmtime_r`, `TZ` | the C runtime's `localtime_s` / `gmtime_s`, which read `TZ` in its own forms (no zoneinfo); the zone name is its `strftime("%Z")` |
| `strftime`, `LC_TIME` | `plib::timefmt`: the POSIX locale's names and formats, `LC_TIME` ignored |
| `clock_settime` | `SetSystemTime` (needs the system-time privilege) |
| `times()` children's CPU | `GetProcessTimes` on the waited-for child's handle, added to the process's own |
| argv as bytes (`OsStrExt::as_bytes`) | `OsStr::as_encoded_bytes`, portable: the same bytes on Unix, UTF-8 on Windows |

### Steps

#### 1. Survey

```sh
cargo check --target x86_64-pc-windows-msvc -p <crate> --all-targets 2>&1 | grep -E '^error'
grep -rnE 'os::unix|os::fd|libc::' <crate> | grep -v '^<crate>/tests'
```

Sort the errors into: missing `plib` modules (step 2), the crate's own sources
(step 3), and its tests (step 4). Decide per binary whether every feature has a
Windows meaning; a binary that cannot be ported keeps the whole crate off
Windows (or the crate is split), never stubbed.

#### 2. plib

`plib` is the base of every crate. Its modules with no Windows meaning are
`#[cfg(unix)]` in `plib/src/lib.rs`; a crate that needs one ports that module
(or the part it uses) first, as its own commit, with the module's unit tests
running on Windows. Ported so far: `diag`, `io` (including the terminal for
prompts), `locale` (characters, case and `strftime`), `lzw`, `regex`, `testing`,
`archive`, `cscan`, `linediff`, `perm` (a file's mode, both platforms), `tmp`. Still
Unix-only: `curuser`, `exec`, `group`, `modestr`, `platform`, `priority`,
`projectdir`, `sccsfile`, `syslog`, `test_expr`, `tty`, `user`, `utmpx`.

#### 3. The crate's sources

Apply the table above through small cfg'd helpers. Prefer a portable std API
that serves both platforms when it is exactly equivalent on Unix. Read every
`libc::` and `std::os::unix` use; `cargo check --target ...` finds them, but
not the ones that compile and mean something else (a path with `/dev/stdout`,
a `:`-separated list).

#### 4. Tests

- Integration tests find binaries with `plib::testing::get_binary_path`, which
  already handles `.exe` and `--target` layouts.
- Gate a test `#[cfg(unix)]` only when its subject is Unix (modes, umask,
  `utimensat`, root, signals, the argv[0] aliases), and say why in
  a comment. A test of portable behaviour that merely used a Unix helper is
  rewritten to run everywhere (pick the Windows equivalent, or a portable
  helper), not gated.
- Fixtures are compared byte for byte; `.gitattributes` keeps them LF on a
  Windows checkout. A fixture whose CRLF is the point is listed there as
  `-text`.
- Windows will not delete a read-only file under Wine: clear the attribute
  before cleanup.
- A test that compiles C takes the compiler from `$CC`, else `cc`, or `gcc`
  on Windows, where the runner has MinGW's on the path and no `cc`; a program
  it builds is named with `std::env::consts::EXE_SUFFIX`, and what that
  program writes to a text stream has CR LF line ends on Windows.
- Tests that drive a program through a pseudo-terminal (`more`'s PTY
  sessions) are Unix-only. On the Windows runner a program driven through
  the pseudo-console (portable-pty over ConPTY) never has anything it writes
  after the first input come back, `cmd.exe` included, so such a test
  exercises the harness rather than the utility. The console path is checked
  by hand under Wine's console instead.
- A stand-in program a test runs (an `EDITOR`) is a shell script on Unix and
  a batch file on Windows; in the batch file, put a redirection before its
  command (`>>"file" echo %%~a`), which also keeps an argument such as `1`
  from reading as a handle number.

#### 5. Verify

Every commit, never two cargo commands at once:

```sh
cargo fmt --all -- --check
cargo clippy --all-targets -- -D warnings                          # Linux, zero
cargo clippy --target x86_64-pc-windows-msvc <crates> --all-targets -- -D warnings
cargo test --release -p <crate>                                    # Unix unchanged
cargo +<rust-version> check --target x86_64-pc-windows-msvc <crates> --all-targets
```

and run the Windows tests on Linux under Wine (needs the `mingw-w64` and `wine`
packages; MinGW is not MSVC, so CI has the last word):

```sh
rustup target add x86_64-pc-windows-gnu
WINEDEBUG=-all CARGO_TARGET_X86_64_PC_WINDOWS_GNU_RUNNER=wine \
    cargo test --release --target x86_64-pc-windows-gnu <crates>
```

A change to `plib::testing` or anything every crate uses gets the full
`cargo test --release` on Linux.

Wine's limits: the `-gnu` target links `msvcrt`, which refuses the UTF-8
locale, so UTF-8 multibyte regex behaviour is exercised only by CI's MSVC
build; and Wine refuses to delete a read-only file even where current Windows
does, which is the stricter behaviour to be correct against. Three
`datetime` tests fail under Wine 9 and pass only on Windows: its `msvcrt`
parses `TZ` wrongly (`TZ=UTC` names the zone `UT`), failing `date`'s
`test_tz_utc` and `test_default_format_utc`; and its `GetProcessTimes`
answers the caller's own times for another process, failing `time`'s
`cpu_bound_child_reports_nonzero_cpu_time`. Wine maps `/dev/full` to the
Linux device, so the tests that write to it run there and not on Windows.

Wine's console neither interprets VT output nor sends keys as VT input, and
its pseudo-console passes no output through at all. Wine starts a Linux
program but cannot wait for it or read its status, so the tests that compile
C need a Windows `gcc` inside Wine (a WinLibs MinGW build works): build the
tests without `CC` set, since the `cc` crate reads it while building `plib`,
then run each test executable under `wine` with `CC` naming that `gcc.exe`.
Wine's `cmd` drops the arguments of a batch file whose loop is redirected as
`(for ...) > file`, and does not know `echo(`.

#### 6. CI and docs

Add `-p <crate>` to `WINDOWS_CRATES` in `.github/workflows/TestingCI.yml`; the
`windows` job, the lint job's Windows clippy and the `msrv` job's Windows
check all read it. Add the crate and its utilities to the table under "Using
the utilities on Windows" and to its build command, anything that behaves
differently to the list there, and any new Unix→Windows meaning to the table
above. README only points here: it is not edited per crate.

#### 7. Commits

A bisectable series, each commit building and passing clippy on both targets
on its own: plib modules first, then the crate's sources, then its tests,
then CI, then docs. A fix to a bug a commit in the series introduced is
folded into that commit, not appended.
