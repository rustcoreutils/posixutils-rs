# Porting a crate to Windows

How a workspace crate is made to build, and its tests to pass, on Windows
(`x86_64-pc-windows-msvc`), and how it then joins CI. Follow it in order; each
step names what to check before going on.

## Rules

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

## Windows meaning of Unix concepts

| Unix | Windows |
|---|---|
| permission bits | the read-only attribute is the owner-write bit: a file reads as `0444` or `0644`; setting a mode without owner write sets read-only |
| umask | none; a new file is `0644` |
| uid, gid, `chown` | none: keep Unix-only |
| `access(W_OK)` | "not read-only" |
| `nlink` | not exposed by stable Rust: a file counts as its only link |
| `pathconf(_PC_NAME_MAX)`, `PATH_MAX` | 255 (NTFS component), no path-length check |
| `utimensat` | `File::set_times`, portable (set through an open write handle: a reopen can be refused) |
| `isatty` | `std::io::IsTerminal`, portable |
| `SIGPIPE` | none: a write to a closed pipe is an error; `restore_sigpipe` does nothing |
| `SIGINT`, `SIGTERM` handlers | the C runtime's `signal()` has both; `SIGHUP`, `SIGQUIT` do not exist |
| `strerror_r` | Rust's own error text (already the system's) |
| `LC_MESSAGES` | none: `LC_ALL` |
| path lists (`:`) | `std::env::split_paths` (`;` on Windows) |
| argv[0] symlink aliases (`zcat`, `[`) | not created (`build.rs` is Unix-only); use the main name's flags |
| deleting a read-only file | refused under Wine and older Windows: clear the attribute first, restore it on failure |
| `/dev/tty` | the console, `CONIN$` / `CONOUT$`: `plib::io::open_terminal_input` / `open_terminal_output` |
| `LC_ALL`, `LC_*`, `LANG` | read per category by `plib::diag::init_locale`: `C` or `POSIX` selects the C locale (the C runtime's `"C"`, and ASCII-only, byte-per-character `plib::locale`); `C.UTF-8` and other `C`/`POSIX` names with a codeset select the C runtime's `"C"` except for `LC_CTYPE`, which stays UTF-8; anything else, or unset, the user's locale in UTF-8 |
| characters, case, multibyte | `plib::locale`: outside the C locale, Rust's Unicode rules with input decoded as UTF-8; before `init_locale`, the C locale |
| POSIX regex | vendored musl regex (`plib::regex`); no characters above U+FFFF |
| `localeconv` (decimal point, grouping) | `.` and no grouping |
| `dev`/`ino` file identity | the canonical path (stable Rust has no file ID) |
| FIFOs, devices, sockets | none: neither a file nor a directory is "special" |
| `SIGQUIT` | `SIGBREAK` (Ctrl-Break) where a quit key is meant |
| absolute paths | a root (`\x`), a drive prefix (`C:x`) or `..` must all be refused where only relative names are allowed |

## Steps

### 1. Survey

```sh
cargo check --target x86_64-pc-windows-msvc -p <crate> --all-targets 2>&1 | grep -E '^error'
grep -rnE 'os::unix|os::fd|libc::' <crate> | grep -v '^<crate>/tests'
```

Sort the errors into: missing `plib` modules (step 2), the crate's own sources
(step 3), and its tests (step 4). Decide per binary whether every feature has a
Windows meaning; a binary that cannot be ported keeps the whole crate off
Windows (or the crate is split), never stubbed.

### 2. plib

`plib` is the base of every crate. Its modules with no Windows meaning are
`#[cfg(unix)]` in `plib/src/lib.rs`; a crate that needs one ports that module
(or the part it uses) first, as its own commit, with the module's unit tests
running on Windows. Ported so far: `diag`, `io` (including the terminal for
prompts), `locale` (characters and case), `lzw`, `regex`, `testing`,
`archive`, `cscan`, `linediff`. Still
Unix-only: `curuser`, `exec`, `group`, `modestr`, `platform`, `priority`,
`projectdir`, `sccsfile`, `syslog`, `test_expr`, `tmp`, `tty`, `user`, `utmpx`.

### 3. The crate's sources

Apply the table above through small cfg'd helpers. Prefer a portable std API
that serves both platforms when it is exactly equivalent on Unix. Read every
`libc::` and `std::os::unix` use; `cargo check --target ...` finds them, but
not the ones that compile and mean something else (a path with `/dev/stdout`,
a `:`-separated list).

### 4. Tests

- Integration tests find binaries with `plib::testing::get_binary_path`, which
  already handles `.exe` and `--target` layouts.
- Gate a test `#[cfg(unix)]` only when its subject is Unix (modes, umask,
  `utimensat`, root, signals, the argv[0] aliases, `plib::tmp`), and say why in
  a comment. A test of portable behaviour that merely used a Unix helper is
  rewritten to run everywhere (pick the Windows equivalent, or a portable
  helper), not gated.
- Fixtures are compared byte for byte; `.gitattributes` keeps them LF on a
  Windows checkout. A fixture whose CRLF is the point is listed there as
  `-text`.
- Windows will not delete a read-only file under Wine: clear the attribute
  before cleanup.

### 5. Verify

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
does, which is the stricter behaviour to be correct against.

### 6. CI and docs

Add `-p <crate>` to `WINDOWS_CRATES` in `.github/workflows/TestingCI.yml`; the
`windows` job, the lint job's Windows clippy and the `msrv` job's Windows
check all read it. List the crate's utilities in README's "Windows" section,
with anything that behaves differently there, and add any new Unix→Windows
meaning to the table above. Ported so far: `xform`, `text`.

### 7. Commits

A bisectable series, each commit building and passing clippy on both targets
on its own: plib modules first, then the crate's sources, then its tests,
then CI, then docs. A fix to a bug a commit in the series introduced is
folded into that commit, not appended.
