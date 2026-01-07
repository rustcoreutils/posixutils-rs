# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## CRITICAL: Git Restrictions

**NEVER `git commit` or `git add`.** without asking.

## Project Overview

posixutils-rs is a suite of Rust-native POSIX command line utilities (cp, mv, awk, make, vi, sh, etc.) using POSIX.2024 as the baseline specification. The goal is clean, race-free, POSIX-compliant utilities that maximize shell script compatibility while minimizing bloat.

**Minimum Rust version**: 1.84.0 | **License**: MIT | **Platforms**: Linux, macOS

## Build and Test Commands

```bash
# Build
cargo build --release			# ALWAYS (do not add -p etc)

# Test all (long, 15+ minutes)
cargo test --release

# Test single crate
cargo test --release -p posixutils-text     # for text utilities
cargo test --release -p posixutils-fs       # for filesystem utilities

# Test single function
cargo test --release -p posixutils-fs test_cp_basic

# Test with all features (includes tests requiring special setup)
cargo test --release --features posixutils_test_all

# Tests requiring root (run individually)
sudo -E cargo test --release --features posixutils_test_all,requires_root <test_name>

# Format check (required for CI)
cargo fmt --all -- --check
```

Integration tests may generate data under `CARGO_TARGET_TMPDIR` (usually `target/tmp`) and `/dev/shm` (Linux).

## Architecture

**Workspace Structure**: Cargo workspace with utilities organized by category:
- `text/` - Text processing (cat, cut, head, tail, sort, uniq, tr, sed, grep, etc.)
- `fs/` - Filesystem operations (cp, mv, rm, ln, ls, chmod, chown, mkdir, etc.)
- `process/` - Process management (kill, nice, nohup, time, xargs, etc.)
- `awk/` - AWK implementation
- `sh/` - Shell implementation
- `make/` - Make implementation
- `editors/` - vi/ex editors
- `plib/` - Shared library with TestPlan framework and common utilities
- `ftw/` - Race-free file tree walking library

Other crates: `calc/` (bc), `cc/` (c99), `cron/`, `datetime/`, `dev/` (development tools), `display/`, `file/`, `m4/`, `mailx/`, `pax/`, `sccs/`, `screen/`, `sys/`, `tree/`, `users/`, `uucp/`, `xform/`, `i18n/`.

**Shared Library (plib)**: Contains:
- `testing.rs` - TestPlan framework for integration tests
- Common utilities: user/group handling, mode string parsing, LZW compression, regex helpers

## Code Style Requirements

1. **`clippy clean and cargo fmt` is required** - Enforced in CI
2. **No warnings** - Code must compile warning-free
3. **No `#[allow(dead_code)]`** - Delete unused code, use `#[cfg(test)]` for test-only code
4. **Minimal dependencies** - Prefer std-only; when needed, use tiny single-purpose crates
5. **Standard crates**: clap (CLI), libc (syscalls), regex, chrono
6. **Small functions**: If a function becomes large, create helpers to shrink it back down.  e.g. refactor match arms into helpers.

## Testing Pattern

For Claude Code Bash tool debugging, diagnostics and testing, use our DEBUG PROTOCOL:
(1) Update EXISTING /tmp/*.sh script, with the test logic
(2) Bash runs /tmp/$YOUR_SCRIPT.sh to see debug output
This is intentionally minimizes prompting the user for input during debugging.

Integration tests use plib's TestPlan framework:
```rust
use plib::testing::{TestPlan, run_test};

let plan = TestPlan {
    cmd: String::from("utility_name"),
    args: vec![String::from("-flag"), String::from("arg")],
    stdin_data: String::from("input"),
    expected_out: String::from("expected output"),
    expected_err: String::new(),
    expected_exit_code: 0,
};
run_test(plan);
```

Test structure:
- Integration test harness files should ONLY contain `mod` statements
- Test logic goes in `$crate/tests/$category/mod.rs`
- Pre-generate input/output data by running system utilities for reference

Pure-function type testing belongs in unit tests.  The remainder should be in
end-to-end integration tests (preferred).

## Core Principles

1. **POSIX.2024 compliance first** - Implement POSIX spec before considering GNU/BSD extensions
2. **Minimalism** - Only implement widely-used extensions, avoid rarely-used options
3. **Race-free** - Use race-free patterns, especially for file operations (see `ftw/`)
4. **Correctness > Readability > Performance** - In that priority order
