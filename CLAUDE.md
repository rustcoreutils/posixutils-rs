# CLAUDE.md

## CRITICAL: Git Restrictions

**NEVER `git commit` or `git add`** without asking.

## Project Overview

posixutils-rs: Rust-native POSIX utilities (cp, mv, awk, sh, cc, make, vi, etc.) targeting POSIX.2024. Goal: clean, race-free, POSIX-compliant utilities.

**Rust**: 1.84.0+ | **License**: MIT | **Platforms**: Linux, macOS

## Commands

```bash
cargo build --release           # Build (ALWAYS full workspace, no -p)
cargo test --release            # Test all (15+ min)
cargo test --release -p posixutils-text  # Test single crate
cargo clippy                    # Lint (required)
cargo fmt --all -- --check      # Format check (required)
```

## Git

NEVER amend git commits.

Pre-commit checks,
- First re-review `git diff HEAD` in totality
- Passes `cargo clippy --all-targets` with zero warnings (pre-existing warnings MUST be fixed)

## Architecture

Workspace by category: `text/`, `fs/`, `process/`, `awk/`, `sh/`, `make/`, `editors/`, `plib/` (shared lib), `ftw/` (race-free file walking).

Other: `calc/`, `cc/`, `cron/`, `datetime/`, `dev/`, `display/`, `file/`, `m4/`, `mailx/`, `pax/`, `sccs/`, `screen/`, `sys/`, `tree/`, `users/`, `uucp/`, `xform/`, `i18n/`.

## Code Style

1. **Zero warnings** - clippy and compiler
2. **No `#[allow(dead_code)]`** - delete unused code
3. **Minimal deps** - prefer std; use clap, libc, regex, chrono
4. **Small functions** - refactor large functions into helpers

## Testing

Debug protocol:
Update EXISTING wrapper script in `/tmp/*.sh`, run via Bash tool.
DO NOT (1) create file with cat, (2) run `bash` from Bash tool

Integration tests use `plib::testing::TestPlan`. Test logic in `$crate/tests/$category/mod.rs`.

## Core Principles

1. **POSIX.2024 first** - spec before GNU/BSD extensions
2. **Minimalism** - only widely-used extensions
3. **Race-free** - especially file ops (see `ftw/`)
4. **Correctness > Readability > Performance**
