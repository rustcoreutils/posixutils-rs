# Copilot Instructions for posixutils-rs

## Project Overview

posixutils-rs is a suite of Rust-native core command line utilities (cp, mv, awk, make, vi, etc.) using POSIX.2024 as the baseline specification. The goal is to create clean, race-free userland utilities that are POSIX compliant, maximizing compatibility with existing shell scripts while minimizing bloat.

## Core Principles

1. **POSIX Compliance First**: The primary specification is POSIX.2024. Implement features according to POSIX specification before considering GNU/BSD extensions.
2. **Minimalism**: Avoid bloat. Only implement widely-used GNU/BSD extensions, not obscure rarely-used options.
3. **Clean Rust Code**: Each utility should look like a standard Rust CLI program with minimal dependencies.
4. **Race-Free**: Design utilities to be race-free in concurrent environments.

## Development Guidelines

### Code Style

1. **Always run `cargo fmt`** - This is required and enforced in CI.
2. **No warnings**: Code must compile without warnings. Never use `#[allow(dead_code)]`.
   - If code is unused, delete it
   - If code is only used in tests, use `#[cfg(test)]`
3. **Readability over cleverness**: Code should be readable by unfamiliar developers. Avoid dense, uncommented code.
4. **Write small functions**: Break up large functions. The compiler can inline as needed.
5. **Priority order**: Correctness, readability, performance (in that order).

### Dependencies

1. **Prefer "std only"**: Minimize external crate dependencies.
2. **Use tiny crates**: When external crates are needed, prefer tiny, single-purpose crates like `tempfile`.
3. **Avoid mega-crates**: Do not introduce large dependency trees.
4. **Standard dependencies**: The project commonly uses:
   - `clap` for command-line parsing
   - `libc` for system calls
   - `regex` for pattern matching
   - `chrono` for date/time handling
   - `plib` (internal) for common utilities

### Testing

1. **Test pattern**:
   - Pre-generate input data files
   - Run system OS utility on input data, generating known-good output
   - Store input and output in-tree as known-good data
   - Write TestPlan that executes our utility using static input/output data
2. **Use plib's TestPlan framework** for integration tests.
3. **Quick tests only in `cargo test`**: Longer tests or tests requiring root should use feature flags like `posixutils_test_all` or `requires_root`.
4. **All commits should pass tests**: Keep `git bisect` working.

### Building and Testing Commands

- **Build**: `cargo build --release`
- **Test all** (long, 15+ minutes): `cargo test --release`
- **Test one module**: `cargo test --release -p posixutils-SOMECRATE`
- **Test with all features**: `cargo test --release --features posixutils_test_all`
- **Format check**: `cargo fmt --all -- --check`

### Project Structure

- Each utility is typically in its own crate under a category directory (e.g., `text/`, `fs/`, `process/`)
- `plib/` contains shared library code and the TestPlan framework
- `ftw/` contains race-free file tree walking functionality
- Integration test harness should ONLY contain `mod` statements
- Test logic goes in `$module/tests/$category/mod.rs` files

### Utility Lifecycle Stages

When working on utilities, be aware of their maturity stage:
1. **Rough draft**: Core algorithm implemented, bugs may exist
2. **Feature complete**: Complete per POSIX specification
3. **Test coverage**: Integration tests complete and passing 100%
4. **Code coverage**: 100% automated code coverage
5. **Translated**: All strings internationalized
6. **Audited**: Externally reviewed for correctness and security

### Commit Guidelines

1. Separate logical changes into separate commits (bug fixes vs. new features)
2. All commits should pass tests
3. All PRs must pass tests before merging
4. All code must be **copyright clean**: freshly written or from compatible open source licenses

### Error Handling and Internationalization

- Support internationalization for error messages
- Use `gettext-rs` for i18n when appropriate
- Handle common OS errors with user-friendly messages

### Platform Support

- Primary targets: Linux and macOS
- The project should work on Unix-like systems
- Consider platform-specific features when necessary, but keep code portable

## What NOT to Do

1. **Don't add GNU-specific bloat**: Only add GNU features that users cannot live without.
2. **Don't break POSIX compliance**: Always verify changes maintain POSIX compliance.
3. **Don't introduce race conditions**: Use race-free patterns, especially with file operations.
4. **Don't add commented-out code**: Delete unused code instead.
5. **Don't add dependencies casually**: Each new dependency should be justified and minimal.

## When Implementing New Features

1. Check the POSIX.2024 specification first
2. Review the existing utility's stage of maturity
3. Follow the code style of similar utilities in the project
4. Add integration tests following the TestPlan pattern
5. Ensure `cargo fmt` and `cargo test --release` pass
6. Consider whether the feature aligns with project minimalism goals

## References

- POSIX specification: https://pubs.opengroup.org/onlinepubs/9699919799/
- Minimum Rust version: 1.85.0
- License: MIT
