# cc (c17) Development Rules

## Testing Requirements

1. **All changes must have tests** — every fix and feature needs accompanying tests to prevent regressions.
2. **Changes to `cc/ir/`, `cc/token/`, `cc/parse/` MUST include unit tests.**
3. **All changes must also have e2e integration tests** in `cc/tests/` to ensure full coverage.
4. **Run the suite alone.** CI runs it with `--test-threads=1`; locally the
   plain `cargo test --release -p posixutils-cc` is reliable, but two full
   runs at once starve the heavy tests into failures that are not real.
