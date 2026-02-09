# cc (pcc) Development Rules

## Testing Requirements

1. **All changes must have tests** — every fix and feature needs accompanying tests to prevent regressions.
2. **Changes to `cc/ir/`, `cc/token/`, `cc/parse/` MUST include unit tests.**
3. **All changes must also have e2e integration tests** in `cc/tests/` to ensure full coverage.
