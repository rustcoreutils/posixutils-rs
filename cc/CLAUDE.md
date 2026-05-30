# cc (pcc) Development Rules

## Testing Requirements

1. **All changes must have tests** — every fix and feature needs accompanying tests to prevent regressions.
2. **Changes to `cc/ir/`, `cc/token/`, `cc/parse/` MUST include unit tests.**
3. **All changes must also have e2e integration tests** in `cc/tests/` to ensure full coverage.

## Determinism Invariant

pcc must produce **bit-identical** assembly and object output on identical
input across runs. Reproducible builds depend on this; subtle "works on my
machine" bugs (where two valid orderings happen to both work locally but
collide somewhere else) are prevented by it; and binary diffs across
compiler changes are only meaningful when the baseline is reproducible.

The single biggest threat to this invariant is `HashMap` / `HashSet`
iteration order. Rust's default `RandomState` seeds a fresh hash per
process, so any compiler pass that iterates a hash-based container in a
way that affects output will produce different output on each run.

### Container selection rule

- **Maps/sets keyed by IR identifiers** (`PseudoId`, `BasicBlockId`,
  `TypeId`, `SymbolId`, `StringId`) — use `BTreeMap` / `BTreeSet`.
  These keys derive `Ord` (insertion-order-stable u32 newtypes); BTree
  iteration is naturally deterministic and a small constant slower than
  `HashMap` for the workloads we hit.
- **Maps keyed by `String`** that get iterated for ID allocation,
  emission, or any output-affecting purpose — use `BTreeMap<String,V>`
  or sort the keys before iteration.
- **`HashMap` / `HashSet` are reserved for pure-lookup workloads**:
  `.get()`, `.contains_key()`, `.insert()` where iteration is never
  used, or only used for order-independent operations (sum, count,
  building a set whose iteration order doesn't propagate to output).

When in doubt: **prefer BTree**. The performance gap is negligible for
compiler-internal maps that hold a few hundred to a few thousand
entries; the determinism guarantee is structural rather than relying on
reviewer attention.

### Regression test

`cc/tests/codegen/determinism.rs` compiles representative C programs
twice and asserts byte-identical assembly. New passes that touch
iteration of hash-based containers should add a test that exercises the
specific code path; existing tests will catch most accidental
regressions even without a targeted test.
