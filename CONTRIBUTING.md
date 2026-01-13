# Welcome to the Project :)

## How to Contribute: Issues, Not Pull Requests

**This project accepts contributions via GitHub Issues exclusively.** We do not accept Pull Requests.

### Why Issues Instead of PRs?

This project uses AI-assisted coding for development. In this workflow:

- **PRs lose context**: A pull request shows *what* changed, but not *why* or *how* decisions were made. The AI prompts, design discussions, and iterative refinements that produced the code are invisible.
- **PRs are hard to review**: AI-generated PRs tend to be large, touching many files, making thorough review impractical.
- **Issues preserve context**: A well-written issue captures the problem, expected behavior, examples, and constraints—everything both humans and AI agents need to produce a good solution.

### What Makes a Good Issue?

The more context you provide, the better the result. Include:

- **Problem description**: What's wrong, or what's missing?
- **Expected behavior**: What should happen?
- **Actual behavior**: What happens instead? (for bugs)
- **Example output**: Paste terminal output, error messages, stack traces
- **Reproduction steps**: Commands to reproduce the issue
- **System info**: Output of `uname -a`, Rust version, etc.
- **POSIX spec references**: Link to relevant POSIX documentation
- **Test cases**: Input files, expected output files
- **Feature rationale**: Why is this feature needed? What use cases?

Think of your issue as providing context for an AI coding agent. The richer the context, the better the implementation.

### Contribution Types

All contributions are welcome via issues:

- **Bug reports**: Describe the bug with reproduction steps and expected behavior
- **Feature requests**: Describe the feature and its use cases
- **POSIX compliance issues**: Reference the spec, show non-compliant behavior
- **Documentation improvements**: Describe what's unclear or missing
- **Test case suggestions**: Provide input/output examples for testing

---

There are several ways to contribute to posixutils-rs:

* coding
* writing tests
* writing documentation
* testing, especially POSIX compliance testing

### Quick start

Build: `cargo build --release`

Test all (long, more than 15 minutes on some hosts) `cargo test --release`

Test (one module): `cargo test --release -p posixutils-SOMECRATE`

### Utility lifecycle:  Stages of Maturity

1. Rough draft:  Core algorithm implemented.  Bugs may exist.  Many options not yet implemented.
2. Feature complete:  Believed to be complete per POSIX specification.
3. Test coverage:  Integration tests, positive and negative, are complete, pass 100%
4. Code coverage:  Automated code coverage data indicates 100%
5. Translated:  All strings are internationalized, including common OS errors for common error cases.
6. Audited:  An external party has reviewed and tested for correctness, 
   POSIX compliance, security, races and similar issues.

### For Maintainers: Commit Standards

1. Separate logical changes into separate commits.  For example, bug fixes
   and new features should be separate commits.
2. All commits should pass tests.  This keeps `git bisect` working.
3. All code must be **copyright clean**:  either freshly written,
   or copied from source code with a compatible open source license.

### CLI utility and Rust style guidelines

1. `cargo fmt` is required.
2. Ideal goal:  **Each utility should look like a standard Rust CLI program.** 
   Small, lightweight utility with command line processing,
   core algorithm, and zero external crate dependencies.
3. "only std"  When an external crate is required, avoid mega-crates.  Prefer
   std-only, or, tiny crates such as `tempfile` that perform a single,
   lightweight function.
4. Correctness, readability, performance, in that order.
   Code should be readable by unfamiliar developers.  Avoid dense,
   uncommented code.
5. Write small functions. Break up large functions.
   The compiler can inline as needed.
6. **No `#[allow(dead_code)]` markers.**  Code should compile without warnings.
   If code is unused, delete it.  If code is only used in tests, use `#[cfg(test)]`.

### Testing, POSIX compliance and programmatic goals

* POSIX compliance
* Full integration test coverage
* Support widely used GNU/BSD extensions
* Race-free userland.  (See `ftw` internal crate.)
* Push small crates out:  Create tiny, light-dep crates from common
  functionality (such as Lex or Yacc file parsing), and publish via cargo.
  Remove from main posixutils tree and set of crates.
* If a system has an OS-specific feature that _must_ be
  exposed through a given utility, do so.

### Testing and Bug Reporting: Info to provide in GH issue

* Include information about your system (`uname -a`) in every issue.
* Provide any input data can that be used to reproduce the bug.
* Provide any error output from the utility.
* Describe expected results:  What did you expect to happen, and did not?

### Writing tests

* Test pattern:
	1. Pre-generate input data files.
	2. Run system OS utility on input data files,
	   generating known-good output.
	3. Store input and output in-tree, as known-good data.
	4. Write a TestPlan that executes our utility, using
	   static input data, and compares output with
	   static output data (OS reference data).
* Use plib's TestPlan framework for integration tests.
* Integration test harness should ONLY contain `mod` statements.
  Test logic is in $module/tests/$category/mod.rs files.
* Only "quick" tests should be run automatically in `cargo test`
* Longer tests, or tests requiring root access, should be triggered
  via special environment variables.

