# Welcome to the Project :)

There are several ways to contribute to posixutils-rs:

* coding
* writing tests
* writing documentation
* testing, especially POSIX compliance testing

### Utility lifecycle:  Stages of Maturity

1. Rough draft:  Core algorithm implemented.  Bugs may exist.  Many options not yet implemented.
2. Feature complete:  Believed to be complete per POSIX specification.
3. Test coverage:  Integration tests, positive and negative, are complete, pass 100%
4. Code coverage:  Automated code coverage data indicates 100%
5. Translated:  All strings are internationalized, including common OS errors for common error cases.
6. Audited:  An external party has reviewed and tested for correctness, 
   POSIX compliance, security, races and similar issues.

### Coding considerations

1. Separate logical changes into separate commits.  For example, bug fixes
   and new features should be separate commits.
2. All commits should pass tests.  This keeps `git bisect` working.
3. All PRs must pass tests before merging.
4. All code contributions must be **copyright clean**:  either freshly written,
   or copied from source code with a compatible open source license.

### CLI utility and Rust style guidelines

1. `cargo fmt` is required.
2. Ideal goal:  **Each utility should look like a standard Rust CLI program.** 
   Small, lightweight utility with command line processing,
   core algorithm, and zero external crate dependencies.
3. "only std"  When an external crate is required, avoid mega-crates.  Prefer
   std-only, or, tiny crates such as `atty` that perform a single,
   lightweight function.
4. Correctness, readability, performance, in that order.
   Code should be readable by unfamiliar developers.  Avoid dense,
   uncommented code.

### Testing, POSIX compliance and programmatic goals

* All utilities should have tests.
* Use plib's TestPlan framework for integration tests.
* Test pattern:
	1. Pre-generate input data files.
	2. Run system OS utility on input data files,
	   generating known-good output.
	3. Store input and output in-tree, as known-good data.
	4. Write a TestPlan that executes our utility, using
	   static input data, and compares output with
	   static output data.
* Only "quick" tests should be run automatically in `cargo test`
* Longer tests, or tests requiring root access, should be triggered
  via special environment variables.
* POSIX compliance
* Support the most widely used GNU/BSD extensions
* If a system has an OS-specific feature that _must_ be
  exposed through a given utility, do so.
* Race-free userland.  See `ftw` internal crate.
* Push small crates out:  Create tiny, light-dep crates from common
  functionality (such as Lex or Yacc file parsing), and publish via cargo.
  Remove from main posixutils tree and set of crates.

### Testing and Bug Reporting: Info to provide in GH issue

* Include information about your system (`uname -a`) in every issue.
* Provide any input data can that be used to reproduce the bug.
* Provide any error output from the utility.
* Describe expected results:  What did you expect to happen, and did not?

