# Welcome to the Project :)

There are several ways to contribute to posixutils-rs:

* coding
* writing tests
* writing documentation
* testing, especially POSIX compliance testing

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
3. When an external crate is required, avoid mega-crates.  Prefer tiny
   crates that perform a single, lightweight function.
4. Correctness, readability, performance, in that order.
   Code should be readable by unfamiliar developers.
   Avoid dense, uncommented code.

### Testing and POSIX compliance goals

* All utilities should have tests.
* Only "quick" tests should be run automatically in `cargo test`
* Goal #1: POSIX compliance
* Goal #2: Support the most popular Linux/BSD extesnsions

### Testing and Bug Reporting

* Include information about your system (`uname -a`) in every issue.
* Provide any input data can that be used to reproduce the bug.
* Provide any error output from the utility.
* Describe expected results:  What did you expect to happen, and did not?

