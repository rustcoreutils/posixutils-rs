# Integration Tests

These test files are consumed by [build.rs](../../build.rs) and used to generate [integration_tests.rs](../../tests/integration_test.rs).

A test input can either be a `.m4` file or a `.args` file. `.m4` files are processed using `m4` as a library (this allows capturing debug logs without them interferring with stderr or stdout used in the test), wheras `.args` files are processed using the command line interface and is used for testing features of the CLI.

A snapshot from the test run with the system `m4`  is generated using the [test-manager](../../test-manager/) (these snapshots have been generated using `GNU m4`) and stored in `.out` files, to be compared with during tests. This approach was chosen because we would ideally like to match some subset of compatibility with the most widely used implementation. You can generate new snapshots by running the following command from the [m4 directory](../../):

```bash
cargo run -p m4-test-manager -- update-snapshots
```

`.out` files also serve as a way to configure the test. See the [TestSnapshot struct in m4-test-manager lib.rs](../../test-manager/src/lib.rs) for descriptions of the various options which are available.