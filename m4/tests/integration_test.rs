//! NOTE: This file has been auto generated using build.rs, don't edit by hand!
use similar_asserts::assert_eq;

#[test]
fn test_evaluation_order() {
    let output = String::from_utf8(
        std::process::Command::new("cargo")
            .arg("run")
            .arg("--")
            .arg("fixtures/integration_tests/evaluation_order.m4")
            .output()
            .unwrap()
            .stdout,
    )
    .unwrap();

    let expected_output =
        std::fs::read_to_string("fixtures/integration_tests/evaluation_order.stdout").unwrap();
    assert_eq!(output, expected_output);
}
