use std::process::Output;

use plib::testing::{run_test_with_checker, TestPlan};

mod basic;
#[cfg(target_os = "linux")]
mod tcp;
#[cfg(target_os = "linux")]
mod udp;
#[cfg(target_os = "linux")]
mod unix;
mod with_user;

pub fn fuser_test(
    args: Vec<String>,
    expected_err: &str,
    expected_exit_code: i32,
    checker: impl FnMut(&TestPlan, &Output),
) {
    run_test_with_checker(
        TestPlan {
            cmd: "fuser".to_string(),
            args,
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: expected_err.to_string(),
            expected_exit_code,
        },
        checker,
    );
}
