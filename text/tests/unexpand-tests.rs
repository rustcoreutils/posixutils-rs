//
// Copyright (c) 2024 Jeff Garzik
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::{run_test, TestPlan};

fn unexpand_test(args: &[&str], test_data: &str, expected_output: &str) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test(TestPlan {
        cmd: String::from("unexpand"),
        args: str_args,
        stdin_data: String::from(test_data),
        expected_out: String::from(expected_output),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

#[cfg(test)]
mod unexpand_tests {
    use crate::unexpand_test;

    #[test]
    fn unexpand_test_1() {
        unexpand_test(
            &["-t", "4,8,12"],
            "    Apple\n        Banana\n            Cherry\n                Date",
            "\tApple\n\t\tBanana\n\t\t\tCherry\n\t\t\t    Date\n",
        );
    }

    #[test]
    fn unexpand_test_2() {
        unexpand_test(
            &["-"],
            "    Apple\n        Banana\n            Cherry\n                Date",
            "    Apple\n\tBanana\n\t    Cherry\n\t        Date\n",
        );
    }

    #[test]
    fn unexpand_test_3() {
        unexpand_test(
            &["-t", "8"],
            "        leading spaces\n",
            "\tleading spaces\n",
        );
    }

    #[test]
    fn unexpand_test_4() {
        unexpand_test(&["-t", "4"], "    leading spaces\n", "\tleading spaces\n");
    }

    #[test]
    fn unexpand_test_5() {
        unexpand_test(
            &["-t", "8"],
            "text    with spaces\n",
            "text    with spaces\n",
        );
    }

    #[test]
    fn unexpand_test_6() {
        unexpand_test(
            &["-a"],
            "text        with                spaces",
            "text\twith\t\tspaces\n",
        );
    }
}
