//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::fs;

use plib::{run_test, TestPlan};
use posixutils_make::error_code::ErrorCode;

fn run_test_helper(
    args: &[&str],
    expected_output: &str,
    expected_error: &str,
    expected_exit_code: i32,
) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test(TestPlan {
        cmd: String::from("make"),
        args: str_args,
        stdin_data: String::new(),
        expected_out: String::from(expected_output),
        expected_err: String::from(expected_error),
        expected_exit_code,
    });
}

fn run_test_with_stdin_helper(
    args: &[&str],
    stdin_data: &str,
    expected_output: &str,
    expected_error: &str,
    expected_exit_code: i32,
) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test(TestPlan {
        cmd: String::from("make"),
        args: str_args,
        stdin_data: String::from(stdin_data),
        expected_out: String::from(expected_output),
        expected_err: String::from(expected_error),
        expected_exit_code,
    });
}

fn run_test_helper_with_setup_and_destruct(
    args: &[&str],
    expected_output: &str,
    expected_error: &str,
    expected_exit_code: i32,
    setup: impl FnOnce(),
    destruct: impl FnOnce(),
) {
    setup();
    run_test_helper(args, expected_output, expected_error, expected_exit_code);
    destruct();
}

mod arguments {
    use super::*;

    #[test]
    fn dash_cap_c() {
        run_test_helper(
            &["-C", "tests/makefiles/arguments/dash_cap_c"],
            "cat works.txt\nChanged directory\n",
            "",
            0,
        )
    }

    #[test]
    fn dash_f() {
        run_test_helper(
            &["-f", "tests/makefiles/arguments/dash_f.mk"],
            "echo \"Changed makefile\"\nChanged makefile\n",
            "",
            0,
        )
    }

    #[test]
    fn dash_i() {
        run_test_helper(
            &["-if", "tests/makefiles/arguments/dash_i.mk"],
            "exit 1\necho Ignored\nIgnored\n",
            "",
            0,
        )
    }

    #[test]
    fn dash_n() {
        run_test_helper(
            &["-nf", "tests/makefiles/arguments/dash_n.mk"],
            "exit 1\n",
            "",
            0,
        );
    }

    #[test]
    fn dash_s() {
        run_test_helper(
            &["-sf", "tests/makefiles/arguments/dash_s.mk"],
            "Silent\n",
            "",
            0,
        )
    }

    #[test]
    fn dash_t() {
        let remove_touches = || {
            let dir = "tests/makefiles/arguments/dash_t";
            for i in 1..=2 {
                let _ = fs::remove_file(format!("{dir}/rule{i}"));
            }
        };

        run_test_helper_with_setup_and_destruct(
            &["-tC", "tests/makefiles/arguments/dash_t/"],
            "touch rule2\ntouch rule1\n",
            "",
            0,
            remove_touches,
            remove_touches,
        )
    }
}

// such tests should be moved directly to the package responsible for parsing makefiles
mod parsing {
    use super::*;

    #[test]
    fn empty() {
        run_test_helper(
            &["-f", "tests/makefiles/parsing/empty.mk"],
            "",
            "make: parse error: unexpected token None\n\n",
            ErrorCode::ParseError("the inner value does not matter for now".into()).into(),
        );
    }

    #[test]
    fn comments() {
        run_test_helper(
            &["-sf", "tests/makefiles/parsing/comments.mk"],
            "This program should not produce any errors.\n",
            "",
            0,
        );
    }
}

mod io {
    use std::io;

    use super::*;

    #[test]
    fn file_not_found() {
        run_test_helper(
            &["-f", "tests/makefiles/does_not_exist.mk"],
            "",
            "make: io error: entity not found\n",
            ErrorCode::IoError(io::ErrorKind::NotFound).into(),
        );
    }

    #[test]
    fn stdin() {
        run_test_with_stdin_helper(
            &["-sf", "-"],
            "rule:\n\techo executed\n",
            "executed\n",
            "",
            0,
        )
    }
}

mod macros {
    use super::*;

    #[test]
    fn substitutes_in_recipes() {
        run_test_helper(
            &["-sf", "tests/makefiles/macros/substitutes_in_recipes.mk"],
            "Macros substitution works.\n",
            "",
            0,
        );
    }
}

mod target_behavior {
    use super::*;

    #[test]
    fn no_targets() {
        run_test_helper(
            &["-f", "tests/makefiles/target_behavior/no_targets.mk"],
            "",
            "make: no targets to execute\n",
            ErrorCode::NoTarget { target: None }.into(),
        );
    }

    #[test]
    fn makefile_priority() {
        run_test_helper(
            &[
                "-sC",
                "tests/makefiles/target_behavior/makefile_priority/little_makefile",
            ],
            "makefile\n",
            "",
            0,
        );

        run_test_helper(
            &[
                "-sC",
                "tests/makefiles/target_behavior/makefile_priority/big_Makefile",
            ],
            "Makefile\n",
            "",
            0,
        );
    }

    #[test]
    fn basic_chaining() {
        run_test_helper(
            &["-sf", "tests/makefiles/target_behavior/basic_chaining.mk"],
            "rule2\nrule1\n",
            "",
            0,
        );
    }

    #[test]
    fn diamond_chaining_with_touches() {
        let remove_touches = || {
            let dir = "tests/makefiles/target_behavior/diamond_chaining_with_touches";
            for i in 1..=4 {
                let _ = fs::remove_file(format!("{}/rule{}", dir, i));
            }
        };

        run_test_helper_with_setup_and_destruct(
            &[
                "-sC",
                "tests/makefiles/target_behavior/diamond_chaining_with_touches",
            ],
            "rule4\nrule2\nrule3\nrule1\n",
            "",
            0,
            remove_touches,
            remove_touches,
        );
    }

    #[test]
    fn recursive_chaining() {
        run_test_helper(
            &[
                "-sf",
                "tests/makefiles/target_behavior/recursive_chaining.mk",
            ],
            "",
            "make: recursive prerequisite found trying to build 'rule1'\n",
            ErrorCode::RecursivePrerequisite {
                origin: "rule1".into(),
            }
            .into(),
        );
    }
}

mod recipes {
    use super::*;

    mod prefixes {
        use super::*;

        #[test]
        fn ignore() {
            run_test_helper(
                &["-f", "tests/makefiles/recipes/prefixes/ignore.mk"],
                "exit 1\necho ignored\nignored\n",
                "",
                0,
            );
        }

        #[test]
        fn silent() {
            run_test_helper(
                &["-f", "tests/makefiles/recipes/prefixes/silent.mk"],
                "silent\n",
                "",
                0,
            );
        }

        mod force_run {
            use super::*;

            #[test]
            fn with_dry_run() {
                run_test_helper(
                    &[
                        "-snf",
                        "tests/makefiles/recipes/prefixes/force_run/with_dry_run.mk",
                    ],
                    "I am NOT skipped\n",
                    "",
                    0,
                );
            }

            #[test]
            fn with_touch() {
                let remove_touches = || {
                    let _ = fs::remove_file(
                        "tests/makefiles/recipes/prefixes/force_run/with_touch/rule",
                    );
                };

                run_test_helper_with_setup_and_destruct(
                    &[
                        "-stC",
                        "tests/makefiles/recipes/prefixes/force_run/with_touch",
                    ],
                    "I am NOT skipped\n",
                    "",
                    0,
                    remove_touches,
                    remove_touches,
                );
            }
        }

        #[test]
        fn multiple() {
            run_test_helper(
                &["-f", "tests/makefiles/recipes/prefixes/multiple.mk"],
                "ignored\n",
                "",
                0,
            );
        }
    }
}

mod special_targets {
    use posixutils_make::special_target;

    use super::*;

    #[test]
    fn default() {
        run_test_helper(
            &[
                "-f",
                "tests/makefiles/special_targets/default.mk",
                "nonexisting_target",
            ],
            "echo Default\nDefault\n",
            "",
            0,
        );
    }

    #[test]
    fn ignore() {
        run_test_helper(
            &["-f", "tests/makefiles/special_targets/ignore.mk"],
            "exit 1\necho \"Ignored\"\nIgnored\n",
            "",
            0,
        );
    }

    #[test]
    fn silent() {
        run_test_helper(
            &["-f", "tests/makefiles/special_targets/silent.mk"],
            "I'm silent\n",
            "",
            0,
        );
    }

    mod validations {
        use super::*;

        #[test]
        fn without_prerequisites() {
            run_test_helper(
                &["-f", "tests/makefiles/special_targets/validations/without_prerequisites.mk"],
                "",
                "make: '.DEFAULT' special target constraint is not fulfilled: the special target must not have prerequisites\n",
                ErrorCode::SpecialTargetConstraintNotFulfilled {
                    target: String::default(),
                    constraint: special_target::Error::MustNotHavePrerequisites,
                }
                .into(),
            );
        }

        #[test]
        fn without_recipes() {
            run_test_helper(
                &["-f", "tests/makefiles/special_targets/validations/without_recipes.mk"],
                "",
                "make: '.SILENT' special target constraint is not fulfilled: the special target must not have recipes\n",
                ErrorCode::SpecialTargetConstraintNotFulfilled {
                    target: String::default(),
                    constraint: special_target::Error::MustNotHaveRecipes,
                }
                .into(),
            );
        }
    }

    mod modifiers {
        use super::*;

        #[test]
        fn additive() {
            run_test_helper(
                &[
                    "-f",
                    "tests/makefiles/special_targets/modifiers/additive.mk",
                ],
                "I'm silent\nMe too\n",
                "",
                0,
            );
        }

        #[test]
        fn global() {
            run_test_helper(
                &["-f", "tests/makefiles/special_targets/modifiers/global.mk"],
                "I'm silent\n",
                "",
                0,
            );
        }
    }

    mod behavior {
        use super::*;

        #[test]
        fn ignores_special_targets_as_first_target() {
            run_test_helper(
                &[
                    "-f",
                    "tests/makefiles/special_targets/behavior/ignores_special_targets_as_first_target.mk",
                ],
                "I'm silent\n",
                "",
                0,
            );
        }
    }
}
