//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::env;
use std::fs::{remove_file, File};
use std::io::Write;
use std::process::{Child, Command, Stdio};

use plib::testing::{run_test, run_test_base, TestPlan};

use posixutils_make::error_code::ErrorCode;

pub fn run_test_not_comparing_error_message(plan: TestPlan) {
    let output = run_test_base(&plan.cmd, &plan.args, plan.stdin_data.as_bytes());

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(stdout, plan.expected_out);

    assert_eq!(output.status.code(), Some(plan.expected_exit_code));
    if plan.expected_exit_code == 0 {
        assert!(output.status.success());
    }
}

fn run_test_helper_without_error_message(
    args: &[&str],
    expected_output: &str,
    expected_exit_code: i32,
) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test_not_comparing_error_message(TestPlan {
        cmd: String::from("make"),
        args: str_args,
        stdin_data: String::new(),
        expected_out: String::from(expected_output),
        expected_err: String::new(),
        expected_exit_code,
    });
}

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

fn manual_test_helper(args: &[&str]) -> Child {
    // Determine the binary path based on the build profile
    let relpath = if cfg!(debug_assertions) {
        format!("target/debug/{}", "make")
    } else {
        format!("target/release/{}", "make")
    };

    // Build the full path to the binary
    let test_bin_path = env::current_dir()
        .expect("failed to get current directory")
        .parent()
        .expect("failed to get parent directory")
        .join(relpath);

    // Create and spawn the command
    Command::new(test_bin_path)
        .args(args)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("failed to spawn command")
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
    fn dash_cap_s() {
        run_test_helper(
            &["-SC", "tests/makefiles/arguments/dash_cap_s"],
            "OK\n",
            "make: execution error: 1\n",
            2,
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
    fn dash_p() {
        run_test_helper(
            &["-p"],
            "{\".MACROS\": {\"AR=ar\", \"ARFLAGS=-rv\", \"CC=c17\", \"CFLAGS=-O 1\", \"GFLAGS=\", \"LDFLAGS=\", \"LEX=lex\", \"LFLAGS=\", \"SCCSFLAGS=\", \"SCCSGETFLAGS=-s\", \"XSI GET=get\", \"YACC=yacc\", \"YFLAGS=\"}, \".SCCS_GET\": {\"sccs $(SCCSFLAGS) get $(SCCSGETFLAGS) $@\"}, \".SUFFIXES\": {\".a\", \".c\", \".c~\", \".l\", \".l~\", \".o\", \".sh\", \".sh~\", \".y\", \".y~\"}, \"SUFFIX RULES\": {\".c.a: $(CC) -c $(CFLAGS) $<; $(AR) $(ARFLAGS) $@ $*.o; rm -f $*.o\", \".c.o: $(CC) $(CFLAGS) -c $<\", \".c: $(CC) $(CFLAGS) $(LDFLAGS) -o $@ $<\", \".l.c: $(LEX) $(LFLAGS) $<; mv lex.yy.c $@\", \".l.o: $(LEX) $(LFLAGS) $<; $(CC) $(CFLAGS) -c lex.yy.c; rm -f lex.yy.c; mv lex.yy.o $@\", \".l~.c: $(GET) $(GFLAGS) -p $< > $*.l; $(LEX) $(LFLAGS) $*.l; mv lex.yy.c $@\", \".l~.o: $(GET) $(GFLAGS) -p $< > $*.l; $(LEX) $(LFLAGS) $*.l; $(CC) $(CFLAGS) -c lex.yy.c; rm -f lex.yy.c; mv lex.yy.o $@\", \".sh: chmod a+x $@\", \".sh: cp $< $@\", \".y.c: $(YACC) $(YFLAGS) $<; mv y.tab.c $@\", \".y.o: $(YACC) $(YFLAGS) $<; $(CC) $(CFLAGS) -c y.tab.c; rm -f y.tab.c; mv y.tab.o $@\", \".y~.c: $(GET) $(GFLAGS) -p $< > $*.y; $(YACC) $(YFLAGS) $*.y; mv y.tab.c $@\", \".y~.o: $(GET) $(GFLAGS) -p $< > $*.y; $(YACC) $(YFLAGS) $*.y; $(CC) $(CFLAGS) -c y.tab.c; rm -f y.tab.c; mv y.tab.o $@\", \"XSI .c~.o: $(GET) $(GFLAGS) -p $< > $*.c; $(CC) $(CFLAGS) -c $*.c\"}}",
            "",
            0,
        )
    }
    #[test]
    fn dash_p_with_mk() {
        run_test_helper(
            &["-pf", "tests/makefiles/arguments/dash_p/with_phony.mk"],
            "{\".MACROS\": {\"AR=ar\", \"ARFLAGS=-rv\", \"CC=c17\", \"CFLAGS=-O 1\", \"GFLAGS=\", \"LDFLAGS=\", \"LEX=lex\", \"LFLAGS=\", \"SCCSFLAGS=\", \"SCCSGETFLAGS=-s\", \"XSI GET=get\", \"YACC=yacc\", \"YFLAGS=\"}, \".PHONY\": {\"clean\"}, \".SCCS_GET\": {\"sccs $(SCCSFLAGS) get $(SCCSGETFLAGS) $@\"}, \".SUFFIXES\": {\".a\", \".c\", \".c~\", \".l\", \".l~\", \".o\", \".sh\", \".sh~\", \".y\", \".y~\"}, \"SUFFIX RULES\": {\".c.a: $(CC) -c $(CFLAGS) $<; $(AR) $(ARFLAGS) $@ $*.o; rm -f $*.o\", \".c.o: $(CC) $(CFLAGS) -c $<\", \".c: $(CC) $(CFLAGS) $(LDFLAGS) -o $@ $<\", \".l.c: $(LEX) $(LFLAGS) $<; mv lex.yy.c $@\", \".l.o: $(LEX) $(LFLAGS) $<; $(CC) $(CFLAGS) -c lex.yy.c; rm -f lex.yy.c; mv lex.yy.o $@\", \".l~.c: $(GET) $(GFLAGS) -p $< > $*.l; $(LEX) $(LFLAGS) $*.l; mv lex.yy.c $@\", \".l~.o: $(GET) $(GFLAGS) -p $< > $*.l; $(LEX) $(LFLAGS) $*.l; $(CC) $(CFLAGS) -c lex.yy.c; rm -f lex.yy.c; mv lex.yy.o $@\", \".sh: chmod a+x $@\", \".sh: cp $< $@\", \".y.c: $(YACC) $(YFLAGS) $<; mv y.tab.c $@\", \".y.o: $(YACC) $(YFLAGS) $<; $(CC) $(CFLAGS) -c y.tab.c; rm -f y.tab.c; mv y.tab.o $@\", \"some\n.y~.c: $(GET) $(GFLAGS) -p $< > $*.y; $(YACC) $(YFLAGS) $*.y; mv y.tab.c $@\", \".y~.o: $(GET) $(GFLAGS) -p $< > $*.y; $(YACC) $(YFLAGS) $*.y; $(CC) $(CFLAGS) -c y.tab.c; rm -f y.tab.c; mv y.tab.o $@\", \"XSI .c~.o: $(GET) $(GFLAGS) -p $< > $*.c; $(CC) $(CFLAGS) -c $*.c\"}}",
            "",
            0,
        )
    }

    #[test]
    fn dash_r() {
        run_test_helper(
            &["-r"],
            "",
            "make: no makefile\n",
            ErrorCode::NoMakefile.into(),
        )
    }

    #[test]
    fn dash_r_with_file() {
        run_test_helper_with_setup_and_destruct(
            &["-rf", "tests/makefiles/arguments/dash_r/with_file.mk"],
            "Converting testfile.txt to testfile.out\n",
            "",
            0,
            || {
                File::create("testfile.txt").expect("failed to create file");
            },
            || {
                remove_file("testfile.txt").expect("failed to remove file");
                remove_file("testfile.out").expect("failed to remove file");
            },
        );
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
                let _ = remove_file(format!("{dir}/rule{i}"));
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

    #[test]
    fn dash_q() {
        run_test_helper(
            &["-qf", "tests/makefiles/arguments/dash_q/cc_target.mk"],
            "",
            "",
            1,
        );
    }
    #[test]
    fn dash_k() {
        run_test_helper(
            &["-kf", "tests/makefiles/arguments/dash_k.mk"],
            "OK\necho 12\n12\n",
            "make: execution error: 1\nmake: Target z not remade because of errors\n",
            2,
        );
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
            "make: parse error:  *** No targets. Stop.\n\n",
            4,
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

    // #[test]
    // #[ignore]
    // fn suffixes_with_no_target() {
    //     run_test_helper(
    //         &["-f", "tests/makefiles/parsing/suffixes_with_no_targets.mk"],
    //         "",
    //         "make: parse error: No Targets",
    //         ErrorCode::ParseError("no targets".into()).into(),
    //     );
    // }
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
    use std::env;

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

    #[test]
    fn envs_in_recipes() {
        run_test_helper_with_setup_and_destruct(
            &["-esf", "tests/makefiles/macros/envs_in_recipes.mk"],
            "macro is replaced succesfully\n",
            "",
            0,
            set_env_vars,
            clean_env_vars,
        );

        fn set_env_vars() {
            env::set_var("MACRO", "echo");
        }

        fn clean_env_vars() {
            env::remove_var("MACRO");
        }
    }
}

mod target_behavior {
    use super::*;
    use libc::{kill, SIGINT};

    use std::{thread, time::Duration};

    #[test]
    fn no_targets() {
        run_test_helper(
            &["-f", "tests/makefiles/target_behavior/no_targets.mk"],
            "",
            "make: no targets to execute\n",
            6,
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
            for i in 1..=5 {
                let _ = remove_file(format!("{}/rule{}", dir, i));
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

    #[test]
    fn async_events() {
        let args = [
            "-f",
            "tests/makefiles/target_behavior/async_events/signal.mk",
        ];
        let child = manual_test_helper(&args);
        let pid = child.id() as i32;

        thread::spawn(move || {
            thread::sleep(Duration::from_millis(100));
            unsafe {
                kill(pid, SIGINT);
            }
        });

        let output = child.wait_with_output().expect("failed to wait for child");

        let stdout = String::from_utf8_lossy(&output.stdout);
        assert_eq!(stdout, "echo \"hello\"\nhello\ntouch text.txt\nsleep 1\n");

        let stderr = String::from_utf8_lossy(&output.stderr);
        assert_eq!(stderr, "make: Interrupt\nmake: Deleting file 'text.txt'\n");

        assert_eq!(output.status.code(), Some(130));
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
                    let _ =
                        remove_file("tests/makefiles/recipes/prefixes/force_run/with_touch/rule");
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
            #[test]
            fn with_dash_q() {
                run_test_helper(
                    &[
                        "-sqf",
                        "tests/makefiles/recipes/prefixes/force_run/with_dry_run.mk",
                    ],
                    "I am NOT skipped\n",
                    "",
                    0,
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
    use super::*;
    use libc::{kill, SIGINT};
    use posixutils_make::special_target;
    use std::fs::remove_dir;
    use std::{fs, thread, time::Duration};

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

    #[test]
    fn phony() {
        run_test_helper_without_error_message(
            &["-f", "tests/makefiles/special_targets/phony/phony_basic.mk"],
            "rm temp\n",
            2,
        );
    }

    #[test]
    fn sccs_get() {
        run_test_helper(
            &["-pf", "tests/makefiles/special_targets/sccs/basic_sccs.mk"],
            "{\".MACROS\": {\"AR=ar\", \"ARFLAGS=-rv\", \"CC=c17\", \"CFLAGS=-O 1\", \"GFLAGS=\", \"LDFLAGS=\", \"LEX=lex\", \"LFLAGS=\", \"SCCSFLAGS=\", \"SCCSGETFLAGS=-s\", \"XSI GET=get\", \"YACC=yacc\", \"YFLAGS=\"}, \".SCCS_GET\": {\"echo \\\"executing command\\\"\"}, \".SUFFIXES\": {\".a\", \".c\", \".c~\", \".l\", \".l~\", \".o\", \".sh\", \".sh~\", \".y\", \".y~\"}, \"SUFFIX RULES\": {\".c.a: $(CC) -c $(CFLAGS) $<; $(AR) $(ARFLAGS) $@ $*.o; rm -f $*.o\", \".c.o: $(CC) $(CFLAGS) -c $<\", \".c: $(CC) $(CFLAGS) $(LDFLAGS) -o $@ $<\", \".l.c: $(LEX) $(LFLAGS) $<; mv lex.yy.c $@\", \".l.o: $(LEX) $(LFLAGS) $<; $(CC) $(CFLAGS) -c lex.yy.c; rm -f lex.yy.c; mv lex.yy.o $@\", \".l~.c: $(GET) $(GFLAGS) -p $< > $*.l; $(LEX) $(LFLAGS) $*.l; mv lex.yy.c $@\", \".l~.o: $(GET) $(GFLAGS) -p $< > $*.l; $(LEX) $(LFLAGS) $*.l; $(CC) $(CFLAGS) -c lex.yy.c; rm -f lex.yy.c; mv lex.yy.o $@\", \".sh: chmod a+x $@\", \".sh: cp $< $@\", \".y.c: $(YACC) $(YFLAGS) $<; mv y.tab.c $@\", \".y.o: $(YACC) $(YFLAGS) $<; $(CC) $(CFLAGS) -c y.tab.c; rm -f y.tab.c; mv y.tab.o $@\", \"something\n.y~.c: $(GET) $(GFLAGS) -p $< > $*.y; $(YACC) $(YFLAGS) $*.y; mv y.tab.c $@\", \".y~.o: $(GET) $(GFLAGS) -p $< > $*.y; $(YACC) $(YFLAGS) $*.y; $(CC) $(CFLAGS) -c y.tab.c; rm -f y.tab.c; mv y.tab.o $@\", \"XSI .c~.o: $(GET) $(GFLAGS) -p $< > $*.c; $(CC) $(CFLAGS) -c $*.c\"}}",
            "",
            0,
        );
    }

    #[test]
    fn precious() {
        let args = [
            "-f",
            "tests/makefiles/special_targets/precious/basic_precious.mk",
        ];
        let child = manual_test_helper(&args);
        let pid = child.id() as i32;

        thread::spawn(move || {
            thread::sleep(Duration::from_millis(100));
            unsafe {
                kill(pid, SIGINT);
            }
        });

        let output = child.wait_with_output().expect("failed to wait for child");

        let stdout = String::from_utf8_lossy(&output.stdout);
        assert_eq!(
            stdout,
            "echo hello\nhello\nmkdir preciousdir\ntouch preciousdir/some.txt\nsleep 1\n"
        );
        assert!(fs::exists("preciousdir/some.txt").unwrap());

        let stderr = String::from_utf8_lossy(&output.stderr);
        assert_eq!(stderr, "make: Interrupt\n");

        assert_eq!(output.status.code(), Some(130));

        remove_file("preciousdir/some.txt").unwrap();
        remove_dir("preciousdir").unwrap();
    }

    #[test]
    fn suffixes() {
        run_test_helper_with_setup_and_destruct(
            &[
                "-f",
                "tests/makefiles/special_targets/suffixes/suffixes_basic.mk",
            ],
            "Converting copied.txt to copied.out\n",
            "",
            0,
            create_txt,
            remove_files,
        );

        fn create_txt() {
            let dir = env::current_dir().unwrap();
            for file in dir.read_dir().unwrap().map(Result::unwrap) {
                if !file.file_type().map(|x| x.is_file()).unwrap_or(false) {
                    continue;
                }
                if file.path().extension().map(|x| x == "txt").unwrap_or(false) {
                    remove_file(file.path()).unwrap();
                }
            }

            File::create("copied.txt")
                .unwrap()
                .write_all(b"some content")
                .unwrap();
        }

        fn remove_files() {
            remove_file("copied.txt").unwrap();
            remove_file("copied.out").unwrap();
        }
    }

    // unspecified stderr and error type, must be refactored and improved
    // #[test]
    // #[ignore]
    // fn clear_suffixes() {
    //     run_test_helper(
    //         &[
    //             "-f",
    //             "tests/makefiles/special_targets/suffixes/clear_suffixes.mk",
    //         ],
    //         "Converting $< to \n",
    //         "make: Nothing be dobe for copied.out",
    //         ErrorCode::ParseError("the inner value does not matter for now".into()).into(),
    //     );
    // }

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
