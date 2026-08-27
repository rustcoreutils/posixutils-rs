//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::fs::{remove_file, File};
use std::io::Write;
use std::process::{Child, Command, Stdio};

use plib::testing::{get_binary_path, run_test, run_test_base, TestPlan};

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
    let test_bin_path = get_binary_path("make");

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
        let bin = get_binary_path("make");
        let output = Command::new(bin)
            .args(["-p", "-f", "/dev/null"])
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .output()
            .expect("failed to run make");
        let stdout = String::from_utf8_lossy(&output.stdout);
        // POSIX 105395: "the complete set of macro definitions and target
        // descriptions"; the format is unspecified, so this is makefile syntax.
        // POSIX Example 8 runs exactly this to view the built-in rules, so it
        // must succeed on a makefile with no targets.
        assert!(stdout.contains("# macros"), "stdout: {stdout}");
        assert!(stdout.contains("CC = c17"), "stdout: {stdout}");
        assert!(stdout.contains("# inference rules"), "stdout: {stdout}");
        assert!(stdout.contains(".c.o:"), "stdout: {stdout}");
        assert!(stdout.ends_with('\n'), "dump must end with a newline");
        assert_eq!(output.status.code(), Some(0));
    }

    // The dump reports what was actually parsed, not a hand-maintained table:
    // the makefile's own macros and rules appear alongside the built-ins.
    #[test]
    fn dash_p_reports_the_parsed_makefile() {
        let bin = get_binary_path("make");
        let output = Command::new(bin)
            .args([
                "-pn",
                "-f",
                "tests/makefiles/arguments/dash_p/with_phony.mk",
            ])
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .output()
            .expect("failed to run make");
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(stdout.contains("# rules"), "stdout: {stdout}");
        assert!(stdout.contains("clean:"), "stdout: {stdout}");
        assert_eq!(output.status.code(), Some(0));
    }

    // The dump is makefile syntax, so it parses as one.
    #[test]
    fn dash_p_output_is_a_makefile() {
        let bin = get_binary_path("make");
        let dump = Command::new(&bin)
            .args(["-p", "-f", "/dev/null"])
            .output()
            .expect("failed to run make");
        let path = std::env::temp_dir().join("make_p_roundtrip.mk");
        std::fs::write(&path, &dump.stdout).unwrap();
        let reparsed = Command::new(&bin)
            .arg("-f")
            .arg(&path)
            .arg("-n")
            .arg(".c.o")
            .output()
            .expect("failed to run make");
        // It parses: the failure mode we are excluding is a parse error (4).
        assert_ne!(reparsed.status.code(), Some(4), "dump did not re-parse");
        let _ = std::fs::remove_file(&path);
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

    // Audit #4: `-k` must not report failure (or exit nonzero) when every
    // target builds successfully.
    #[test]
    fn dash_k_success() {
        run_test_helper(
            &["-kf", "tests/makefiles/arguments/dash_k_success.mk"],
            "a-ok\nb-ok\n",
            "",
            0,
        );
    }

    // Audit #10: multiple `-f` makefiles shall be processed in order.
    #[test]
    fn multiple_dash_f() {
        run_test_helper(
            &[
                "-f",
                "tests/makefiles/arguments/multi_f/a.mk",
                "-f",
                "tests/makefiles/arguments/multi_f/b.mk",
                "b",
            ],
            "from-b\n",
            "",
            0,
        );
    }
}

// Audit #15 (internal macros) and #13 (MAKEFLAGS) exercised end-to-end.
mod internal_macros {
    use super::*;

    // `$^` removes duplicate prerequisites; `$+` keeps them in order.
    #[test]
    fn caret_and_plus() {
        run_test_helper(
            &["-f", "tests/makefiles/macros/internal.mk"],
            "caret a b\nplus a b a\n",
            "",
            0,
        );
    }

    // Audit #36: the recipe shell comes from the `SHELL` macro. It used to be
    // inert because `Make::macros` was always empty, so every recipe ran under
    // /bin/sh no matter what the makefile said. `/bin/echo` as the shell makes
    // the invocation visible instead of running it.
    #[test]
    fn shell_macro_selects_the_recipe_shell() {
        let bin = get_binary_path("make");
        let output = Command::new(bin)
            .args(["-f", "tests/makefiles/macros/shell_macro.mk"])
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .output()
            .expect("failed to run make");
        let stdout = String::from_utf8_lossy(&output.stdout);
        // /bin/echo consumes the leading `-e` as its own "enable escapes"
        // flag, so what it prints back is the rest of the invocation. Seeing
        // it at all proves /bin/echo, not /bin/sh, ran the recipe.
        assert_eq!(stdout, "-c echo hi\n", "stdout: {stdout}");
        assert_eq!(output.status.code(), Some(0));
    }

    fn run_env_export(extra_env: Option<(&str, &str)>, args: &[&str]) -> String {
        let bin = get_binary_path("make");
        let mut cmd = Command::new(bin);
        cmd.arg("-f")
            .arg("tests/makefiles/macros/env_export.mk")
            .args(args);
        if let Some((k, v)) = extra_env {
            cmd.env(k, v);
        }
        let output = cmd
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .output()
            .expect("failed to run make");
        String::from_utf8_lossy(&output.stdout).to_string()
    }

    // POSIX 105869: a macro defined in a makefile "shall not be added to the
    // environment of make if they are not already in its environment". Wiring
    // #36 made `Make::macros` non-empty for the first time, which turned
    // `init_env`'s unconditional export into a live leak.
    #[test]
    fn makefile_macro_absent_from_env_is_not_exported() {
        let stdout = run_env_export(None, &[]);
        assert_eq!(stdout, "MYMACRO=[]\n", "stdout: {stdout}");
    }

    // 105871 leaves updating an existing variable unspecified; we update it,
    // matching GNU make.
    #[test]
    fn makefile_macro_present_in_env_is_updated() {
        let stdout = run_env_export(Some(("MYMACRO", "fromenv")), &[]);
        assert_eq!(stdout, "MYMACRO=[frommakefile]\n", "stdout: {stdout}");
    }

    // Under -e the environment wins over the makefile.
    #[test]
    fn dash_e_lets_the_environment_win() {
        let stdout = run_env_export(Some(("MYMACRO", "fromenv")), &["-e"]);
        assert_eq!(stdout, "MYMACRO=[fromenv]\n", "stdout: {stdout}");
    }

    // Audit #13: the `MAKEFLAGS` environment variable seeds options; `n`
    // behaves as `-n` (print recipe, do not execute).
    #[test]
    fn makeflags_letters_form() {
        let bin = get_binary_path("make");
        let output = Command::new(bin)
            .args(["-f", "tests/makefiles/macros/internal.mk"])
            .env("MAKEFLAGS", "n")
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .output()
            .expect("failed to run make");
        let stdout = String::from_utf8_lossy(&output.stdout);
        // Under -n the recipes are printed, not run.
        assert!(stdout.contains("echo caret"), "stdout: {stdout}");
        assert_eq!(output.status.code(), Some(0));
    }
}

// Audit #9: parallel execution (`-j`, `.WAIT`, `.NOTPARALLEL`).
mod parallel {
    use super::*;

    fn run_capture(args: &[&str]) -> (String, Option<i32>) {
        let bin = get_binary_path("make");
        let output = Command::new(bin)
            .args(args)
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .output()
            .expect("failed to run make");
        (
            String::from_utf8_lossy(&output.stdout).into_owned(),
            output.status.code(),
        )
    }

    // `-j` is accepted and builds all independent targets (output order is
    // unspecified under parallelism, so only membership/exit are checked).
    #[test]
    fn dash_j_builds_all_targets() {
        let (out, code) =
            run_capture(&["-j", "2", "-f", "tests/makefiles/parallel/independent.mk"]);
        assert_eq!(code, Some(0), "out: {out}");
        for t in ["a", "b", "c"] {
            assert!(out.contains(t), "missing {t} in: {out}");
        }
    }

    // The last `-j` value wins (POSIX) — `-j 1 -j 2` must not be rejected.
    #[test]
    fn dash_j_last_value_wins() {
        let (_out, code) = run_capture(&[
            "-j",
            "1",
            "-j",
            "2",
            "-f",
            "tests/makefiles/parallel/independent.mk",
        ]);
        assert_eq!(code, Some(0));
    }

    // `.NOTPARALLEL` is recognized (not rejected) and the build succeeds.
    #[test]
    fn notparallel_recognized() {
        let (out, code) =
            run_capture(&["-j", "2", "-f", "tests/makefiles/parallel/notparallel.mk"]);
        assert_eq!(code, Some(0), "out: {out}");
        assert!(out.contains('a') && out.contains('b'), "out: {out}");
    }

    // `.WAIT` as a target has no effect and as a prerequisite is a barrier, not
    // a target to build (so it must not error with "no target '.WAIT'").
    #[test]
    fn wait_barrier_is_not_built() {
        let (out, code) = run_capture(&["-j", "2", "-f", "tests/makefiles/parallel/wait.mk"]);
        assert_eq!(code, Some(0), "out: {out}");
        assert!(out.contains('a') && out.contains('b'), "out: {out}");
    }
}

// Audit #11 (shell -e) and the `$(MAKE)` recursive-make special case.
mod recipe_execution {
    use super::*;

    // Audit #11: with errors not ignored, the shell -e option is in effect, so
    // a recipe line aborts at the first failing command.
    #[test]
    fn shell_e_aborts_on_first_failure() {
        run_test_helper(
            &["-sf", "tests/makefiles/recipe_execution/shell_e.mk"],
            "",
            "make: execution error: 1\n",
            2,
        );
    }

    // The `$(MAKE)` macro expands to the make program and its recipe line runs
    // even under -n (recursive sub-make), while ordinary lines are only printed.
    #[test]
    fn make_macro_runs_under_dry_run() {
        let bin = get_binary_path("make");
        let output = Command::new(bin)
            .args([
                "-n",
                "-f",
                "tests/makefiles/recipe_execution/make_recurse.mk",
            ])
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .output()
            .expect("failed to run make");
        let stdout = String::from_utf8_lossy(&output.stdout);
        // The plain line is printed (not executed); the $(MAKE) line executes
        // the sub-make, which prints SUBMAKE-RAN.
        assert!(stdout.contains("echo top-line"), "stdout: {stdout}");
        assert!(stdout.contains("SUBMAKE-RAN"), "stdout: {stdout}");
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

    // Audit #1: a recipe line that contains '=' (a shell assignment, an
    // option like --prefix=, or a `test x = y`) must not be mistaken for a
    // macro definition and must not abort the parse.
    #[test]
    fn recipe_line_with_equals() {
        run_test_helper(
            &["-f", "tests/makefiles/parsing/recipe_with_equals.mk"],
            "result=ok\ntesteq_ok\n./configure --prefix=/usr\n",
            "",
            0,
        );
    }

    // Audit #3: a missing `include` file must produce a graceful error, not
    // an uncontrolled panic (which exits 101).
    #[test]
    fn missing_include_is_graceful_error() {
        run_test_helper_without_error_message(
            &["-f", "tests/makefiles/parsing/missing_include.mk"],
            "",
            ErrorCode::ParserError {
                constraint: posixutils_make::parser::parse::ParseError(vec![]),
            }
            .into(),
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

    // Audit #5: a command-line `macro=value` operand defines a macro and takes
    // precedence over a definition in the makefile.
    #[test]
    fn cmdline_macro_overrides_file() {
        run_test_helper(
            &[
                "-sf",
                "tests/makefiles/macros/cmdline_macro.mk",
                "FOO=override",
                "all",
            ],
            "FOO=override\n",
            "",
            0,
        );
    }

    #[test]
    fn cmdline_macro_defines() {
        run_test_helper(
            &[
                "-sf",
                "tests/makefiles/macros/cmdline_only_macro.mk",
                "BAR=hi",
                "all",
            ],
            "BAR=hi\n",
            "",
            0,
        );
    }
}

mod target_behavior {
    use super::*;
    use libc::{kill, SIGINT};
    use posixutils_make::parser::parse::ParseError;
    use std::{thread, time::Duration};

    #[test]
    fn no_targets() {
        run_test_helper(
            &["-f", "tests/makefiles/target_behavior/no_targets.mk"],
            "",
            "make: parse error:  *** No targets. Stop.\n\n",
            ErrorCode::ParserError {
                constraint: ParseError(vec![]),
            }
            .into(),
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
        // Clean up any leftover files from previous test runs
        let _ = remove_file("text.txt");

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

        // Audit #20: make resets the signal to default and re-raises it, so it
        // dies *from* the signal (no exit code) rather than exiting 128+signo.
        use std::os::unix::process::ExitStatusExt;
        assert_eq!(output.status.code(), None);
        assert_eq!(output.status.signal(), Some(SIGINT));

        // The makefile creates text.txt and the signal handler should delete it,
        // but clean up anyway in case test fails
        let _ = remove_file("text.txt");
    }

    // Audit #17: signals are caught even under -i (ignore errors); -i is not an
    // exemption from registration, so an interrupt still cleans up and the
    // process dies from the re-raised signal.
    #[test]
    fn async_events_registered_under_dash_i() {
        // Uses its own target file so it does not race async_events on text.txt.
        let _ = remove_file("text_i.txt");

        let args = [
            "-i",
            "-f",
            "tests/makefiles/target_behavior/async_events/signal_i.mk",
        ];
        let child = manual_test_helper(&args);
        let pid = child.id() as i32;

        thread::spawn(move || {
            thread::sleep(Duration::from_millis(300));
            unsafe {
                kill(pid, SIGINT);
            }
        });

        let output = child.wait_with_output().expect("failed to wait for child");
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(stderr.contains("make: Interrupt"), "stderr: {stderr}");

        use std::os::unix::process::ExitStatusExt;
        assert_eq!(output.status.signal(), Some(SIGINT));

        let _ = remove_file("text_i.txt");
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

    // Audit #22: subsequent occurrences of `.PHONY` add to (not replace) the
    // list, observable in the `-p` dump.
    #[test]
    fn phony_accumulates() {
        // Both `a` and `b` are declared phony by separate `.PHONY` lines, so
        // both recipes must run even though files of those names exist. This
        // used to be asserted against the `-p` mirror table; with that gone the
        // behaviour itself is what to check, which is the better test anyway.
        for target in ["a", "b"] {
            let _ = File::create(target);
        }
        let bin = get_binary_path("make");
        for target in ["a", "b"] {
            let output = Command::new(&bin)
                .args([
                    "-f",
                    "tests/makefiles/special_targets/phony_accumulate.mk",
                    target,
                ])
                .output()
                .expect("failed to run make");
            let stdout = String::from_utf8_lossy(&output.stdout);
            assert!(
                stdout.contains(&format!("PHONY-{target}")),
                "{target} was treated as up to date: {stdout}"
            );
        }
        for target in ["a", "b"] {
            let _ = std::fs::remove_file(target);
        }
    }

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

    // Audit #2: `.POSIX` must be accepted (a portable makefile is required to
    // include it), not rejected as an unsupported special target.
    #[test]
    fn posix() {
        run_test_helper(
            &["-f", "tests/makefiles/special_targets/posix.mk"],
            "posix-ok\n",
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
        // `.SCCS_GET` is accepted and validated, but inert: this make performs
        // no SCCS retrieval, so nothing ever runs the recipe (audit #61). It
        // used to be stored in the `-p` mirror table and read by nothing, which
        // made the gap look like a feature. What is testable is that the
        // makefile is still accepted and the ordinary target builds.
        run_test_helper(
            &[
                "-f",
                "tests/makefiles/special_targets/sccs/basic_sccs.mk",
                "target",
            ],
            "something\n",
            "",
            0,
        );
    }

    #[test]
    fn precious() {
        // Clean up any leftover files from previous test runs
        let _ = remove_file("precious_text.txt");
        let _ = remove_file("preciousdir/some.txt");
        let _ = remove_dir("preciousdir");

        let args = [
            "-f",
            "tests/makefiles/special_targets/precious/basic_precious.mk",
        ];
        let child = manual_test_helper(&args);
        let pid = child.id() as i32;

        thread::spawn(move || {
            // Give makefile time to execute through mkdir, touch, and start sleep
            thread::sleep(Duration::from_millis(800));
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

        // Audit #20: dies from the re-raised signal rather than exiting 128+signo.
        use std::os::unix::process::ExitStatusExt;
        assert_eq!(output.status.code(), None);
        assert_eq!(output.status.signal(), Some(SIGINT));

        let _ = remove_file("precious_text.txt");
        remove_file("preciousdir/some.txt").unwrap();
        remove_dir("preciousdir").unwrap();
    }

    #[test]
    fn suffixes() {
        run_test_helper_with_setup_and_destruct(
            &[
                "-f",
                "tests/makefiles/special_targets/suffixes/suffixes_basic.mk",
                // The fixture defines only an inference rule, so it has no
                // default target and the target must be named. `make` with no
                // operand used to fall back to the first inference rule and
                // scan the working directory for anything matching; POSIX
                // 105428 makes the default the first target that is not
                // special or an inference rule, and GNU agrees.
                "suffixes_test.xfo",
            ],
            "Converting suffixes_test.sfx to suffixes_test.xfo\n",
            "",
            0,
            create_file,
            remove_files,
        );

        fn create_file() {
            File::create("suffixes_test.sfx")
                .unwrap()
                .write_all(b"some content")
                .unwrap();
        }

        fn remove_files() {
            let _ = remove_file("suffixes_test.sfx");
            let _ = remove_file("suffixes_test.xfo");
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

        // Audit (Phase 9): `.DEFAULT` is specified with commands; an empty
        // `.DEFAULT:` is a constraint violation.
        #[test]
        fn default_without_recipes() {
            run_test_helper(
                &["-f", "tests/makefiles/special_targets/validations/default_without_recipes.mk"],
                "",
                "make: '.DEFAULT' special target constraint is not fulfilled: the special target must have recipes\n",
                ErrorCode::SpecialTargetConstraintNotFulfilled {
                    target: String::default(),
                    constraint: special_target::Error::MustHaveRecipes,
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

mod inference_rules {
    use super::*;

    #[test]
    fn dash_r_suffix_classification() {
        // Test that -r flag + user-defined .SUFFIXES + inference rule works correctly.
        // This directly tests the original bug: inference rules must be classified
        // correctly even when -r clears built-in suffixes.
        run_test_helper_with_setup_and_destruct(
            &["-rf", "tests/makefiles/inference_rules/dash_r_suffix.mk"],
            "Converting dash_r_test.txt to dash_r_test.out\n",
            "",
            0,
            || {
                File::create("dash_r_test.txt").expect("failed to create file");
            },
            || {
                let _ = remove_file("dash_r_test.txt");
                let _ = remove_file("dash_r_test.out");
            },
        );
    }

    #[test]
    fn target_no_commands_uses_inference() {
        // Test that a target rule with no commands triggers inference rule lookup.
        // Per POSIX: "When no target rule with commands is found to update a
        // target, the inference rules shall be checked."
        run_test_helper_with_setup_and_destruct(
            &[
                "-sf",
                "tests/makefiles/inference_rules/target_no_commands.mk",
            ],
            "Inference applied: inference_test.src -> inference_test.dst\n",
            "",
            0,
            || {
                File::create("inference_test.src").expect("failed to create file");
            },
            || {
                let _ = remove_file("inference_test.src");
                let _ = remove_file("inference_test.dst");
            },
        );
    }

    #[test]
    fn inference_rule_not_default_target() {
        // Test that inference rules are never selected as the default target.
        // Per POSIX: "the first target that make encounters that is not a
        // special target or an inference rule shall be used."
        run_test_helper(
            &[
                "-sf",
                "tests/makefiles/inference_rules/not_default_target.mk",
            ],
            "CORRECT: real target ran\n",
            "",
            0,
        );
    }

    // Audit #8: a single-suffix inference rule (`.c:`) builds a suffixless
    // target from its `.c` prerequisite.
    #[test]
    fn single_suffix_rule() {
        run_test_helper_with_setup_and_destruct(
            &[
                "-sf",
                "tests/makefiles/inference_rules/single_suffix.mk",
                "ssfx_test",
            ],
            "built ssfx_test from ssfx_test.c\n",
            "",
            0,
            || {
                File::create("ssfx_test.c").expect("failed to create file");
            },
            || {
                let _ = remove_file("ssfx_test.c");
                let _ = remove_file("ssfx_test");
            },
        );
    }

    // Audit #16: an empty `.SUFFIXES:` clears the suffix list; a later
    // `.SUFFIXES:` with prerequisites appends, so inference works on the
    // re-added suffixes.
    #[test]
    fn suffixes_clear_then_readd() {
        run_test_helper_with_setup_and_destruct(
            &[
                "-sf",
                "tests/makefiles/special_targets/suffixes/clear_then_readd.mk",
                "cleartest.p2",
            ],
            "converted cleartest.p1 to cleartest.p2\n",
            "",
            0,
            || {
                File::create("cleartest.p1").expect("failed to create file");
            },
            || {
                let _ = remove_file("cleartest.p1");
                let _ = remove_file("cleartest.p2");
            },
        );
    }
}

// Build-graph state: cycles, rule merging, and single-build under -j.
mod build_graph {
    use super::*;

    fn run(args: &[&str]) -> (String, Option<i32>) {
        let bin = get_binary_path("make");
        let output = Command::new(bin)
            .args(args)
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .output()
            .expect("failed to run make");
        (
            String::from_utf8_lossy(&output.stdout).to_string(),
            output.status.code(),
        )
    }

    // Audit #28: a cycle that does not pass through the root used to recurse
    // until the stack overflowed (exit 134). The visited/stack sets were seeded
    // with the root and never inserted into during the walk.
    #[test]
    fn indirect_cycle_is_diagnosed_not_a_stack_overflow() {
        let (_, code) = run(&["-f", "tests/makefiles/graph/indirect_cycle.mk", "a"]);
        assert_eq!(code, Some(8), "expected a clean cycle diagnostic");
    }

    // Audit #30: POSIX 105653 lets several rules name one target, with
    // prerequisites accumulating. Only the first was used, so `b` never built
    // and the recipe never ran -- silently, with exit 0.
    #[test]
    fn prerequisites_accumulate_across_rules() {
        let (stdout, code) = run(&["-f", "tests/makefiles/graph/split_rules.mk", "all"]);
        assert!(stdout.contains("BUILT-A"), "stdout: {stdout}");
        assert!(stdout.contains("BUILT-B"), "stdout: {stdout}");
        assert!(stdout.contains("done"), "stdout: {stdout}");
        assert_eq!(code, Some(0));
    }

    // Audit #31: a target reachable by two paths was built once per path.
    #[test]
    fn a_shared_prerequisite_builds_once() {
        let (stdout, code) = run(&["-f", "tests/makefiles/graph/diamond.mk", "all"]);
        assert_eq!(stdout.matches("SHARED").count(), 1, "stdout: {stdout}");
        assert_eq!(code, Some(0));
    }

    // Audit #29: and under -j two shells ran that recipe *concurrently*, which
    // is a data race on whatever the recipe writes.
    #[test]
    fn a_shared_prerequisite_builds_once_under_dash_j() {
        let (stdout, code) = run(&["-f", "tests/makefiles/graph/diamond.mk", "-j4", "all"]);
        assert_eq!(stdout.matches("SHARED").count(), 1, "stdout: {stdout}");
        assert_eq!(code, Some(0));
    }
}

// Inference rules, `%` pattern rules, and default-target selection.
mod inference {
    use super::*;
    use std::fs;

    fn run(args: &[&str]) -> (String, Option<i32>) {
        let bin = get_binary_path("make");
        let output = Command::new(bin)
            .args(args)
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .output()
            .expect("failed to run make");
        (
            String::from_utf8_lossy(&output.stdout).to_string(),
            output.status.code(),
        )
    }

    fn with_files(files: &[&str], body: impl FnOnce()) {
        for f in files {
            let _ = File::create(f);
        }
        body();
        for f in files {
            let _ = fs::remove_file(f);
        }
    }

    // `%.o: %.c` -- not POSIX, but how real makefiles express inference.
    #[test]
    fn pattern_rule_builds_with_stem_and_input() {
        with_files(&["pat_probe.c"], || {
            let (stdout, code) = run(&["-f", "tests/makefiles/inference/pattern.mk", "all"]);
            assert!(
                stdout.contains("target=pat_probe.o input=pat_probe.c stem=pat_probe"),
                "stdout: {stdout}"
            );
            assert_eq!(code, Some(0));
        });
        let _ = fs::remove_file("pat_probe.o");
    }

    // Audit #37: `$*` is the target with its suffix removed, not the target.
    #[test]
    fn star_is_the_stem() {
        with_files(&["star_probe.c"], || {
            let (stdout, _) = run(&["-f", "tests/makefiles/inference/star.mk", "all"]);
            assert!(stdout.contains("STAR=[star_probe]"), "stdout: {stdout}");
            assert!(stdout.contains("AT=[star_probe.o]"), "stdout: {stdout}");
        });
        let _ = fs::remove_file("star_probe.o");
    }

    // Audit #47: POSIX 105920 -- ".SUFFIXES" order picks the rule, not the
    // order the rules happen to appear in the makefile. Both orderings must
    // agree, and both must pick .sh because it is listed first.
    #[test]
    fn suffixes_order_decides_not_rule_order() {
        with_files(&["ord_probe.c", "ord_probe.sh"], || {
            for fixture in [
                "tests/makefiles/inference/suffix_order_c_first.mk",
                "tests/makefiles/inference/suffix_order_sh_first.mk",
            ] {
                let _ = fs::remove_file("ord_probe");
                let (stdout, _) = run(&["-f", fixture, "ord_probe"]);
                assert!(stdout.contains("VIA-SH"), "{fixture} gave: {stdout}");
            }
        });
        let _ = fs::remove_file("ord_probe");
    }

    // Audit #38: `.config` is not an inference rule just because it starts
    // with a dot. It used to be classified as one, find no files to scan, and
    // silently run nothing -- while also displacing the real default target.
    #[test]
    fn a_dot_target_is_not_an_inference_rule() {
        let (stdout, code) = run(&["-f", "tests/makefiles/inference/dot_target.mk", ".config"]);
        assert!(stdout.contains("BUILT-DOTCONFIG"), "stdout: {stdout}");
        assert_eq!(code, Some(0));
    }

    // POSIX 105428: the default is the first target that is not special or an
    // inference rule, so a leading dot-target is skipped. GNU agrees.
    #[test]
    fn default_target_skips_a_dot_target() {
        let (stdout, code) = run(&["-f", "tests/makefiles/inference/dot_target.mk"]);
        assert!(stdout.contains("BUILT-REAL"), "stdout: {stdout}");
        assert!(!stdout.contains("DOTCONFIG"), "stdout: {stdout}");
        assert_eq!(code, Some(0));
    }
}

// Recipe execution and command-line semantics.
mod execution {
    use super::*;
    use std::fs;

    fn run(args: &[&str]) -> (String, String, Option<i32>) {
        let bin = get_binary_path("make");
        let output = Command::new(bin)
            .args(args)
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .output()
            .expect("failed to run make");
        (
            String::from_utf8_lossy(&output.stdout).to_string(),
            String::from_utf8_lossy(&output.stderr).to_string(),
            output.status.code(),
        )
    }

    // Audit #33: -k means "keep building the other targets", not "build this
    // target's dependents anyway". The failing recipe used to return Ok, so
    // `all` ran against inputs that were never produced.
    #[test]
    fn keep_going_skips_a_target_whose_prerequisite_failed() {
        let (stdout, _, code) = run(&[
            "-f",
            "tests/makefiles/execution/keep_going_skips.mk",
            "-k",
            "all",
        ]);
        assert!(!stdout.contains("SHOULD-NOT-RUN"), "stdout: {stdout}");
        // ...but an independent sibling still gets built, which is the point of -k.
        assert!(stdout.contains("B-RAN"), "stdout: {stdout}");
        assert_eq!(code, Some(2));
    }

    // Audit #42: `-t` on a .PHONY target used to create a file named after it,
    // after which every `make <target>` reported it up to date forever.
    #[test]
    fn touch_does_not_materialize_a_phony_target() {
        let _ = fs::remove_file("phony_probe");
        let (_, _, code) = run(&[
            "-f",
            "tests/makefiles/execution/phony_touch.mk",
            "-t",
            "phony_probe",
        ]);
        assert_eq!(code, Some(0));
        assert!(
            !std::path::Path::new("phony_probe").exists(),
            "-t created a file for a .PHONY target"
        );
    }

    // Audit #39: a target is a filename and need not be valid UTF-8.
    #[test]
    fn a_non_utf8_target_is_diagnosed_not_a_panic() {
        use std::os::unix::ffi::OsStrExt;
        let bin = get_binary_path("make");
        let output = Command::new(bin)
            .arg("-f")
            .arg("tests/makefiles/execution/makeflags.mk")
            .arg(std::ffi::OsStr::from_bytes(b"\xff"))
            .output()
            .expect("failed to run make");
        // 101 is the Rust panic exit status.
        assert_ne!(output.status.code(), Some(101), "make panicked");
        assert_eq!(output.status.code(), Some(2));
    }

    // Audit #40: POSIX 105866 -- make passes its options down through
    // MAKEFLAGS. It was never constructed, so a sub-make saw nothing.
    #[test]
    fn makeflags_carries_options_to_children() {
        let (stdout, _, _) = run(&["-f", "tests/makefiles/execution/makeflags.mk", "-k", "all"]);
        assert!(stdout.contains("MAKEFLAGS=[k]"), "stdout: {stdout}");
    }

    #[test]
    fn makeflags_is_empty_when_no_options_are_given() {
        let (stdout, _, _) = run(&["-f", "tests/makefiles/execution/makeflags.mk", "all"]);
        assert!(stdout.contains("MAKEFLAGS=[]"), "stdout: {stdout}");
    }
}

// VPATH search, and the built-in inference rules POSIX requires.
mod builtins {
    use super::*;
    use std::fs;

    fn run(args: &[&str]) -> (String, Option<i32>) {
        let bin = get_binary_path("make");
        let output = Command::new(bin)
            .args(args)
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .output()
            .expect("failed to run make");
        (
            String::from_utf8_lossy(&output.stdout).to_string(),
            output.status.code(),
        )
    }

    // Audit #53: VPATH names directories to search for a prerequisite that is
    // not where the rule says. `$<` must name where it was actually found.
    #[test]
    fn vpath_finds_a_prerequisite_and_names_it() {
        let _ = fs::create_dir_all("vpath_src");
        let _ = fs::write("vpath_src/vp_probe.c", "");
        let (stdout, code) = run(&["-f", "tests/makefiles/vpath/vpath.mk", "all"]);
        assert!(
            stdout.contains("IN=vpath_src/vp_probe.c"),
            "stdout: {stdout}"
        );
        assert_eq!(code, Some(0));
        let _ = fs::remove_file("vp_probe.o");
        let _ = fs::remove_dir_all("vpath_src");
    }

    // Audit #55: the `vpath` directive gives a search path per pattern, unlike
    // the blanket VPATH macro. `$<` must name where the file was found.
    #[test]
    fn vpath_directive_finds_a_prerequisite() {
        let _ = fs::create_dir_all("vpath_dir_probe");
        let _ = fs::write("vpath_dir_probe/vpd_probe.c", "");
        let (stdout, code) = run(&["-f", "tests/makefiles/vpath/directive.mk", "all"]);
        assert!(
            stdout.contains("IN=vpath_dir_probe/vpd_probe.c"),
            "stdout: {stdout}"
        );
        assert_eq!(code, Some(0));
        let _ = fs::remove_file("vpd_probe.o");
        let _ = fs::remove_dir_all("vpath_dir_probe");
    }

    // Audit #54: the default rules existed only as display strings in the -p
    // table, so `make f.o` with an f.c present reported "no target" -- every
    // makefile relying on the built-in .c.o rule, which is most of them.
    #[test]
    fn builtin_c_to_o_rule_applies() {
        let _ = fs::write("builtin_probe.c", "int probe(void){return 0;}\n");
        // CFLAGS defaults to POSIX's `-O 1` (spec line 106049), which c17 takes as
        // two arguments and cc does not, so it is cleared alongside CC.
        let (_, code) = run(&[
            "-f",
            "tests/makefiles/vpath/builtin.mk",
            "CC=cc",
            "CFLAGS=",
            "all",
        ]);
        assert_eq!(code, Some(0), "built-in .c.o rule did not apply");
        assert!(std::path::Path::new("builtin_probe.o").exists());
        let _ = fs::remove_file("builtin_probe.o");
        let _ = fs::remove_file("builtin_probe.c");
    }

    // -r suppresses them, as POSIX requires. Its own fixture and source file,
    // so it cannot race the test above under parallel execution.
    #[test]
    fn dash_r_suppresses_the_builtin_rules() {
        let _ = fs::write("builtin_r_probe.c", "int probe(void){return 0;}\n");
        let (_, code) = run(&[
            "-r",
            "-f",
            "tests/makefiles/vpath/builtin_r.mk",
            "CC=cc",
            "CFLAGS=",
            "all",
        ]);
        assert_ne!(code, Some(0), "-r must suppress the built-in rules");
        let _ = fs::remove_file("builtin_r_probe.o");
        let _ = fs::remove_file("builtin_r_probe.c");
    }
}
