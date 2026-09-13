//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{get_binary_path, run_test, run_test_with_checker, TestPlan};
use std::fs;
use std::path::Path;
use std::process::{Command, Output, Stdio};

/// Unescape newlines from .out file format
fn unescape_newlines(input: &str) -> String {
    input.replace("\\n", "\n")
}

/// Parse the .out file format (key=value lines)
fn parse_out_file(content: &str) -> (String, String, i32, bool, bool, Option<String>) {
    let mut stdout = String::new();
    let mut stderr = String::new();
    let mut status = 0i32;
    let mut ignore = false;
    let mut expect_error = false;
    let mut stdout_regex: Option<String> = None;

    for line in content.lines() {
        if line.is_empty() || line.starts_with('#') {
            continue;
        }
        if let Some((key, value)) = line.split_once('=') {
            match key {
                "stdout" => stdout = unescape_newlines(value),
                "stderr" => stderr = unescape_newlines(value),
                "status" => status = value.parse().unwrap_or(0),
                "ignore" => ignore = value == "true",
                "expect_error" => expect_error = value == "true",
                "stdout_regex" => stdout_regex = Some(unescape_newlines(value)),
                "skip_update" => {} // Not needed at runtime
                _ => {}
            }
        }
    }

    (stdout, stderr, status, ignore, expect_error, stdout_regex)
}

/// Load a fixture and create a TestPlan
fn load_fixture(name: &str) -> TestPlan {
    let base = Path::new("fixtures/integration_tests");

    // Check if this is a .args file (CLI args test) or .m4 file (stdin test)
    let args_path = base.join(format!("{}.args", name));
    let m4_path = base.join(format!("{}.m4", name));
    let out_path = base.join(format!("{}.out", name));

    let out_content = fs::read_to_string(&out_path)
        .unwrap_or_else(|_| panic!("Failed to read {}", out_path.display()));
    let (expected_out, expected_err, expected_exit_code, _, _, _) = parse_out_file(&out_content);

    if args_path.exists() {
        // CLI args test - args are in the file content
        let args_content = fs::read_to_string(&args_path)
            .unwrap_or_else(|_| panic!("Failed to read {}", args_path.display()));
        let args: Vec<String> = args_content.split_whitespace().map(String::from).collect();

        TestPlan {
            cmd: String::from("m4"),
            args,
            stdin_data: String::new(),
            expected_out,
            expected_err,
            expected_exit_code,
        }
    } else {
        // stdin-based test
        let input = fs::read_to_string(&m4_path)
            .unwrap_or_else(|_| panic!("Failed to read {}", m4_path.display()));

        TestPlan {
            cmd: String::from("m4"),
            args: vec![],
            stdin_data: input,
            expected_out,
            expected_err,
            expected_exit_code,
        }
    }
}

/// Checker for expect_error tests - only check that stderr is non-empty if expected
fn expect_error_checker(plan: &TestPlan, output: &Output) {
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(stdout.as_ref(), &plan.expected_out, "stdout mismatch");

    // For expect_error, we only check that stderr is non-empty if expected
    if !plan.expected_err.is_empty() {
        assert!(!output.stderr.is_empty(), "expected stderr to be non-empty");
    }

    assert_eq!(
        output.status.code(),
        Some(plan.expected_exit_code),
        "exit code mismatch"
    );
}

/// Checker for stdout_regex tests
fn stdout_regex_checker(regex_pattern: &str) -> impl Fn(&TestPlan, &Output) + '_ {
    move |plan: &TestPlan, output: &Output| {
        let stdout = String::from_utf8_lossy(&output.stdout);
        let re = regex::Regex::new(regex_pattern).unwrap();
        assert!(
            re.is_match(&stdout),
            "stdout doesn't match regex: {}\nActual stdout: {}",
            regex_pattern,
            stdout
        );

        let stderr = String::from_utf8_lossy(&output.stderr);
        assert_eq!(stderr.as_ref(), &plan.expected_err, "stderr mismatch");

        assert_eq!(
            output.status.code(),
            Some(plan.expected_exit_code),
            "exit code mismatch"
        );
    }
}

// Macro for standard fixture tests
macro_rules! m4_test {
    ($name:ident) => {
        #[test]
        fn $name() {
            run_test(load_fixture(stringify!($name)));
        }
    };
}

// Macro for expect_error tests
macro_rules! m4_test_expect_error {
    ($name:ident) => {
        #[test]
        fn $name() {
            run_test_with_checker(load_fixture(stringify!($name)), expect_error_checker);
        }
    };
}

// Macro for stdout_regex tests
macro_rules! m4_test_regex {
    ($name:ident, $regex:expr) => {
        #[test]
        fn $name() {
            let plan = load_fixture(stringify!($name));
            run_test_with_checker(plan, stdout_regex_checker($regex));
        }
    };
}

// ============================================================================
// Standard tests
// ============================================================================

m4_test!(bsd);
m4_test!(bsd_math);
m4_test!(changecom);
m4_test!(changequote);
m4_test!(decr);
m4_test!(define);
m4_test!(define_args);
m4_test!(define_eval_order_unquoted);
m4_test!(define_eval_syntax_order_quoted_evaluated);
m4_test!(define_eval_syntax_order_quoted_unevaluated);
m4_test!(define_eval_syntax_order_unquoted);
m4_test!(define_hanging_quotes);
m4_test!(define_invalid_macro_name);
m4_test!(define_iterative);
m4_test!(define_iterative_2);
m4_test!(define_nested);
m4_test!(define_nested_first_arg);
m4_test!(define_order_defined);
m4_test!(define_order_undefined);
m4_test!(define_pushpopdef_undefine);
m4_test!(define_stacked);
m4_test!(define_undefine_order);
m4_test!(define_undefine_operand_order);
m4_test!(defn);
m4_test!(defn_builtin_rename);
m4_test!(dollar_hash_args);
m4_test!(divert);
m4_test!(divert_large);
m4_test!(divert_nested);
m4_test!(divert_nested_2);
m4_test!(divert_nested_3);
m4_test!(divert_nested_4);
m4_test!(dnl);
m4_test!(dnl_nested);
m4_test!(dumpdef);
m4_test!(eval);
m4_test!(eval_base_literals);
m4_test!(eval_radix);
m4_test!(eval_shift_precedence);
m4_test!(evaluation_order);
// file test needs the actual file path (uses __file__ macro), not stdin
#[test]
fn file() {
    run_test(TestPlan {
        cmd: String::from("m4"),
        args: vec![String::from("fixtures/integration_tests/file.m4")],
        stdin_data: String::new(),
        expected_out: String::from(
            "fixtures/integration_tests/file.m4\nfixtures/integration_tests/include/file.m4",
        ),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}
m4_test!(forloop_nested);
m4_test!(forloop_simple);
m4_test!(forward_file_order);
m4_test!(ifdef);
m4_test!(ifelse);
m4_test!(include);
m4_test!(include_divert);
m4_test!(incr);
m4_test!(index);
m4_test!(index_empty_second_arg);
m4_test!(len);
m4_test!(m4exit_error);
m4_test!(m4exit_no_args);
m4_test!(m4exit_success);
m4_test!(m4wrap);
m4_test!(m4wrap_rescan);
m4_test!(macro_errprint_evaluation);
m4_test!(macro_errprint_no_evaluation);
m4_test!(macro_errprint_no_evaluation_quoted);
m4_test!(recurse);
m4_test!(recursive_defines);
m4_test!(redefine_inbuilt);
m4_test!(reverse);
m4_test!(shift);
m4_test!(sinclude);
m4_test!(substr);
m4_test!(synclines_1);
m4_test!(synclines_2);
m4_test!(synclines_divert);
m4_test!(synclines_literals);
m4_test!(synclines_undivert_empty);
m4_test!(syscmd_sysval);
m4_test!(trace);
m4_test!(translit);
m4_test!(translit_single_arg);
m4_test!(changecom_single_arg);
m4_test!(two_files);
m4_test!(undivert);
m4_test!(undivert_2);
m4_test!(undivert_current);
m4_test!(undivert_nested);

// ============================================================================
// Expect error tests (stderr may not match exactly)
// ============================================================================

m4_test_expect_error!(define_number_parsing);
m4_test_expect_error!(define_parse_brackets);
m4_test_expect_error!(define_quoted_number_stacked);
m4_test_expect_error!(define_unquoted_number_arg);
m4_test_expect_error!(dumpdef_notexist);
m4_test_expect_error!(divert_survives_failed_operand);
m4_test_expect_error!(incr_non_numeric);
m4_test_expect_error!(missing_file_operand);
m4_test_expect_error!(eval_divide_by_zero);
m4_test_expect_error!(eval_modulo_by_zero);
m4_test_expect_error!(index_too_few_args);
m4_test_expect_error!(quoted_nested_eof_in_string);

// ============================================================================
// Regex stdout tests (output contains random/variable data)
// ============================================================================

m4_test_regex!(maketemp, r"^/tmp/m4-.{6}$");
m4_test_regex!(mkstemp, r"^/tmp/m4-.{6}$");

// ============================================================================
// Command-line option-argument validation
// ============================================================================

// A -D or -U name that is not a name token is a usage error. Both used to
// `.expect()` on the parse, printing the Debug spelling of a clap::Error and
// aborting with 101; they now render like any other bad option-argument.
#[test]
fn define_option_rejects_a_name_starting_with_a_digit() {
    run_test(TestPlan {
        cmd: String::from("m4"),
        args: vec![String::from("-D9bad=x"), String::from("/dev/null")],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::from("error: invalid value '9bad=x' for '-D <name[=value]>'\n"),
        expected_exit_code: 2,
    });
}

// POSIX 103940 leaves an ifelse of fewer than three arguments unspecified, so
// where to warn is a quality call, and GNU m4 draws it at exactly two: two
// comparands with no branch warns, while one argument or none is silent. This
// warned for all three.
#[test]
fn ifelse_warns_at_two_arguments_but_not_fewer() {
    for args in ["ifelse()", "ifelse(a)"] {
        run_test(TestPlan {
            cmd: String::from("m4"),
            args: vec![],
            stdin_data: format!("{}\n", args),
            expected_out: String::from("\n"),
            expected_err: String::new(),
            expected_exit_code: 0,
        });
    }
    run_test_with_checker(
        TestPlan {
            cmd: String::from("m4"),
            args: vec![],
            stdin_data: String::from("ifelse(a,a)\n"),
            expected_out: String::from("\n"),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(
                stderr.contains("too few arguments to builtin `ifelse'"),
                "expected the two-argument warning, got {stderr:?}"
            );
        },
    );
}

#[test]
fn undefine_option_rejects_a_name_starting_with_a_digit() {
    run_test(TestPlan {
        cmd: String::from("m4"),
        args: vec![String::from("-U9bad"), String::from("/dev/null")],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::from("error: invalid value '9bad' for '-U <name>'\n"),
        expected_exit_code: 2,
    });
}

// ============================================================================
// GNU m4 differential gate
// ============================================================================

/// Run every stdin-driven fixture through both this m4 and the system's GNU
/// m4, and require the stdout to agree.
///
/// The 85 committed `.out` files are frozen snapshots: they say what this
/// implementation produced when they were written, not what m4 is supposed to
/// produce. Nothing in the crate had ever compared against a reference, which
/// is how `m4/docs/Compatibility.md` came to assert a breadth-first
/// argument-expansion incompatibility that had stopped being true.
///
/// Skipped, loudly, when GNU m4 is not installed -- CI must stay green without
/// it, but a silent skip would make this gate exactly the kind of claim it
/// exists to check.
///
/// stdout only. stderr wording is deliberately ours (the utility name, and the
/// diagnostics reworded over the last several commits), and diverging there is
/// not a semantic difference. Fixtures whose output is not a function of the
/// input alone are named and excluded.
#[test]
fn gnu_m4_differential() {
    const GNU: &str = "/usr/bin/m4";

    // maketemp/mkstemp generate random filenames by design, so no two runs of
    // any implementation agree, let alone two implementations.
    const NONDETERMINISTIC: &[&str] = &["maketemp", "mkstemp"];

    if !Path::new(GNU).exists() {
        eprintln!("skipping gnu_m4_differential: {GNU} is not installed");
        return;
    }

    let base = Path::new("fixtures/integration_tests");
    let mut compared = 0usize;
    let mut differing: Vec<String> = Vec::new();

    let mut entries: Vec<_> = fs::read_dir(base)
        .expect("fixture directory")
        .filter_map(Result::ok)
        .map(|e| e.path())
        .filter(|p| p.extension().is_some_and(|e| e == "m4"))
        .collect();
    entries.sort();

    for path in entries {
        let name = path.file_stem().unwrap().to_string_lossy().to_string();
        if NONDETERMINISTIC.iter().any(|n| name.contains(n)) {
            continue;
        }
        // `.args` fixtures drive the CLI, not stdin; their argv lives in a
        // separate file and several reference paths relative to the crate.
        if base.join(format!("{name}.args")).exists() {
            continue;
        }
        let Ok(input) = fs::read_to_string(&path) else {
            continue;
        };

        let run = |bin: &std::ffi::OsStr| -> Option<String> {
            let mut child = Command::new(bin)
                .current_dir(base)
                .stdin(Stdio::piped())
                .stdout(Stdio::piped())
                .stderr(Stdio::null())
                .spawn()
                .ok()?;
            use std::io::Write;
            let _ = child.stdin.as_mut()?.write_all(input.as_bytes());
            let out = child.wait_with_output().ok()?;
            Some(String::from_utf8_lossy(&out.stdout).into_owned())
        };

        let (Some(ours), Some(theirs)) =
            (run(get_binary_path("m4").as_os_str()), run(GNU.as_ref()))
        else {
            continue;
        };
        compared += 1;
        if ours != theirs {
            differing.push(format!("{name}:\n  ours: {ours:?}\n  gnu : {theirs:?}"));
        }
    }

    eprintln!("gnu_m4_differential: compared {compared} fixtures against {GNU}");
    assert!(
        compared >= 50,
        "the gate compared only {compared} fixtures; it is not exercising the suite"
    );
    assert!(
        differing.is_empty(),
        "{} of {compared} fixtures disagree with GNU m4:\n{}",
        differing.len(),
        differing.join("\n")
    );
}
