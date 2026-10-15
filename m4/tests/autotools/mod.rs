//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! What autoconf, automake and libtoolize need from m4.  Every expected
//! output below was produced by GNU m4 1.4.19 from the same input.

use plib::testing::{run_test, run_test_base, TestPlan};

/// Run m4 with `args` on `stdin` and require exactly this stdout, stderr and
/// exit status.
fn m4(args: &[&str], stdin: &str, stdout: &str, stderr: &str, status: i32) {
    run_test(TestPlan {
        cmd: String::from("m4"),
        args: args.iter().map(|a| a.to_string()).collect(),
        stdin_data: String::from(stdin),
        expected_out: String::from(stdout),
        expected_err: String::from(stderr),
        expected_exit_code: status,
    });
}

/// Run m4 and hand back stdout and stderr, for checks that cannot spell out
/// the whole of either.
fn m4_output(args: &[&str], stdin: &str) -> (String, String, Option<i32>) {
    let args: Vec<String> = args.iter().map(|a| a.to_string()).collect();
    let out = run_test_base("m4", &args, stdin.as_bytes());
    (
        String::from_utf8_lossy(&out.stdout).into_owned(),
        String::from_utf8_lossy(&out.stderr).into_owned(),
        out.status.code(),
    )
}

// ---------------------------------------------------------------------------
// POSIX: dumpdef and defn
// ---------------------------------------------------------------------------

// POSIX: "dumpdef ... or, if no arguments are specified, for all macros".  A
// bare `dumpdef` used to be left as text because the built-in demanded an
// argument list.  autom4te learns the names of the built-ins this way.
#[test]
fn bare_dumpdef_dumps_every_macro_sorted() {
    let (out, err, status) = m4_output(&[], "define(`foo',`bar')dumpdef\n");
    assert_eq!(out, "\n");
    assert_eq!(status, Some(0));
    let lines: Vec<&str> = err.lines().collect();
    assert!(lines.contains(&"foo:\tbar"), "{err}");
    assert!(lines.contains(&"define:\t<define>"), "{err}");
    assert!(lines.contains(&"dumpdef:\t<dumpdef>"), "{err}");
    let mut sorted = lines.clone();
    sorted.sort();
    assert_eq!(lines, sorted, "dumpdef output is sorted by name");
}

#[test]
fn dumpdef_sorts_named_macros_and_reports_undefined_ones() {
    m4(
        &[],
        "define(`b',`B')define(`a',`A')dumpdef(`b',`nosuch',`a')\n",
        "\n",
        "m4:stdin:1: undefined macro `nosuch'\na:\tA\nb:\tB\n",
        0,
    );
}

// POSIX: defn yields "the quoted definition ... of its arguments": all of
// them, not just the first.
#[test]
fn defn_concatenates_every_named_definition() {
    m4(
        &[],
        "define(`a',`A')define(`b',`B')[defn(`a',`zz',`b')]\n",
        "[AB]\n",
        "",
        0,
    );
}

#[test]
fn defn_of_a_builtin_among_several_names_is_dropped_with_a_warning() {
    m4(
        &[],
        "define(`a',`A')define(`d',defn(`define'))define(`f',defn(`d',`a'))dumpdef(`f')\n",
        "\n",
        "m4:stdin:1: Warning: cannot concatenate builtin `d'\nf:\tA\n",
        0,
    );
}

// A built-in copied under a new name stays that built-in: copying the copy
// works, and dumpdef names the built-in, not the alias.
#[test]
fn copied_builtin_keeps_its_identity() {
    m4(
        &[],
        "define(`d',defn(`define'))d(`e',defn(`d'))dumpdef(`d',`e')e(`x',`X')x\n",
        "X\n",
        "d:\t<define>\ne:\t<define>\n",
        0,
    );
}

// ---------------------------------------------------------------------------
// Macro names, one-argument define, predefined macros
// ---------------------------------------------------------------------------

// m4sugar keeps state in macros whose names are not name tokens, such as
// `_m4_divert(KILL)`, and reaches them through defn, ifdef and the like.
#[test]
fn any_string_names_a_macro() {
    m4(
        &[],
        "define(`a-b',`AB')define(`',`empty')dnl
1 ifdef(`a-b',yes,no) ifdef(`',yes,no) [defn(`a-b')] [defn(`')]
pushdef(`a-b',`2')pushdef(`x y',`3')dnl
2 defn(`a-b') defn(`x y')
popdef(`a-b',`x y')dnl
3 defn(`a-b') ifdef(`x y',yes,no)
undefine(`a-b',`')dnl
4 ifdef(`a-b',yes,no) ifdef(`',yes,no)
",
        "1 yes yes [AB] [empty]\n2 2 3\n3 AB no\n4 no no\n",
        "",
        0,
    );
}

#[test]
fn one_argument_define_defines_an_empty_macro() {
    m4(
        &[],
        "define(`one')[one] ifdef(`one',yes,no)\n",
        "[] yes\n",
        "",
        0,
    );
}

// m4sugar refuses to run unless `__gnu__` is defined.
#[test]
fn gnu_predefined_macros() {
    m4(
        &["--gnu"],
        "[__gnu__] [__unix__] __line__ [ifdef(`unix',u)]\ndefine(`L',`__line__')L __line__(
)
dumpdef(`__gnu__',`__unix__',`__line__')",
        "[] [] 1 []\n2 2\n",
        "m4:stdin:2: Warning: excess arguments to builtin `__line__' ignored\n__gnu__:\t\n__line__:\t<__line__>\n__unix__:\t\n",
        0,
    );
}

#[test]
fn builtin_argument_counts_are_checked() {
    m4(
        &[],
        "ifdef(`a') len(a,b) translit(abc) define(`a',`b',`c')a\n",
        " 1 abc b\n",
        "m4:stdin:1: Warning: too few arguments to builtin `ifdef'
m4:stdin:1: Warning: excess arguments to builtin `len' ignored
m4:stdin:1: Warning: too few arguments to builtin `translit'
m4:stdin:1: Warning: excess arguments to builtin `define' ignored
",
        0,
    );
}

// A diagnostic names the line the call began on, not the line its
// arguments ended on.
#[test]
fn diagnostics_name_the_line_a_call_began_on() {
    m4(
        &[],
        "\nlen(a,\nb)\n",
        "\n1\n",
        "m4:stdin:2: Warning: excess arguments to builtin `len' ignored\n",
        0,
    );
}

// changecom with no arguments disables comments inside macro arguments too.
#[test]
fn disabled_comments_stay_disabled_inside_arguments() {
    m4(
        &[],
        "changecom(`1')changecom index(a 1b,b)\n",
        " 3\n",
        "",
        0,
    );
    m4(
        &[],
        "changecom(`1')changecom() index(a 1b,b)\n",
        " 3\n",
        "",
        0,
    );
}

// ---------------------------------------------------------------------------
// Command-line options
// ---------------------------------------------------------------------------

// libtoolize spells `--prefix-builtins` as `--prefix`, a GNU getopt
// abbreviation.  The predefined text macros keep their names.
#[test]
fn prefix_builtins_renames_every_builtin() {
    for opt in ["-P", "--prefix-builtins", "--prefix"] {
        m4(
            &[opt],
            "m4_define(`x',`X')x define(`y') m4___line__ m4_len(abc) [__gnu__]\n",
            "X define(y) 1 3 []\n",
            "",
            0,
        );
    }
}

#[test]
fn include_path_is_searched_and_names_the_file_found() {
    for opt in [
        "-Ifixtures/integration_tests/include",
        "--include=fixtures/integration_tests/include",
    ] {
        m4(
            &[opt],
            "include(`file.m4')\n",
            "fixtures/integration_tests/include/file.m4\n",
            "",
            0,
        );
    }
}

#[test]
fn sinclude_ignores_a_missing_file_and_a_directory() {
    m4(
        &[],
        "sinclude(`nosuch.m4')sinclude(`fixtures')x\n",
        "x\n",
        "",
        0,
    );
}

#[test]
fn fatal_warnings_once_fails_the_exit_status() {
    for opt in ["-E", "--fatal-warnings", "--fatal-warning"] {
        m4(
            &[opt],
            "a len(a,b)x\nb\n",
            "a 1x\nb\n",
            "m4:stdin:1: Warning: excess arguments to builtin `len' ignored\n",
            1,
        );
    }
}

#[test]
fn fatal_warnings_twice_stops_at_the_first() {
    m4(
        &["-EE"],
        "a len(a,b)x\nb\n",
        "a ",
        "m4:stdin:1: Warning: excess arguments to builtin `len' ignored\n",
        1,
    );
}

#[test]
fn nesting_limit_stops_runaway_recursion() {
    for args in [&["-L", "20"][..], &["--nesting-limit=20"][..]] {
        m4(
            args,
            "define(`r',`r(r)')dnl\nbefore\nr\n",
            "before\n",
            "m4:stdin:3: recursion limit of 20 exceeded, use -L<N> to change it\n",
            1,
        );
    }
    // autom4te passes 1024; ordinary recursion stays well inside it.
    m4(
        &["--nesting-limit=1024"],
        "define(`count',`ifelse($1,0,done,`count(decr($1))')')count(100)\n",
        "done\n",
        "",
        0,
    );
}

#[test]
fn end_of_file_inside_an_argument_list_is_fatal() {
    m4(
        &[],
        "len(abc\n",
        "",
        "m4:stdin:1: ERROR: end of file in argument list\n",
        1,
    );
}

#[test]
fn long_option_spellings_of_define_and_undefine() {
    m4(
        &["--define=x=X", "--undefine=len", "--synclines"],
        "x len\n",
        "#line 1 \"stdin\"\nX len\n",
        "",
        0,
    );
}

// ---------------------------------------------------------------------------
// GNU built-ins m4sugar is written in
// ---------------------------------------------------------------------------

#[test]
fn builtin_and_indir_call_by_name() {
    m4(
        &[],
        "changequote([,])dnl
1 builtin([len],[abcd]) builtin([nosuch]) indir([len],[ab]) indir([nosuch],x)
define([a-b],[<$1|$0>])2 indir([a-b],[X])
3 builtin([define],[q],[Q])q builtin([dnl]) gone
",
        "1 4  2 \n2 <X|a-b>\n3 Q ",
        "m4:stdin:2: undefined builtin `nosuch'\nm4:stdin:2: undefined macro `nosuch'\n",
        0,
    );
}

// libtoolize defines `dnl` as `m4_builtin([dnl])` under --prefix-builtins:
// builtin takes the unprefixed names.
#[test]
fn builtin_takes_original_names_under_prefix_builtins() {
    m4(
        &["-P"],
        "m4_builtin(`dnl')gone\nm4_builtin(`len',`abc')\n",
        "3\n",
        "",
        0,
    );
}

// A built-in reached through indir can still be copied with defn.
#[test]
fn indir_passes_a_builtin_token_through() {
    m4(
        &[],
        "indir(`define',`mylen',defn(`len'))mylen(`abcd')\n",
        "4\n",
        "",
        0,
    );
}

#[test]
fn regexp_in_emacs_syntax() {
    m4(
        &[],
        "changequote([,])dnl
1 regexp([GNUs not Unix], [\\<[a-z]\\w+]) regexp([GNUs not Unix], [\\<Q\\w*])
2 regexp([GNUs not Unix], [\\w\\(\\w+\\)$], [*** \\& *** \\1 ***])
3 regexp([abc],[\\(x\\)\\|b],[\\1-\\&]) regexp([abc])
",
        "1 5 -1\n2 *** Unix *** nix ***\n3 -b 0\n",
        "m4:stdin:4: Warning: too few arguments to builtin `regexp'\n",
        0,
    );
}

#[test]
fn patsubst_in_emacs_syntax() {
    m4(
        &[],
        "changequote([,])dnl
1 patsubst([GNUs not Unix], [\\w+], [(\\&)]) patsubst([abc], [], [-])
2 patsubst([GNUs not Unix], [^], [OBS: ]) patsubst([a
b], [^], [> ])
3 patsubst([m4_define], [^m4_\\(.*\\)], [[\\1]]) patsubst([a.b.c],[\\.],[,])
4 patsubst([a(b)c], [(\\(b\\))], [[\\1]]) patsubst([aaa], [a*], [x]) patsubst([abc], [x*], [-])
5 patsubst([foo bar], [\\(o\\)\\|\\(a\\)], [\\2\\1]) patsubst([abc])
",
        "1 (GNUs) (not) (Unix) -a-b-c-\n2 OBS: GNUs not Unix > a\n> b\n3 define a,b,c\n4 abc xx -a-b-c-\n5 foo bar abc\n",
        "m4:stdin:7: Warning: too few arguments to builtin `patsubst'\n",
        0,
    );
}

#[test]
fn regular_expression_diagnostics() {
    m4(
        &[],
        "changequote([,])regexp([abc],[\\(]) regexp([x], [x], [\\3])\n",
        " \n",
        "m4:stdin:1: bad regular expression: `\\(': Unmatched ( or \\(\n\
         m4:stdin:1: Warning: sub-expression 3 not present\n",
        0,
    );
}

#[test]
fn format_conversions() {
    m4(
        &[],
        "changequote([,])dnl
1 format([%s-%d-%5s|%-5s|%.2s], [x], [42], [ab], [cd], [efgh])
2 format([%*s|%.*s|%c], [4], [x], [2], [abcdef], [65])
",
        "1 x-42-   ab|cd   |ef\n2    x|ab|A\n",
        "",
        0,
    );
}

#[test]
fn esyscmd_expands_to_the_command_output() {
    m4(&[], "esyscmd(`echo hi; exit 3')sysval\n", "hi\n3\n", "", 0);
}

#[test]
fn debugmode_q_quotes_dumpdef_and_bad_flags_are_reported() {
    m4(
        &[],
        "define(`a',`A')debugmode(`aq')dumpdef(`a')debugmode debugmode(`zz')\n",
        " \n",
        "a:\t`A'\nm4:stdin:1: Debugmode: bad debug flags: `zz'\n",
        0,
    );
}

#[test]
fn debugfile_receives_dumpdef() {
    let dir = plib::tmp::tempdir().unwrap();
    let file = dir.path().join("debug.out");
    let input = format!(
        "define(`a',`A')debugfile(`{}')dumpdef(`a')debugfile()dumpdef(`a')debugfile\n",
        file.display()
    );
    m4(&[], &input, "\n", "", 0);
    assert_eq!(std::fs::read_to_string(&file).unwrap(), "a:\tA\n");
}

// ---------------------------------------------------------------------------
// Tracing, as autom4te and libtoolize read it
// ---------------------------------------------------------------------------

/// The input of the trace tests: AC_INIT traced before it is defined,
/// across a redefinition and an undefinition, with arguments that span
/// lines and a built-in token among them.
const TRACE_INPUT: &str = "changequote([,])dnl
define([AC_INIT], [init($@)])dnl
AC_INIT([pkg], [1.0])
AC_INIT
AC_INIT()
define([X], [x])X([a
b], [c])
define([AC_INIT], [again])AC_INIT([z], defn([len]))
undefine([AC_INIT])AC_INIT(1)
define([AC_INIT],[third])dnl
AC_INIT(2, [3])
len(AC_INIT(4))
";

/// autom4te: `--debug=aflq --trace=NAME...`, read back from `--debugfile`.
#[test]
fn trace_lines_as_autom4te_reads_them() {
    let dir = plib::tmp::tempdir().unwrap();
    let debugfile = dir.path().join("traces");
    let debug_arg = format!("--debugfile={}", debugfile.display());
    m4(
        &["--debug=aflq", "--trace=AC_INIT", "--trace=X", &debug_arg],
        TRACE_INPUT,
        "init(pkg,1.0)\ninit()\ninit()\nx\nagain\nAC_INIT(1)\nthird\n5\n",
        "",
        0,
    );
    assert_eq!(
        std::fs::read_to_string(&debugfile).unwrap(),
        "m4trace:stdin:3: -1- AC_INIT([pkg], [1.0])
m4trace:stdin:4: -1- AC_INIT
m4trace:stdin:5: -1- AC_INIT([])
m4trace:stdin:6: -1- X([a
b], [c])
m4trace:stdin:8: -1- AC_INIT([z], [])
m4trace:stdin:11: -1- AC_INIT([2], [3])
m4trace:stdin:12: -2- AC_INIT([4])
"
    );
}

/// libtoolize: `m4 -daq --prefix --trace=NAME - FILE`, traces on stderr.
#[test]
fn trace_lines_as_libtoolize_reads_them() {
    m4(
        &["-daq", "--prefix", "--trace=AC_CONFIG_MACRO_DIRS", "-"],
        "m4_changequote([,])m4_define([AC_DEFUN], [m4_define([$1])])\
         AC_DEFUN([AC_CONFIG_MACRO_DIRS])AC_CONFIG_MACRO_DIRS([m4], [build-aux])\n",
        "\n",
        "m4trace: -1- AC_CONFIG_MACRO_DIRS([m4], [build-aux])\n",
        0,
    );
}

#[test]
fn trace_shows_expansions_and_builtin_arguments() {
    let input = "define(`foo',`[$1]')dnl
foo(`a',`b') len(`xyz')
traceon(`len')len(foo(`q'))
define(`d',defn(`define'))d(`e',`E')e
";
    m4(
        &["-d", "-tfoo", "-td"],
        input,
        "[a] 3\n3\nE\n",
        "m4trace: -1- foo(`a', `b') -> `[a]'
m4trace: -2- foo(`q') -> `[q]'
m4trace: -1- len(`[q]') -> `3'
m4trace: -1- d(`e', `E')
",
        0,
    );
    m4(
        &["-dq", "--trace=define"],
        "define(`d',defn(`define'))\n",
        "\n",
        "m4trace: -1- define\n",
        0,
    );
    m4(
        &["-daq", "--trace=define"],
        "define(`d',defn(`define'))\n",
        "\n",
        "m4trace: -1- define(`d', <define>)\n",
        0,
    );
}

#[test]
fn trace_flag_t_traces_every_call() {
    m4(
        &["-dtl"],
        "define(`x',`len(ab)')x\n",
        "2\n",
        "m4trace:1: -1- define\nm4trace:1: -1- x\nm4trace:1: -1- len\n",
        0,
    );
}

// ---------------------------------------------------------------------------
// GNU m4 semantics autoconf's output depends on
// ---------------------------------------------------------------------------

#[test]
fn engine_semantics_m4sugar_relies_on() {
    m4(
        &[],
        "define(`w', `len(a,b)')dnl
1 w(x,
)
2 [shift(`x',`a',`')] define(`a',`A')
3 changequote(`')`a'changequote
4 define(`f',`[$1]')f(defn(`len'))
5 changequote([,])define([g],[$10|$11|$1])g(1,2,3,4,5,6,7,8,9,ten,eleven)
6 translit([hello], [a-z], [A-Z]) translit([a-b], [a-], [X_]) eval([0r36:z])
",
        "1 1\n2 [a,] \n3 `A'\n4 []\n5 ten|eleven|1\n6 HELLO X_b 35\n",
        // The warning names the line of the call whose expansion made it.
        "m4:stdin:2: Warning: excess arguments to builtin `len' ignored\n",
        0,
    );
}

// Multi-character quotes, as m4sugar's m4_split sets them: a partial match
// of the quote string is text.
#[test]
fn multi_character_quotes() {
    m4(
        &[],
        "changequote([,])dnl
define([f],[<$1|$2|$3>])dnl
changequote([-=<{(], [)}>=-])dnl
f(-=<{(-=<{( a b )}>=-)}>=-, -=<{([\t ]+)}>=-, -=<{(], [x], [79])[][]g([)}>=-)
patsubst(-=<{(-=<{( a b )}>=-)}>=-, -=<{([\t ]+)}>=-, -=<{(]+[)}>=-)
",
        "< a b |[\t ]+|], [x], [79])[][]g([>\n]+[a]+[b]+[\n",
        "",
        0,
    );
}

// Bytes 1 and 2 are text: m4sugar builds a string of every byte.
#[test]
fn control_bytes_are_text() {
    m4(&[], "format(`%c%c|', 1, 2)\n", "\u{1}\u{2}|\n", "", 0);
}

// m4wrap text runs in the diversion current at the end of input.
#[test]
fn m4wrap_text_runs_in_the_current_diversion() {
    m4(&[], "m4wrap(`wrapped')divert(-1)\n", "", "", 0);
    m4(&[], "m4wrap(`__line__')\n\n", "\n\n1", "", 0);
}
