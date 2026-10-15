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

use plib::testing::{run_test, run_test_base, run_test_base_with_env, run_test_with_env, TestPlan};

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

// ---------------------------------------------------------------------------
// Frozen state files
// ---------------------------------------------------------------------------

/// A frozen file in GNU m4 1.4.19's version 1 format: quotes, comments, a
/// pushdef stack of text with a newline in it, built-ins under their own
/// names and a copied one, a diversion, and diversion -1 current.
const FROZEN: &str = "# This is a frozen state file generated by GNU M4 1.4.19
V1
Q1,1
[]
C2,2
/**/
F6,6
definedefine
F6,6
divertdivert
F6,6
popdefpopdef
F2,3
cplen
T3,6
fooFoo $1
T3,8
fooBar
line
D2,9
diverted

D-1,0

# End of frozen state file
";

const FROZEN_INPUT: &str =
    "hidden\ndivert(0)foo /*foo*/ [foo] popdef([foo])foo(x) cp(abc) len(abc)\n";

// What GNU m4 1.4.19 makes of FROZEN_INPUT after reloading FROZEN: the
// table is exactly the file's, so `len` is not defined.
const FROZEN_OUTPUT: &str = "Bar\nline /*foo*/ foo Foo x 3 len(abc)\ndiverted\n";

#[test]
fn reload_state_restores_a_gnu_frozen_file() {
    let dir = plib::tmp::tempdir().unwrap();
    let frozen = dir.path().join("state.m4f");
    std::fs::write(&frozen, FROZEN).unwrap();
    let arg = format!("--reload-state={}", frozen.display());
    m4(&[&arg], FROZEN_INPUT, FROZEN_OUTPUT, "", 0);
    let arg = frozen.display().to_string();
    m4(&["-R", &arg], FROZEN_INPUT, FROZEN_OUTPUT, "", 0);
}

// autom4te reloads, then undefines, then reads its files.
#[test]
fn reload_state_comes_before_undefine() {
    let dir = plib::tmp::tempdir().unwrap();
    let frozen = dir.path().join("state.m4f");
    std::fs::write(&frozen, FROZEN).unwrap();
    let arg = format!("--reload-state={}", frozen.display());
    m4(
        &[&arg, "--undefine=foo"],
        "divert(0)foo cp(ab)\n",
        "foo 2\ndiverted\n",
        "",
        0,
    );
}

#[test]
fn freeze_then_reload_gives_the_same_state() {
    let dir = plib::tmp::tempdir().unwrap();
    let frozen = dir.path().join("out.m4f");
    let arg = format!("--freeze-state={}", frozen.display());
    // Freezing writes no diversions to the output: they go in the file.
    m4(
        &[&arg],
        "changequote([,])define([foo],[Foo $1])pushdef([foo],[Bar\nline])\
         define([cp],defn([len]))undefine([eval])divert(2)diverted\n\
         divert(-1)changecom([/*],[*/])\n",
        "",
        "",
        0,
    );
    let text = std::fs::read(&frozen).unwrap();
    for directive in [
        &b"\nV1\n"[..],
        b"\nQ1,1\n[]\n",
        b"\nC2,2\n/**/\n",
        b"\nT3,6\nfooFoo $1\nT3,8\nfooBar\nline\n",
        b"\nF2,3\ncplen\n",
        b"\nD2,9\ndiverted\n\nD-1,0\n\n",
    ] {
        assert!(
            text.windows(directive.len()).any(|w| w == directive),
            "{:?} not in the frozen file:\n{}",
            String::from_utf8_lossy(directive),
            String::from_utf8_lossy(&text)
        );
    }
    let arg = format!("--reload-state={}", frozen.display());
    m4(
        &[&arg],
        "hidden\ndivert(0)foo /*foo*/ [foo] popdef([foo])foo(x) cp(abc) eval(1)\n",
        "Bar\nline /*foo*/ foo Foo x 3 eval(1)\ndiverted\n",
        "",
        0,
    );
}

#[test]
fn frozen_file_of_another_version_is_refused() {
    let dir = plib::tmp::tempdir().unwrap();
    let frozen = dir.path().join("v2.m4f");
    std::fs::write(&frozen, "V2\n").unwrap();
    let name = frozen.display().to_string();
    m4(
        &["-R", &name],
        "x\n",
        "",
        &format!("m4:{name}:1: frozen file version 2 greater than max supported of 1\n"),
        63,
    );
}

// autom4te refuses to run an m4 whose --help does not mention it.
#[test]
fn help_lists_reload_state() {
    let (out, _, status) = m4_output(&["--help"], "");
    assert_eq!(status, Some(0));
    assert!(out.contains("--reload-state"), "{out}");
    assert!(out.contains("--freeze-state"), "{out}");
}

// ---------------------------------------------------------------------------
// GNU m4 1.4 by default, POSIX under POSIXLY_CORRECT
// ---------------------------------------------------------------------------

/// Run m4 on `stdin` with the environment `env` and hand back stdout as
/// bytes, requiring an empty stderr and a zero exit status.
fn m4_env_stdout(env: &[(&str, &str)], stdin: &str) -> Vec<u8> {
    let out = run_test_base_with_env("m4", &[], stdin.as_bytes(), env);
    assert_eq!(String::from_utf8_lossy(&out.stderr), "");
    assert_eq!(out.status.code(), Some(0));
    out.stdout
}

const MULTIBYTE_COUNTING: &str = "len(é) index(aéb,b) substr(aéb,1,1)|substr(aéb,2) \
                                  translit(aéb,é,X) translit(xéy,a-z,A-Z)\n";

/// What GNU m4 1.4.19 gives for [`MULTIBYTE_COUNTING`], in every locale.
const MULTIBYTE_COUNTING_BYTES: &[u8] = b"2 3 \xc3|\xa9b aXb X\xc3\xa9Y\n";

// GNU m4 1.4.19 counts bytes in every locale.  tar's testsuite, which
// autotest generates from UTF-8 text with m4sugar's `m4_len` and
// `m4_substr`, came out with a broken test group when m4 counted characters.
#[test]
fn len_index_substr_translit_count_bytes_by_default() {
    assert_eq!(
        m4_env_stdout(&[("LC_ALL", "C")], MULTIBYTE_COUNTING),
        MULTIBYTE_COUNTING_BYTES
    );
    if let Some(utf8) = plib::testing::utf8_locale() {
        assert_eq!(
            m4_env_stdout(&[("LC_ALL", &utf8)], MULTIBYTE_COUNTING),
            MULTIBYTE_COUNTING_BYTES
        );
    }
}

// POSIX: len, index, substr and translit count characters (LC_CTYPE).
#[test]
fn len_index_substr_translit_count_characters_under_posixly_correct() {
    // In the C locale a character is a byte.  Any value turns it on.
    assert_eq!(
        m4_env_stdout(
            &[("LC_ALL", "C"), ("POSIXLY_CORRECT", "")],
            MULTIBYTE_COUNTING
        ),
        MULTIBYTE_COUNTING_BYTES
    );
    let Some(utf8) = plib::testing::utf8_locale() else {
        eprintln!("no UTF-8 locale installed: skipping the multibyte half");
        return;
    };
    assert_eq!(
        m4_env_stdout(
            &[("LC_ALL", &utf8), ("POSIXLY_CORRECT", "1")],
            MULTIBYTE_COUNTING
        ),
        "1 2 é|b aXb XéY\n".as_bytes()
    );
}

/// Run m4 on `stdin` with `env` set and require exactly this stdout,
/// stderr and exit status.
fn m4_env(env: &[(&str, &str)], stdin: &str, stdout: &str, stderr: &str, status: i32) {
    run_test_with_env(
        TestPlan {
            cmd: String::from("m4"),
            args: Vec::new(),
            stdin_data: String::from(stdin),
            expected_out: String::from(stdout),
            expected_err: String::from(stderr),
            expected_exit_code: status,
        },
        env,
    );
}

// Wrapped text queued while wrapped text runs is a second round, ordered the
// same way.
const M4WRAP_ORDER: &str =
    "m4wrap(`W1 ')m4wrap(`W2 ')m4wrap(`[a m4wrap(`c ')m4wrap(`d ')] ')m4wrap(`W3 ')body\n";

// GNU m4 1.4 runs m4wrap text last in, first out.
#[test]
fn m4wrap_runs_last_in_first_out_by_default() {
    m4(&[], M4WRAP_ORDER, "body\nW3 [a ] W2 W1 d c ", "", 0);
    // The pieces of a round are read as one text, so a call can begin in
    // one and end in the next.
    m4(
        &[],
        "define(`x', `<$1>')m4wrap(`)')m4wrap(`x(')body\n",
        "body\n<>",
        "",
        0,
    );
}

// POSIX: "in the order in which the m4wrap macros were processed".
#[test]
fn m4wrap_runs_in_order_under_posixly_correct() {
    m4_env(
        &[("POSIXLY_CORRECT", "1")],
        M4WRAP_ORDER,
        "body\nW1 W2 [a ] W3 c d ",
        "",
        0,
    );
}

/// autoconf-dickey's `m4_init`, which ncurses' configure is generated with,
/// reduced to the macros it uses: it wraps a check that every
/// `m4_divert_push` was popped, then pushes `KILL` and wraps the pop, which
/// must run first.  autoconf-dickey does not pass `--gnu`.
const DICKEY_M4_INIT: &str = "changequote([,])dnl
define([_m4_divert(KILL)], -1)dnl
define([_m4_divert], [ifdef([_m4_divert($1)], [indir([_m4_divert($1)])], [$1])])dnl
define([m4_divert_push], [pushdef([_m4_divert_diversion], [$1])\
divert(_m4_divert(_m4_divert_diversion))])dnl
define([m4_divert_pop], [popdef([_m4_divert_diversion])\
divert(ifdef([_m4_divert_diversion], [_m4_divert(_m4_divert_diversion)], -1))])dnl
define([m4_init], [m4wrap([ifdef([_m4_divert_diversion], [errprint([m4_init: unbalanced m4_divert_push
])m4exit(1)])[]])dnl
m4_divert_push([KILL])
m4wrap([m4_divert_pop([KILL])[]])])dnl
before
m4_init()dnl
killed
";

#[test]
fn autoconf_dickey_m4_init_balances() {
    m4(&[], DICKEY_M4_INIT, "before\n", "", 0);
    // In POSIX order the check runs before the pop it checks for.
    m4_env(
        &[("POSIXLY_CORRECT", "1")],
        DICKEY_M4_INIT,
        "before\n",
        "m4_init: unbalanced m4_divert_push\n",
        1,
    );
}
