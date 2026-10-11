//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// The `[-W<name>]` tag gcc puts after a diagnostic an option controls: the
// warning's group, `-Wpedantic` for a pedantic one, the group again when
// `-pedantic-errors` makes it an error, and nothing for a warning no option
// names -- or for any under `-fno-diagnostics-show-option`. Checked against
// gcc 13.
//

use crate::test_compile::compile;

#[track_caller]
fn has(name: &str, src: &str, flags: &[&str], line: &str) {
    let c = compile(name, src, flags);
    assert!(
        c.stderr.lines().any(|l| l == format!("{name}.c:{line}")),
        "expected {line:?} under {flags:?} in:\n{}",
        c.stderr
    );
}

const INT_CONVERSION: &str = "int *p = 5;\n";
const INT_CONVERSION_MSG: &str =
    "1:10: {}: initialization of 'int *' from 'int' makes pointer from integer without a cast";

fn int_conversion(level: &str, tag: &str) -> String {
    format!("{}{tag}", INT_CONVERSION_MSG.replace("{}", level))
}

/// A warning in a group names it, as warning or as the error `-Werror`
/// makes of it; `-pedantic-errors` names the group without `error=`.
#[test]
fn group_warnings_name_their_option() {
    has(
        "tag_group",
        INT_CONVERSION,
        &[],
        &int_conversion("warning", " [-Wint-conversion]"),
    );
    has(
        "tag_werror",
        INT_CONVERSION,
        &["-Werror"],
        &int_conversion("error", " [-Werror=int-conversion]"),
    );
    has(
        "tag_pedantic_errors",
        INT_CONVERSION,
        &["-pedantic-errors"],
        &int_conversion("error", " [-Wint-conversion]"),
    );
}

/// A pedantic warning is `-Wpedantic`'s; a default pedwarn with no option
/// has no tag, as warning or as error.
#[test]
fn pedwarns_name_pedantic_or_nothing() {
    let label = "void f(void) { l: }\n";
    has(
        "tag_pedantic",
        label,
        &["-pedantic"],
        "1:19: warning: label at end of compound statement [-Wpedantic]",
    );
    has(
        "tag_pedantic_e",
        label,
        &["-pedantic-errors"],
        "1:19: error: label at end of compound statement [-Wpedantic]",
    );
    has(
        "tag_werror_pedantic",
        label,
        &["-Werror=pedantic"],
        "1:19: error: label at end of compound statement [-Werror=pedantic]",
    );
    let excess = "int x = {1, 2};\n";
    has(
        "tag_none",
        excess,
        &[],
        "1:9: warning: excess elements in scalar initializer",
    );
    has(
        "tag_none_e",
        excess,
        &["-pedantic-errors"],
        "1:9: error: excess elements in scalar initializer",
    );
    has(
        "tag_none_werror",
        excess,
        &["-Werror"],
        "1:9: error: excess elements in scalar initializer [-Werror]",
    );
}

/// `-fno-diagnostics-show-option` leaves every tag off, `-Werror`'s too.
#[test]
fn no_show_option_drops_the_tags() {
    let flag = "-fno-diagnostics-show-option";
    has(
        "tag_off",
        INT_CONVERSION,
        &[flag],
        &int_conversion("warning", ""),
    );
    has(
        "tag_off_werror",
        INT_CONVERSION,
        &[flag, "-Werror"],
        &int_conversion("error", ""),
    );
    has(
        "tag_off_unnamed",
        "int x = {1, 2};\n",
        &[flag, "-Werror"],
        "1:9: error: excess elements in scalar initializer",
    );
}

/// The warnings gcc puts in a group of their own: `#else`/`#endif`
/// labels, a built-in macro redefined, a K&R parameter left `int`, and a
/// `scalar_storage_order` pragma gcc cannot read.
#[test]
fn warnings_in_gcc_groups() {
    has(
        "tag_endif",
        "#if 1\n#else X\n#endif X\n#ifdef A b\n#endif\n",
        &[],
        "2:7: warning: extra tokens at end of #else directive [-Wendif-labels]",
    );
    has(
        "tag_endif2",
        "#if 1\n#endif X\n",
        &[],
        "2:8: warning: extra tokens at end of #endif directive [-Wendif-labels]",
    );
    has(
        "tag_ifdef",
        "#ifdef A b\n#endif\n",
        &[],
        "1:10: warning: extra tokens at end of #ifdef directive",
    );
    has(
        "tag_builtin_macro",
        "#define __FILE__ 2\n",
        &[],
        "1:9: warning: '__FILE__' redefined: it is a built-in macro [-Wbuiltin-macro-redefined]",
    );
    has(
        "tag_undef_builtin",
        "#undef __TIME__\n#undef __LINE__\n",
        &[],
        "1:1: warning: undefining \"__TIME__\" [-Wbuiltin-macro-redefined]",
    );
    has(
        "tag_undef_line",
        "#undef __LINE__\n",
        &[],
        "1:1: warning: undefining \"__LINE__\"",
    );
    has(
        "tag_knr",
        "int f(a, b) int a; { return a + b; }\n",
        &[],
        "1:20: warning: type of 'b' defaults to 'int' [-Wimplicit-int]",
    );
    has(
        "tag_sso",
        "#pragma scalar_storage_order\nint x;\n",
        &[],
        "1:9: warning: missing 'big-endian', 'little-endian', or 'default' after \
         '#pragma scalar_storage_order' [-Wpragmas]",
    );
}

/// The groups are the ones `-Wno-<name>` turns off.
#[test]
fn new_groups_answer_to_their_option() {
    for (src, flag) in [
        ("#if 1\n#endif X\n", "-Wno-endif-labels"),
        ("#define __FILE__ 2\n", "-Wno-builtin-macro-redefined"),
        ("#pragma scalar_storage_order\nint x;\n", "-Wno-pragmas"),
        (
            "int f(a, b) int a; { return a + b; }\n",
            "-Wno-implicit-int",
        ),
    ] {
        let c = compile("tag_off_group", src, &[flag]);
        assert!(c.success && c.stderr.is_empty(), "{flag}:\n{}", c.stderr);
    }
}

/// gcc's preprocessor gives its pedantic warnings in no group: no tag,
/// an error under plain `-Werror` but not `-Werror=pedantic`, and silenced
/// by `-Wno-pedantic` with the rest of `-pedantic`.
#[test]
fn preprocessor_pedwarns_have_no_option() {
    let src = "char c = '\\e';\n#line 0\nint x;\n";
    let escape = "1:10: warning: non-ISO-standard escape sequence, '\\e'";
    has("tag_cpp", src, &["-pedantic"], escape);
    has(
        "tag_cpp_line",
        src,
        &["-pedantic"],
        "2:7: warning: line number out of range",
    );
    has(
        "tag_cpp_named",
        src,
        &["-pedantic", "-Werror=pedantic"],
        escape,
    );
    has(
        "tag_cpp_werror",
        src,
        &["-pedantic", "-Werror"],
        "1:10: error: non-ISO-standard escape sequence, '\\e' [-Werror]",
    );
    let c = compile("tag_cpp_off", src, &["-pedantic", "-Wno-pedantic"]);
    assert!(c.success && c.stderr.is_empty(), "{}", c.stderr);
}

/// gcc 13's words for a `#pragma pack(pop)` with nothing pushed.
#[test]
fn pack_pop_without_push() {
    has(
        "tag_pack_pop",
        "#pragma pack(pop)\nint x;\n",
        &[],
        "2:6: warning: '#pragma pack (pop)' encountered without matching \
         '#pragma pack (push)' [-Wpragmas]",
    );
}
