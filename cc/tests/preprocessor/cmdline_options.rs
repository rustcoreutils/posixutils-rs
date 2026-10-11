//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Preprocessor options from gcc's command line: `-undef` and `-imacros`.
//

use crate::common::{preprocess_text, run_c17};

/// A scratch directory with `name` holding `text`.
fn scratch(name: &str, text: &str) -> (plib::tmp::TempDir, String) {
    let dir = plib::tmp::Builder::new()
        .prefix("c17_pp_cmdline_")
        .tempdir()
        .unwrap();
    let path = dir.path().join(name);
    std::fs::write(&path, text).unwrap();
    (dir, path.to_str().unwrap().to_string())
}

/// The `#define` lines `-dM` prints for an empty unit under `args`.
fn predefined(args: &[&str], lang: &str) -> Vec<String> {
    let (_dir, empty) = scratch(&format!("empty.{lang}"), "");
    let mut argv = vec!["-dM", "-E"];
    argv.extend(args);
    argv.push(&empty);
    let r = run_c17(&argv);
    assert!(r.success, "{args:?}: {}", r.stderr);
    r.stdout.lines().map(str::to_string).collect()
}

/// `-undef` leaves only the macros the C standard names: gcc's `-undef -dM`
/// is `__STDC__`, `__STDC_VERSION__`, `__STDC_HOSTED__` and the
/// `__STDC_*` feature macros, and nothing of the target, the system, gcc
/// or the options -- glibc preprocesses its version scripts and
/// `shlib-versions` with `-undef` so that `linux` and `unix` stay words.
/// A `-D`, `-pthread`'s `_REENTRANT` among them, still defines its macro.
#[test]
fn cmdline_options_undef_keeps_only_the_standard_macros() {
    let lines = predefined(
        &[
            "-undef",
            "-O2",
            "-fPIC",
            "-fstack-protector",
            "-fexceptions",
            "-fsignaling-nans",
            "-pthread",
            "-DUSER=1",
        ],
        "c",
    );
    for line in &lines {
        assert!(
            line.starts_with("#define __STDC")
                || line == "#define _REENTRANT 1"
                || line == "#define USER 1",
            "{line}\n{lines:#?}"
        );
    }
    for want in [
        "#define __STDC__ 1",
        "#define __STDC_HOSTED__ 1",
        "#define __STDC_VERSION__ 201710L",
        "#define _REENTRANT 1",
        "#define USER 1",
    ] {
        assert!(lines.iter().any(|l| l == want), "{want}\n{lines:#?}");
    }
    // Without it, the system's macros are there.
    let lines = predefined(&[], "c");
    assert!(lines.iter().any(|l| l.starts_with("#define __GNUC__ ")));
}

/// The dynamic macros are not predefined macros to `-undef`: `__LINE__`,
/// `__FILE__` and `__has_include` still answer, as in gcc, while `linux`,
/// `unix` and `__GNUC__` are plain identifiers.
#[test]
fn cmdline_options_undef_keeps_the_dynamic_macros() {
    let r = preprocess_text(
        "undef_dynamic.c",
        "__LINE__ linux unix __GNUC__ __STDC_VERSION__\n\
         #if defined(__has_include) && __has_include(<stddef.h>)\nhas\n#endif\n",
        &["-P", "-undef"],
    );
    assert!(r.success, "{}", r.stderr);
    let words: Vec<&str> = r.stdout.split_whitespace().collect();
    assert_eq!(
        words,
        ["1", "linux", "unix", "__GNUC__", "201710L", "has"],
        "{}",
        r.stdout
    );
}

/// In a `.S` file `-undef` leaves `__ASSEMBLER__`, the one macro gcc
/// defines for assembly beyond the standard's.
#[test]
fn cmdline_options_undef_keeps_assembler_in_a_dot_s() {
    let (_dir, src) = scratch(
        "u.S",
        "__ASSEMBLER__ __OPTIMIZE__ __PIC__ __GNUC__ __x86_64__ __aarch64__ linux\n",
    );
    let r = run_c17(&["-E", "-P", "-undef", "-O2", "-fPIC", &src]);
    assert!(r.success, "{}", r.stderr);
    assert_eq!(
        r.stdout.trim(),
        "1 __OPTIMIZE__ __PIC__ __GNUC__ __x86_64__ __aarch64__ linux"
    );
}

/// `-imacros FILE` reads FILE for its macros and throws its text away, before
/// the `-include` files, which see what it defined -- gcc's order. The
/// option takes its file joined or as the next word, a missing file is
/// fatal, and the file is a dependency like any header.
#[test]
fn cmdline_options_imacros_takes_the_macros_only() {
    let (dir, macros) = scratch("im.h", "#define M 42\nint from_imacros;\n");
    let inc = dir.path().join("inc.h");
    std::fs::write(&inc, "int from_include = M;\n").unwrap();
    let inc = inc.to_str().unwrap();
    let joined = format!("-imacros{macros}");
    for spelling in [vec!["-imacros", macros.as_str()], vec![joined.as_str()]] {
        let mut args = vec!["-P", "-include", inc];
        args.extend(&spelling);
        let r = preprocess_text("imacros.c", "int v = M;\n", &args);
        assert!(r.success, "{spelling:?}: {}", r.stderr);
        let text: Vec<&str> = r.stdout.lines().filter(|l| !l.is_empty()).collect();
        assert_eq!(
            text,
            ["int from_include = 42;", "int v = 42;"],
            "{spelling:?}"
        );
    }

    let r = preprocess_text("imacros_m.c", "int v = M;\n", &["-M", "-imacros", &macros]);
    assert!(r.success, "{}", r.stderr);
    assert!(r.stdout.contains("im.h"), "{}", r.stdout);

    let r = preprocess_text("imacros_missing.c", "M\n", &["-imacros", "no-such-c17.h"]);
    assert!(!r.success);
    assert!(r.stderr.contains("no-such-c17.h"), "{}", r.stderr);
}

/// `-imacros` serves a `.S` file as it does C.
#[test]
fn cmdline_options_imacros_in_a_dot_s() {
    let (dir, macros) = scratch("asm.h", "#define VALUE 0x2a\n");
    let src = dir.path().join("v.S");
    std::fs::write(&src, "\t.long VALUE\n").unwrap();
    let r = run_c17(&["-E", "-P", "-imacros", &macros, src.to_str().unwrap()]);
    assert!(r.success, "{}", r.stderr);
    assert_eq!(r.stdout.trim(), ".long 0x2a", "{}", r.stdout);
}

/// Comments in text lines, in directives, in a skipped group, and around
/// tokens on a line, for `-C` and `-CC`.
const COMMENTED: &str = "/* head\n   two */\n#define G 1 /* g */\n#if 1 /* cond */\n\
                         int y = G; /* after G */ // tail\n#endif /* end */\n#if 0\n\
                         /* skipped */\n#endif\nint z;\t/* tab */ int w;\n  /* indented */ int v;\n";

/// `-C` keeps every comment of the text where it stands, as gcc does, and
/// drops those of the directives and of a skipped group with them; a comment
/// across lines keeps the lines after it where they were. `-CC` also keeps
/// the comments of a macro's definition, in its expansions. (gcc's output,
/// less the comments of `<stdc-predef.h>`, which gcc includes and c17 does
/// not.)
#[test]
fn cmdline_options_c_keeps_comments() {
    let r = preprocess_text("comments.c", COMMENTED, &["-C", "-P"]);
    assert!(r.success, "{}", r.stderr);
    assert_eq!(
        r.stdout,
        "/* head\n   two */\nint y = 1; /* after G */ // tail\n\
         int z; /* tab */ int w;\n  /* indented */ int v;\n"
    );
    let r = preprocess_text("comments.c", COMMENTED, &["-CC", "-P"]);
    assert!(r.success, "{}", r.stderr);
    assert_eq!(
        r.stdout,
        "/* head\n   two */\nint y = 1 /* g */; /* after G */ // tail\n\
         int z; /* tab */ int w;\n  /* indented */ int v;\n"
    );
    // With markers, every line of text is on its own line number.
    let r = preprocess_text("comments.c", COMMENTED, &["-C"]);
    assert!(r.success, "{}", r.stderr);
    let text: Vec<&str> = r.stdout.lines().filter(|l| !l.starts_with('#')).collect();
    assert_eq!(
        text,
        [
            "/* head",
            "   two */",
            "",
            "",
            "int y = 1; /* after G */ // tail",
            "",
            "",
            "",
            "",
            "int z; /* tab */ int w;",
            "  /* indented */ int v;",
        ],
        "{}",
        r.stdout
    );
}

/// `-CC` keeps a macro definition's comments in its expansion, a `//` one
/// rewritten as `/* */` so that the line it lands on goes on; `-C` drops
/// them with the directive.
#[test]
fn cmdline_options_cc_keeps_comments_in_macros() {
    let src = "#define F(a, b) a /* in body */ + b // line in body\n\
               #define H /* only */\nint x = F(1, 2);\nint h = H;\n";
    let r = preprocess_text("macro_comments.c", src, &["-CC", "-P"]);
    assert!(r.success, "{}", r.stderr);
    assert_eq!(
        r.stdout,
        "int x = 1 /* in body */ + 2 /* line in body*/;\nint h = /* only */;\n"
    );
    let r = preprocess_text("macro_comments.c", src, &["-C", "-P"]);
    assert!(r.success, "{}", r.stderr);
    assert_eq!(r.stdout, "int x = 1 + 2;\nint h = ;\n");
}

/// `-C` keeps the comments among a macro's arguments, as gcc's does: they
/// are tokens of the argument, so they stand wherever it is substituted,
/// once per use, are spelled by `#`, and make a variadic part non-empty. A
/// `//` comment becomes a `/* */` one, since the expansion goes on past its
/// line, with each `/` beside a `*` in its text made `|` so the comment
/// cannot end early or nest -- in a macro body under `-CC` too. c17 dropped
/// every comment in an argument.
#[test]
fn cmdline_options_c_keeps_comments_in_macro_arguments() {
    let src = "#define S(x) #x\n\
               #define D(x) x x\n\
               #define V(a, ...) a __VA_OPT__(has) __VA_ARGS__\n\
               #define I(x) x\n\
               #define F(x, y) x + y\n\
               #define B 1 // a/*b*/c\n\
               s S(a /* c */ b);\n\
               d D(1 /* c */);\n\
               v V(1, /* c */);\n\
               i I(I(/* in */ x));\n\
               c I(1 // a/*b*/c\n  );\n\
               int a = F(1 /* one */, /* two */ 2);\n\
               int d = I(3 /* multi\nline */ );\n\
               b B;\n";
    let args = "s \"a /* c */ b\";\n\
                d 1 /* c */ 1 /* c */;\n\
                v 1 has /* c */;\n\
                i /* in */ x;\n\
                c 1 /* a|*b*|c*/;\n\
                int a = 1 /* one */ + /* two */ 2;\n\
                int d = 3 /* multi\nline */;\n";
    for (flag, b) in [("-C", "b 1;\n"), ("-CC", "b 1 /* a|*b*|c*/;\n")] {
        let r = preprocess_text("arg_comments.c", src, &[flag, "-P"]);
        assert!(r.success, "{flag}: {}", r.stderr);
        assert_eq!(r.stdout, format!("{args}{b}"), "{flag}");
    }
}

/// Without `-E` there is no text for a comment to be kept in, and gcc takes
/// `-C` and `-CC` regardless.
#[test]
fn cmdline_options_c_without_e_compiles() {
    let (dir, src) = scratch("c.c", COMMENTED);
    let obj = dir.path().join("c.o");
    for flag in ["-C", "-CC"] {
        let r = run_c17(&[flag, "-c", "-o", obj.to_str().unwrap(), &src]);
        assert!(r.success, "{flag}: {}", r.stderr);
    }
}

/// `-isystemDIR`, `-idirafterDIR` and `-includeFILE` are taken joined as
/// gcc takes them, as `-iquoteDIR` and `-imacrosFILE` already were; c17
/// refused the three as unrecognized options.
#[test]
fn cmdline_options_joined_include_options() {
    let (dir, header) = scratch("joined.h", "#define FROM_JOINED 7\n");
    let dir_s = dir.path().to_str().unwrap();
    let src = "#ifdef FROM_JOINED\nseen FROM_JOINED\n#endif\n";
    let include_src = "#include <joined.h>\nseen FROM_JOINED\n";
    for (option, value, text) in [
        ("-isystem", dir_s, include_src),
        ("-idirafter", dir_s, include_src),
        ("-include", header.as_str(), src),
    ] {
        let joined = format!("{option}{value}");
        for spelling in [vec![option, value], vec![joined.as_str()]] {
            let mut args = vec!["-P"];
            args.extend(&spelling);
            let r = preprocess_text("joined_inc.c", text, &args);
            assert!(r.success, "{spelling:?}: {}", r.stderr);
            assert_eq!(r.stdout.trim(), "seen 7", "{spelling:?}");
        }
    }
}
