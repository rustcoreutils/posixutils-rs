//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// gcc's path prefix maps: `-fdebug-prefix-map`, `-fmacro-prefix-map` and
// `-ffile-prefix-map`, which Debian's default CFLAGS pass so that the build
// directory stays out of the objects.
//

use std::path::{Path, PathBuf};
use std::process::{Command, Output};

/// A scratch directory holding `t.c` with `src`, by its canonical path:
/// the compilation directory c17 records is the canonical one, and the test
/// must name the same string in its maps.
fn scratch(tag: &str, src: &str) -> (plib::tmp::TempDir, PathBuf) {
    let dir = plib::tmp::Builder::new()
        .prefix(&format!("c17_prefix_map_{tag}_"))
        .tempdir()
        .expect("tempdir");
    let canon = dir.path().canonicalize().expect("canonicalize");
    std::fs::write(canon.join("t.c"), src).expect("write");
    (dir, canon)
}

/// Run `c17` with `args` from `dir`.
fn c17_in(dir: &Path, args: &[&str]) -> Output {
    Command::new(env!("CARGO_BIN_EXE_c17"))
        .args(args)
        .current_dir(dir)
        .output()
        .expect("failed to run c17")
}

/// `c17 -S` of `t.c` in `dir`, by its absolute path, with `flags`; the
/// assembly.
fn asm_in(dir: &Path, flags: &[&str]) -> String {
    let src = dir.join("t.c");
    let out = dir.join("t.s");
    let mut args = flags.to_vec();
    args.extend(["-S", "-o", out.to_str().unwrap(), src.to_str().unwrap()]);
    let r = c17_in(dir, &args);
    assert!(
        r.status.success(),
        "{flags:?}: {}",
        String::from_utf8_lossy(&r.stderr)
    );
    std::fs::read_to_string(out).expect("read asm")
}

/// Build `t.c` in `dir` with `flags`, run it, and return what it printed.
fn run_in(dir: &Path, flags: &[&str]) -> String {
    let src = dir.join("t.c");
    let exe = dir.join("t");
    let mut args = flags.to_vec();
    args.extend(["-o", exe.to_str().unwrap(), src.to_str().unwrap()]);
    let r = c17_in(dir, &args);
    assert!(
        r.status.success(),
        "{flags:?}: {}",
        String::from_utf8_lossy(&r.stderr)
    );
    let out = Command::new(&exe).output().expect("run");
    assert!(out.status.success());
    String::from_utf8_lossy(&out.stdout).into_owned()
}

const PRINT_FILE: &str = "#include <stdio.h>\n\
    int main(void) { puts(__FILE__); puts(__BASE_FILE__); return 0; }\n";

const PLAIN: &str = "int f(int x) { return x + 1; }\n";

#[test]
fn prefix_map_debug_maps_comp_dir_name_and_file_directives() {
    let (_g, dir) = scratch("debug", PLAIN);
    let d = dir.to_str().unwrap();
    let asm = asm_in(&dir, &["-g", &format!("-fdebug-prefix-map={d}=/X")]);
    assert!(asm.contains(".file 1 \"/X/t.c\""), "{asm}");
    // DW_AT_name is the mapped source path; DW_AT_comp_dir the mapped cwd.
    assert!(asm.contains("\"/X/t.c\""), "{asm}");
    assert!(asm.contains(".asciz \"/X\""), "{asm}");
    assert!(!asm.contains(d), "the build directory leaked:\n{asm}");
}

#[test]
fn prefix_map_debug_leaves_file_macro_alone() {
    let (_g, dir) = scratch("debug_macro", PRINT_FILE);
    let d = dir.to_str().unwrap();
    let out = run_in(&dir, &[&format!("-fdebug-prefix-map={d}=/X")]);
    assert_eq!(out, format!("{d}/t.c\n{d}/t.c\n"));
}

#[test]
fn prefix_map_macro_maps_file_and_base_file() {
    let (_g, dir) = scratch("macro", PRINT_FILE);
    let d = dir.to_str().unwrap();
    let flag = format!("-fmacro-prefix-map={d}=/M");
    assert_eq!(run_in(&dir, &[&flag]), "/M/t.c\n/M/t.c\n");
    // ... and not the debug information.
    let asm = asm_in(&dir, &["-g", &flag]);
    assert!(asm.contains(&format!(".file 1 \"{d}/t.c\"")), "{asm}");
}

#[test]
fn prefix_map_macro_reaches_headers_line_and_assert() {
    let (_g, dir) = scratch(
        "macro_hdr",
        "#include <stdio.h>\n#include \"h.h\"\n\
         int main(void) { puts(hdr()); puts(__FILE__);\n\
         #line 7 \"zz.c\"\n puts(__FILE__); return 0; }\n",
    );
    std::fs::write(
        dir.join("h.h"),
        "static const char *hdr(void) { return __FILE__; }\n",
    )
    .unwrap();
    let d = dir.to_str().unwrap();
    // A plain string prefix: `zz` is mapped with no directory boundary.
    let out = run_in(
        &dir,
        &[
            &format!("-fmacro-prefix-map={d}=/M"),
            "-fmacro-prefix-map=zz=Q",
        ],
    );
    assert_eq!(out, "/M/h.h\n/M/t.c\nQ.c\n");

    let (_g2, dir2) = scratch(
        "assert",
        "#include <assert.h>\nint main(void) { assert(0); return 0; }\n",
    );
    let d2 = dir2.to_str().unwrap();
    let src = dir2.join("t.c");
    let exe = dir2.join("t");
    let r = c17_in(
        &dir2,
        &[
            &format!("-fmacro-prefix-map={d2}=/A"),
            "-o",
            exe.to_str().unwrap(),
            src.to_str().unwrap(),
        ],
    );
    assert!(r.status.success(), "{}", String::from_utf8_lossy(&r.stderr));
    let out = Command::new(&exe).output().unwrap();
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(stderr.contains("/A/t.c"), "{stderr}");
    assert!(!stderr.contains(d2), "{stderr}");
}

#[test]
fn prefix_map_macro_reaches_assembler_with_cpp() {
    let (_g, dir) = scratch("asm", PLAIN);
    std::fs::write(dir.join("s.S"), "x: .asciz __FILE__\n").unwrap();
    let d = dir.to_str().unwrap();
    let src = dir.join("s.S");
    let r = c17_in(
        &dir,
        &[
            &format!("-fmacro-prefix-map={d}=/M"),
            "-E",
            src.to_str().unwrap(),
        ],
    );
    let out = String::from_utf8_lossy(&r.stdout);
    assert!(r.status.success(), "{}", String::from_utf8_lossy(&r.stderr));
    assert!(out.contains("x: .asciz \"/M/s.S\""), "{out}");
}

#[test]
fn prefix_map_file_maps_both() {
    let (_g, dir) = scratch("file", PRINT_FILE);
    let d = dir.to_str().unwrap();
    let flag = format!("-ffile-prefix-map={d}=.");
    assert_eq!(run_in(&dir, &[&flag]), "./t.c\n./t.c\n");
    let asm = asm_in(&dir, &["-g", &flag]);
    assert!(asm.contains(".file 1 \"./t.c\""), "{asm}");
    assert!(asm.contains(".asciz \".\""), "{asm}");
    assert!(!asm.contains(d), "{asm}");
}

#[test]
fn prefix_map_last_matching_option_wins_across_spellings() {
    let (_g, dir) = scratch("order", PRINT_FILE);
    let d = dir.to_str().unwrap();
    // gcc: `-ffile-prefix-map` feeds both lists in command-line order, and
    // the last option that matches is the one applied.
    let flags = [
        format!("-ffile-prefix-map={d}=."),
        format!("-fdebug-prefix-map={d}=/D"),
    ];
    let flags: Vec<&str> = flags.iter().map(String::as_str).collect();
    assert_eq!(run_in(&dir, &flags), "./t.c\n./t.c\n");
    let mut g = flags.clone();
    g.push("-g");
    let asm = asm_in(&dir, &g);
    assert!(asm.contains(".file 1 \"/D/t.c\""), "{asm}");

    // The longer prefix does not win by being longer.
    let parent = dir.parent().unwrap().to_str().unwrap();
    let flags = [
        format!("-fmacro-prefix-map={d}=/LONG"),
        format!("-fmacro-prefix-map={parent}=/SHORT"),
    ];
    let flags: Vec<&str> = flags.iter().map(String::as_str).collect();
    let name = dir.file_name().unwrap().to_str().unwrap();
    assert_eq!(
        run_in(&dir, &flags),
        format!("/SHORT/{name}/t.c\n/SHORT/{name}/t.c\n")
    );
}

#[test]
fn prefix_map_splits_at_the_last_equals() {
    let (_g, dir) = scratch("equals", PRINT_FILE);
    let sub = dir.join("d=x");
    std::fs::create_dir(&sub).unwrap();
    std::fs::write(sub.join("t.c"), PRINT_FILE).unwrap();
    let d = sub.to_str().unwrap();
    let out = run_in(&sub, &[&format!("-fmacro-prefix-map={d}=/E")]);
    assert_eq!(out, "/E/t.c\n/E/t.c\n");
}

#[test]
fn prefix_map_rejects_a_missing_equals() {
    let (_g, dir) = scratch("bad", PLAIN);
    for (flag, msg) in [
        (
            "-fdebug-prefix-map=noeq",
            "invalid argument 'noeq' to '-fdebug-prefix-map'",
        ),
        (
            "-fmacro-prefix-map=",
            "missing argument to '-fmacro-prefix-map='",
        ),
        (
            "-ffile-prefix-map=x",
            "invalid argument 'x' to '-ffile-prefix-map'",
        ),
    ] {
        let r = c17_in(&dir, &[flag, "-S", "-o", "/dev/null", "t.c"]);
        let stderr = String::from_utf8_lossy(&r.stderr);
        assert!(!r.status.success(), "{flag}: accepted");
        assert!(stderr.contains(msg), "{flag}: {stderr}");
    }
}

/// The point of the option: the same source built in two directories gives
/// the same assembly and the same object.
#[test]
fn prefix_map_file_makes_builds_reproducible() {
    let src = "#include <assert.h>\n#include <stdio.h>\n\
               int main(void) { puts(__FILE__); assert(1); return 0; }\n";
    let (_g1, d1) = scratch("repro1", src);
    let (_g2, d2) = scratch("repro2", src);
    let build = |dir: &Path| {
        let flag = format!("-ffile-prefix-map={}=.", dir.to_str().unwrap());
        for (mode, out) in [("-S", "t.s"), ("-c", "t.o")] {
            let r = c17_in(dir, &["-g", &flag, mode, "-o", out, "t.c"]);
            assert!(r.status.success(), "{}", String::from_utf8_lossy(&r.stderr));
        }
        (
            std::fs::read(dir.join("t.s")).unwrap(),
            std::fs::read(dir.join("t.o")).unwrap(),
        )
    };
    let (s1, o1) = build(&d1);
    let (s2, o2) = build(&d2);
    let (s1, s2) = (String::from_utf8_lossy(&s1), String::from_utf8_lossy(&s2));
    let differing = s1.lines().zip(s2.lines()).find(|(a, b)| a != b);
    assert!(
        s1 == s2,
        "assembly differs between build directories: {differing:?}"
    );
    assert!(o1 == o2, "objects differ between build directories");
    assert!(!s1.contains(d1.to_str().unwrap()));
}

/// A source path holding a quote or a backslash is written into the
/// assembly (`.file`, and DWARF names with `-g`) and into `__FILE__`; each
/// must be escaped. An unescaped `"` ended the `.file` string early and the
/// assembler rejected the line.
#[test]
fn prefix_map_source_path_with_quote_and_backslash() {
    let dir = plib::tmp::Builder::new()
        .prefix("c17_pathquote_")
        .tempdir()
        .expect("tempdir");
    let sub = dir.path().join("q\"d\\e");
    std::fs::create_dir(&sub).expect("mkdir");
    let src = sub.join("f.c");
    std::fs::write(
        &src,
        "#include <string.h>\n\
         int main(void) { return !(strchr(__FILE__, '\"') && strchr(__FILE__, '\\\\')); }\n",
    )
    .expect("write");
    let exe = dir.path().join("a.out");
    for flags in [&[][..], &["-g"]] {
        let mut args: Vec<&str> = flags.to_vec();
        args.extend(["-o", exe.to_str().unwrap(), src.to_str().unwrap()]);
        let run = crate::common::run_c17(&args);
        assert!(run.success, "{flags:?}: {}", run.stderr);
        let status = std::process::Command::new(&exe).status().expect("run");
        assert_eq!(status.code(), Some(0), "{flags:?}");
    }
}
