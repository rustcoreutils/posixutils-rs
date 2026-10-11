//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// The line markers `-E` writes, against gcc 13's for the same sources (less
// the `<stdc-predef.h>` gcc includes and c17 does not, as `-ffreestanding`
// leaves it out of gcc's).
//

use crate::common::run_c17;

/// A scratch tree holding `files`, each `(path, text)`.
fn tree(files: &[(&str, &str)]) -> (plib::tmp::TempDir, String) {
    let dir = plib::tmp::Builder::new()
        .prefix("c17_pp_markers_")
        .tempdir()
        .unwrap();
    for (path, text) in files {
        let path = dir.path().join(path);
        std::fs::create_dir_all(path.parent().unwrap()).unwrap();
        std::fs::write(path, text).unwrap();
    }
    let root = dir.path().to_str().unwrap().to_string();
    (dir, root)
}

/// The markers follow inclusion, not tokens: a header that yields no text
/// is entered and left all the same, an entry names the header's line 1
/// whatever line its first token is on, and the return names the line after
/// the `#include`. The command line is gcc's `<command-line>`, entered at
/// line 0 by a `-include` file and returned to, the compiler's text is
/// `<built-in>`, and the source is named again after them. A system header
/// carries `3 4`, and the first token after one that is not a system
/// header's is marked again. c17 marked the first token of each stream
/// instead -- `# 3 "h.h" 1` for a header whose text starts on line 3, no
/// marker for a header with no text and no return from one, and none of the
/// pseudo-files.
#[test]
fn line_markers_follow_inclusion() {
    let (_dir, d) = tree(&[
        (
            "m.c",
            "#include \"e.h\"\n#include \"h.h\"\nint a;\n#include \"h.h\"\nint b;\n\
             #include <s.h>\nint c;\n",
        ),
        ("e.h", "#define E 1\n"),
        ("h.h", "\n#define H 1\nint h = H;\n"),
        ("sys/s.h", "int s;\n"),
        ("pre.h", "int p;\n"),
    ]);
    let r = run_c17(&[
        "-E",
        "-isystem",
        &format!("{d}/sys"),
        "-include",
        &format!("{d}/pre.h"),
        &format!("{d}/m.c"),
    ]);
    assert!(r.success, "{}", r.stderr);
    let expected = format!(
        "# 0 \"{d}/m.c\"\n# 0 \"<built-in>\"\n# 0 \"<command-line>\"\n\
         # 1 \"{d}/pre.h\" 1\nint p;\n# 0 \"<command-line>\" 2\n\
         # 1 \"{d}/m.c\"\n\
         # 1 \"{d}/e.h\" 1\n# 2 \"{d}/m.c\" 2\n\
         # 1 \"{d}/h.h\" 1\n\n\nint h = 1;\n# 3 \"{d}/m.c\" 2\nint a;\n\
         # 1 \"{d}/h.h\" 1\n\n\nint h = 1;\n# 5 \"{d}/m.c\" 2\nint b;\n\
         # 1 \"{d}/sys/s.h\" 1 3 4\n\n# 1 \"{d}/sys/s.h\" 3 4\nint s;\n\
         # 7 \"{d}/m.c\" 2\n\n# 7 \"{d}/m.c\"\nint c;\n"
    );
    assert_eq!(r.stdout, expected);
}

/// A bundled header is a system header named `<built-in>`, entered and
/// left like any other: c17 wrote `# 52 "<built-in>" 1` for `<stddef.h>`,
/// the line of its first token and no system flag, and no return to the
/// includer until a token of it came along.
#[test]
fn line_markers_bundled_header_is_a_system_header() {
    let (_dir, d) = tree(&[("b.c", "#include <stddef.h>\n#include <stdarg.h>\nint x;\n")]);
    let r = run_c17(&["-E", &format!("{d}/b.c")]);
    assert!(r.success, "{}", r.stderr);
    let markers: Vec<&str> = r.stdout.lines().filter(|l| l.starts_with("# ")).collect();
    let main = format!("\"{d}/b.c\"");
    assert_eq!(
        markers[..4],
        [
            format!("# 0 {main}"),
            "# 0 \"<built-in>\"".to_string(),
            "# 0 \"<command-line>\"".to_string(),
            format!("# 1 {main}"),
        ]
    );
    assert_eq!(markers[4], "# 1 \"<built-in>\" 1 3 4");
    let returns: Vec<&&str> = markers.iter().filter(|m| m.ends_with(" 2")).collect();
    assert_eq!(
        returns,
        [
            &format!("# 2 {main} 2").as_str(),
            &format!("# 3 {main} 2").as_str()
        ]
    );
    for m in markers.iter().filter(|m| m.contains("<built-in>")).skip(1) {
        assert!(m.ends_with(" 3 4"), "{m}");
    }
    assert!(
        r.stdout.ends_with(&format!("# 3 {main}\nint x;\n")),
        "{}",
        r.stdout
    );
}

/// `-dD` writes the compiler's definitions under `<built-in>` and the
/// command line's `-D` and `-U` under `<command-line>`, each line after the
/// first a marker of its own as gcc writes them, all at line 0. c17 wrote
/// one `# 1 "<built-in>"` above both, the command line's merged into the
/// compiler's, and no `#undef` for a `-U`.
#[test]
fn line_markers_dash_dd_pseudo_files() {
    let (_dir, d) = tree(&[("d.c", "int x;\n")]);
    let r = run_c17(&[
        "-E",
        "-dD",
        "-DONE=1",
        "-DTWO",
        "-UTHREE",
        &format!("{d}/d.c"),
    ]);
    assert!(r.success, "{}", r.stderr);
    let lines: Vec<&str> = r.stdout.lines().collect();
    assert_eq!(
        lines[..2],
        [format!("# 0 \"{d}/d.c\""), "# 0 \"<built-in>\"".to_string()]
    );
    let cmdline = lines
        .iter()
        .position(|l| *l == "# 0 \"<command-line>\"")
        .expect("no <command-line>");
    // The compiler's: a definition, then marker and definition in turn.
    let builtin = &lines[2..cmdline];
    assert!(builtin.len() > 3, "{builtin:?}");
    for (i, line) in builtin.iter().enumerate() {
        if i % 2 == 0 {
            assert!(line.starts_with("#define "), "{line}");
        } else {
            assert_eq!(*line, "# 0 \"<built-in>\"");
        }
    }
    assert!(!builtin.iter().any(|l| l.contains("ONE")));
    assert_eq!(
        lines[cmdline..],
        [
            "# 0 \"<command-line>\"".to_string(),
            "#define ONE 1".to_string(),
            "# 0 \"<command-line>\"".to_string(),
            "#define TWO 1".to_string(),
            "# 0 \"<command-line>\"".to_string(),
            "#undef THREE".to_string(),
            format!("# 1 \"{d}/d.c\""),
            "int x;".to_string(),
        ]
    );
}

/// A `-include` file is searched for from the working directory, not the
/// source's, and named from it, `./pre.h`, as gcc's is; the dependency rule
/// names it without the `./`, as gcc's does. c17 looked beside the source,
/// so a `-include` of `sub/pre.h` for `sub/s.c` was not found.
#[test]
fn line_markers_dash_include_is_found_from_the_working_directory() {
    let (dir, _) = tree(&[
        ("pre.h", "int top;\n"),
        ("sub/pre.h", "int sub;\n"),
        ("sub/s.c", "#include \"./pre.h\"\nint x;\n"),
    ]);
    let run = |args: &[&str]| {
        let out = std::process::Command::new(env!("CARGO_BIN_EXE_c17"))
            .current_dir(dir.path())
            .args(args)
            .output()
            .unwrap();
        assert!(
            out.status.success(),
            "{args:?}: {}",
            String::from_utf8_lossy(&out.stderr)
        );
        String::from_utf8(out.stdout).unwrap()
    };
    let text = run(&[
        "-E",
        "-include",
        "pre.h",
        "-include",
        "sub/pre.h",
        "sub/s.c",
    ]);
    assert_eq!(
        text,
        "# 0 \"sub/s.c\"\n# 0 \"<built-in>\"\n# 0 \"<command-line>\"\n\
         # 1 \"./pre.h\" 1\nint top;\n# 0 \"<command-line>\" 2\n\
         # 1 \"./sub/pre.h\" 1\nint sub;\n# 0 \"<command-line>\" 2\n\
         # 1 \"sub/s.c\"\n# 1 \"sub/./pre.h\" 1\nint sub;\n# 2 \"sub/s.c\" 2\nint x;\n"
    );
    let rule = run(&["-M", "-include", "pre.h", "./sub/s.c"]);
    assert_eq!(rule, "s.o: sub/s.c pre.h sub/./pre.h\n");
}
