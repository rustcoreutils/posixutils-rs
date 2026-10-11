//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// An assembler operand read as gcc reads it: with `-include`, from standard
// input, and with a dependency rule that names only real files.
//

use super::{s, WorkDir};
use crate::common::C17Run;
use plib::testing::run_test_base;

/// Run c17 with `args`, feeding it `stdin`, from inside `dir`.
fn run_c17_stdin(args: &[&str], stdin: &str) -> C17Run {
    let owned: Vec<String> = args.iter().map(|a| a.to_string()).collect();
    let out = run_test_base("c17", &owned, stdin.as_bytes());
    C17Run {
        stdout: String::from_utf8_lossy(&out.stdout).into_owned(),
        stderr: String::from_utf8_lossy(&out.stderr).into_owned(),
        success: out.status.success(),
    }
}

/// `-include <file>` is processed ahead of a `.S`, as gcc does. glibc
/// compiles every `.S` with `-include libc-symbols.h`, and Kbuild every one
/// with `-include kconfig.h`; c17 ignored the option for assembler input.
#[test]
fn asm_input_include_applies_to_dot_s_capital() {
    let w = WorkDir::new("asm_include");
    let inc = w.write("inc.h", "#define INSN nop\n");
    let src = w.write(
        "inc.S",
        "#ifndef INSN\n#error not included\n#endif\n\tINSN\n",
    );
    let r = crate::common::run_c17(&["-include", &s(&inc), "-E", "-P", &s(&src)]);
    assert!(r.success, "{}", r.stderr);
    assert_eq!(r.stdout.trim(), "nop");

    let obj = w.join("inc.o");
    let r = crate::common::run_c17(&["-include", &s(&inc), "-c", &s(&src), "-o", &s(&obj)]);
    assert!(r.success, "{}", r.stderr);
    assert!(obj.exists());
}

/// `-x assembler-with-cpp -` reads standard input: glibc's `compile-syscall`
/// builds every syscall wrapper so, and Kbuild's `as-instr` probes the
/// assembler so. c17 looked for a file named `-`.
#[test]
fn asm_input_stdin_assembler_with_cpp() {
    let w = WorkDir::new("asm_stdin");
    let obj = w.join("st.o");
    let src = "#define N nop\n\tN\n";
    let r = run_c17_stdin(
        &["-c", "-x", "assembler-with-cpp", "-", "-o", &s(&obj)],
        src,
    );
    assert!(r.success, "{}", r.stderr);
    assert!(obj.exists());

    let r = run_c17_stdin(&["-E", "-P", "-x", "assembler-with-cpp", "-"], src);
    assert!(r.success, "{}", r.stderr);
    assert_eq!(r.stdout.trim(), "nop");
}

/// `-x assembler -` is assembled from standard input, and `-E` copies it.
#[test]
fn asm_input_stdin_assembler() {
    let w = WorkDir::new("asm_stdin_plain");
    let obj = w.join("st.o");
    let r = run_c17_stdin(&["-c", "-x", "assembler", "-", "-o", &s(&obj)], "\tnop\n");
    assert!(r.success, "{}", r.stderr);
    assert!(obj.exists());

    let r = run_c17_stdin(&["-E", "-x", "assembler", "-"], "\tnop\n");
    assert!(r.success, "{}", r.stderr);
    assert!(r.stdout.contains("nop"), "{:?}", r.stdout);
}

/// A dependency rule for standard input names the headers and nothing for
/// the input itself: gcc writes no `<stdin>` (or `-`) prerequisite, and
/// glibc's next `make` failed with "No rule to make target '<stdin>'".
#[test]
fn asm_input_stdin_dependency_rule_names_only_headers() {
    let w = WorkDir::new("deps_of_input");
    let inc = w.write("inc.h", "#define N nop\n");
    let dep = w.join("x.d");
    let obj = w.join("x.o");
    let include = format!("#include \"{}\"\n", s(&inc));
    for (lang, body) in [("c", "int x;\n"), ("assembler-with-cpp", "\tN\n")] {
        let _ = std::fs::remove_file(&dep);
        let r = run_c17_stdin(
            &[
                "-c",
                "-x",
                lang,
                "-",
                "-MD",
                "-MP",
                "-MF",
                &s(&dep),
                "-MT",
                "sd.o",
                "-o",
                &s(&obj),
            ],
            &format!("{include}{body}"),
        );
        assert!(r.success, "{lang}: {}", r.stderr);
        let rule = std::fs::read_to_string(&dep).unwrap_or_else(|e| panic!("{lang}: {e}"));
        // Long temporary paths wrap the rule onto continuation lines.
        let rule = rule.replace(" \\\n", "");
        let first = rule.lines().next().unwrap_or_default();
        assert_eq!(first, format!("sd.o: {}", s(&inc)), "{lang}: {rule:?}");
        assert!(!rule.contains("stdin"), "{lang}: {rule:?}");
        assert!(
            rule.contains(&format!("{}:", s(&inc))),
            "{lang}: -MP {rule:?}"
        );
    }

    // With no -MT the target is the -o object.
    let r = run_c17_stdin(
        &["-c", "-x", "c", "-", "-MD", "-MF", &s(&dep), "-o", &s(&obj)],
        &format!("{include}int x;\n"),
    );
    assert!(r.success, "{}", r.stderr);
    let rule = std::fs::read_to_string(&dep).unwrap();
    // Long temporary paths wrap the rule onto continuation lines.
    let rule = rule.replace(" \\\n", "");
    assert_eq!(rule.trim_end(), format!("{}: {}", s(&obj), s(&inc)));
}

/// `-M` on standard input names the target `-`, as gcc does.
#[test]
fn asm_input_stdin_dash_m_target() {
    let r = run_c17_stdin(&["-M", "-x", "c", "-"], "int x;\n");
    assert!(r.success, "{}", r.stderr);
    assert_eq!(r.stdout.trim_end(), "-:");
}

/// A `##` that forms no single token is no error in assembler-with-cpp, as
/// in gcc's `-lang-asm`: the two tokens stand side by side. The kernel's
/// linkage.h pastes onto a `.macro` parameter (`.L__sym_size_##name`),
/// vmlinux.lds.h builds `initcall##level##.init`, and the crypto code
/// writes `,##reg`; c17 stopped with "preprocessing failed".
#[cfg(target_os = "linux")]
#[test]
fn asm_input_invalid_paste_is_two_tokens() {
    let w = WorkDir::new("asm_paste");
    let src = w.write(
        "paste.S",
        "#define SIZE(name) .set .L__sym_size_##name, 4\n\
         #define SEC(level) .section .initcall##level##.init,\"a\"\n\
         #define LIST(a, b) .long a,##b\n\
         .macro m name\nSIZE(\\name)\n.long .L__sym_size_\\name\n.endm\n\
         \tSEC(1)\n\tLIST(1, 2)\n\tm foo\n",
    );
    let obj = w.join("paste.o");
    let r = crate::common::run_c17(&["-c", &s(&src), "-o", &s(&obj)]);
    assert!(r.success, "{}", r.stderr);
    assert!(r.stderr.is_empty(), "{}", r.stderr);
    let dump = std::process::Command::new("objdump")
        .args(["-s", "-j", ".initcall1.init", &s(&obj)])
        .output()
        .expect("objdump");
    let dump = String::from_utf8_lossy(&dump.stdout);
    assert!(dump.contains(" 0000 01000000 02000000 04000000"), "{dump}");
}

/// The source the next three tests preprocess: definitions among the text,
/// and blank lines.
const DEFINING_ASM: &str =
    "#define FOO 1\n#define BAR(x) x+1\n\n\nmov FOO, %eax\n#define BAZ 2\nmov BAZ, %eax\n";

/// `-dM -E` of a `.S` lists the macros in force at its end, assembler-with-
/// cpp's own among them, as gcc's does; c17 wrote nothing at all.
#[test]
fn asm_input_dash_dm_lists_the_macros() {
    let w = WorkDir::new("asm_dm");
    let src = w.write("x.S", DEFINING_ASM);
    let r = crate::common::run_c17(&["-dM", "-E", &s(&src)]);
    assert!(r.success, "{}", r.stderr);
    let lines: Vec<&str> = r.stdout.lines().collect();
    for want in [
        "#define FOO 1",
        "#define BAR(x) x+1",
        "#define BAZ 2",
        "#define __ASSEMBLER__ 1",
    ] {
        assert!(lines.contains(&want), "{want}: {}", r.stdout);
    }
    assert!(
        !lines.iter().any(|l| l.starts_with("#define __STDC__ ")),
        "{}",
        r.stdout
    );
    assert!(!r.stdout.contains("mov"), "{}", r.stdout);
}

/// `-E -P` of a `.S` closes up the lines its directives stood on, as it does
/// for C and as gcc does; c17 left a blank line for each.
#[test]
fn asm_input_dash_p_closes_up_directive_lines() {
    let w = WorkDir::new("asm_p");
    let src = w.write("x.S", DEFINING_ASM);
    let r = crate::common::run_c17(&["-E", "-P", &s(&src)]);
    assert!(r.success, "{}", r.stderr);
    assert_eq!(r.stdout, "mov 1, %eax\nmov 2, %eax\n");
}

/// `-E` of a `.S` carries gcc's line markers, as C's does: c17 wrote none,
/// so the text after a `#include` claimed the includer's lines. And `-dD`
/// keeps the definitions where they stood. A line's indentation is gcc's,
/// a tab written as one space.
#[test]
fn asm_input_dash_e_writes_line_markers() {
    let w = WorkDir::new("asm_markers");
    let inc = w.write("regs.h", "#define RET ret\n\tnop\n");
    let src = w.write(
        "x.S",
        &format!("{DEFINING_ASM}#include \"regs.h\"\n\tRET\n"),
    );
    let (src, inc) = (s(&src), s(&inc));
    let r = crate::common::run_c17(&["-E", &src]);
    assert!(r.success, "{}", r.stderr);
    assert_eq!(
        r.stdout,
        format!(
            "# 0 \"{src}\"\n# 0 \"<built-in>\"\n# 0 \"<command-line>\"\n# 1 \"{src}\"\n\
             \n\n\n\nmov 1, %eax\n\nmov 2, %eax\n\
             # 1 \"{inc}\" 1\n\n nop\n# 9 \"{src}\" 2\n ret\n"
        )
    );
    let r = crate::common::run_c17(&["-E", "-dD", "-P", &src]);
    assert!(r.success, "{}", r.stderr);
    assert!(
        r.stdout.ends_with(
            "#define FOO 1\n#define BAR(x) x+1\nmov 1, %eax\n#define BAZ 2\n\
             mov 2, %eax\n#define RET ret\n nop\n ret\n"
        ),
        "{}",
        r.stdout
    );
}
