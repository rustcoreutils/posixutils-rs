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
