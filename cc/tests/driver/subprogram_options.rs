//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// The gcc options that speak to the programs behind the driver: `-Wa,` to
// the assembler, `-Wp,` to the preprocessor, and `-B` to where those
// programs, and the link's, are found.
//

use super::{run_exe, s, WorkDir};
use crate::common::run_c17;

/// Whether the object at `path` has a section named `name`.
#[cfg(target_os = "linux")]
fn has_section(path: &std::path::Path, name: &str) -> bool {
    let out = std::process::Command::new("readelf")
        .args(["-S", "-W"])
        .arg(path)
        .output()
        .expect("readelf");
    String::from_utf8_lossy(&out.stdout).contains(name)
}

/// `-Wa,--noexecstack` reaches `as` for a `.s` and a `.S` operand, so the
/// object says its stack is not executable. glibc passes it with every
/// `.S`; c17 dropped it, and ld warned that `start.o` implied an
/// executable stack, which fails glibc's `check-execstack`.
#[cfg(target_os = "linux")]
#[test]
fn subprogram_wa_noexecstack_reaches_the_assembler() {
    let w = WorkDir::new("wa_noexec");
    for name in ["ne.s", "ne.S"] {
        let src = w.write(name, "\t.text\n\tnop\n");
        let obj = w.join("ne.o");
        let r = run_c17(&["-Wa,--noexecstack", "-c", &s(&src), "-o", &s(&obj)]);
        assert!(r.success, "{name}: {}", r.stderr);
        assert!(
            has_section(&obj, ".note.GNU-stack"),
            "{name}: -Wa,--noexecstack was not given to as"
        );
    }
}

/// `-Wa,--defsym,FOO=1` is split at its commas, as gcc splits it, and both
/// words reach `as` for every kind of input, `.c` included.
#[cfg(target_os = "linux")]
#[test]
fn subprogram_wa_defsym_is_split_and_reaches_every_input() {
    let w = WorkDir::new("wa_defsym");
    let s_src = w.write("ds.s", ".if FOO\n\tnop\n.endif\n");
    let cap_src = w.write("ds.S", "#define X nop\n.if FOO\n\tX\n.endif\n");
    let c_src = w.write("ds.c", "__asm__(\".if FOO\\n.endif\");\nint x;\n");
    for src in [s_src, cap_src, c_src] {
        let obj = w.join("ds.o");
        let r = run_c17(&["-Wa,--defsym,FOO=1", "-c", &s(&src), "-o", &s(&obj)]);
        assert!(r.success, "{}: {}", src.display(), r.stderr);
        assert!(obj.exists());
    }
}

/// An option `as` does not know fails the run: the options are the
/// assembler's, not something c17 may quietly drop.
#[cfg(target_os = "linux")]
#[test]
fn subprogram_wa_unknown_option_fails_the_assembler() {
    let w = WorkDir::new("wa_bogus");
    let src = w.write("t.c", "int x;\n");
    let obj = w.join("t.o");
    let r = run_c17(&["-Wa,--c17-no-such-option", "-c", &s(&src), "-o", &s(&obj)]);
    assert!(!r.success, "as accepted an option it does not know");
    assert!(r.stderr.contains("c17-no-such-option"), "{}", r.stderr);
}

/// `-Wa,--version` prints the assembler's version, as with gcc: the Linux
/// kernel's `scripts/as-version.sh` runs exactly this line and Kconfig
/// stops when it prints nothing.
#[cfg(target_os = "linux")]
#[test]
fn subprogram_wa_version_prints_the_assembler_version() {
    let r = run_c17(&[
        "-Wa,--version",
        "-c",
        "-x",
        "assembler-with-cpp",
        "/dev/null",
        "-o",
        "/dev/null",
    ]);
    assert!(r.success, "{}", r.stderr);
    assert!(r.stdout.contains("GNU assembler"), "{:?}", r.stdout);
}

/// `-Xassembler <option>` is `-Wa,` without the comma splitting.
#[cfg(target_os = "linux")]
#[test]
fn subprogram_xassembler_reaches_the_assembler() {
    let w = WorkDir::new("xassembler");
    let src = w.write("ne.s", "\t.text\n\tnop\n");
    let obj = w.join("ne.o");
    let r = run_c17(&[
        "-Xassembler",
        "--noexecstack",
        "-c",
        &s(&src),
        "-o",
        &s(&obj),
    ]);
    assert!(r.success, "{}", r.stderr);
    assert!(has_section(&obj, ".note.GNU-stack"));
}

/// `-Wp,-MD,<file>` and `-Wp,-MMD,<file>` are `-MD`/`-MMD` with `-MF
/// <file>`: Kbuild compiles every object with `-Wp,-MMD,./.x.o.d` and
/// `fixdep` stops when the file is missing.
#[test]
fn subprogram_wp_md_writes_the_dependency_file() {
    let w = WorkDir::new("wp_md");
    w.write("h.h", "#define H 1\n");
    let src = w.write("a.c", "#include \"h.h\"\nint x = H;\n");
    let obj = w.join("a.o");
    for (opt, dep) in [("-MMD", ".a.o.d"), ("-MD", ".b.o.d")] {
        let dep = w.join(dep);
        let wp = format!("-Wp,{opt},{}", s(&dep));
        let r = run_c17(&[&wp, "-c", &s(&src), "-o", &s(&obj)]);
        assert!(r.success, "{wp}: {}", r.stderr);
        let rule = std::fs::read_to_string(&dep).unwrap_or_else(|e| panic!("{wp}: {e}"));
        assert!(rule.starts_with(&format!("{}:", s(&obj))), "{rule:?}");
        assert!(rule.contains("h.h"), "{rule:?}");
        assert!(obj.exists(), "{wp} must still compile");
    }
}

/// `-Wp,-D...`, `-Wp,-U...` and `-Wp,-I...` are the preprocessor's options
/// as gcc hands them on, attached or in the next comma-separated word.
#[test]
fn subprogram_wp_define_undefine_include() {
    let w = WorkDir::new("wp_d");
    let inc = w.path().join("inc");
    std::fs::create_dir(&inc).unwrap();
    std::fs::write(inc.join("v.h"), "#define V 7\n").unwrap();
    let src = w.write("t.c", "#include <v.h>\nA B C V\n");
    let wi = format!("-Wp,-I,{}", s(&inc));
    let r = run_c17(&[
        "-E",
        "-P",
        "-DC=3",
        "-Wp,-DA=1",
        "-Wp,-D,B=2,-UC",
        &wi,
        &s(&src),
    ]);
    assert!(r.success, "{}", r.stderr);
    assert_eq!(r.stdout.trim(), "1 2 C 7");
}

/// A `-Wp,` option c17 does not implement is refused, never dropped.
#[test]
fn subprogram_wp_unknown_option_is_an_error() {
    let w = WorkDir::new("wp_bogus");
    let src = w.write("t.c", "int x;\n");
    let r = run_c17(&["-Wp,-bogus", "-E", &s(&src)]);
    assert!(!r.success, "-Wp,-bogus was accepted");
    assert!(r.stderr.contains("-bogus"), "{}", r.stderr);
}

/// `-B<dir>` is gcc's prefix for the programs the driver runs: glibc links
/// every shared object with `-B$(csu-objpfx)`. c17 parsed it as POSIX's
/// binding option and refused the directory. A prefix the link does not
/// need still links.
#[test]
fn subprogram_b_prefix_directory_links() {
    let w = WorkDir::new("bprefix");
    let src = w.write("m.c", "int main(void){return 3;}\n");
    let dir = w.path().join("pfx");
    std::fs::create_dir(&dir).unwrap();
    for argv in [
        vec![format!("-B{}/", s(&dir))],
        vec!["-B".to_string(), s(&dir)],
    ] {
        let exe = w.join("m");
        let mut full = argv.clone();
        full.extend([s(&src), "-o".to_string(), s(&exe)]);
        let full: Vec<&str> = full.iter().map(String::as_str).collect();
        let r = run_c17(&full);
        assert!(r.success, "{argv:?}: {}", r.stderr);
        assert_eq!(run_exe(&exe), 3, "{argv:?}");
    }
}

/// The binding modes are still POSIX's, in both spellings.
#[test]
fn subprogram_b_static_and_dynamic_still_bind() {
    let w = WorkDir::new("bbind");
    let src = w.write("m.c", "int main(void){return 0;}\n");
    for argv in [
        &["-Bstatic"][..],
        &["-B", "static"],
        &["-Bdynamic"],
        &["-B", "dynamic"],
    ] {
        let exe = w.join("m");
        let mut full: Vec<String> = argv.iter().map(|a| a.to_string()).collect();
        full.extend([s(&src), "-o".to_string(), s(&exe)]);
        let full: Vec<&str> = full.iter().map(String::as_str).collect();
        let r = run_c17(&full);
        assert!(r.success, "{argv:?}: {}", r.stderr);
        assert_eq!(run_exe(&exe), 0, "{argv:?}");
    }
}

/// A program in a `-B` directory is the one run, as gcc runs it: c17's own
/// `as`, and the link's `ld` through the host driver.
#[cfg(target_os = "linux")]
#[test]
fn subprogram_b_prefix_finds_as_and_ld() {
    use std::os::unix::fs::PermissionsExt;
    let w = WorkDir::new("bprogs");
    let dir = w.path().join("pfx");
    std::fs::create_dir(&dir).unwrap();
    for prog in ["as", "ld"] {
        let wrapper = dir.join(prog);
        let marker = dir.join(format!("{prog}-used"));
        std::fs::write(
            &wrapper,
            format!("#!/bin/sh\ntouch '{}'\nexec {prog} \"$@\"\n", s(&marker)),
        )
        .unwrap();
        std::fs::set_permissions(&wrapper, std::fs::Permissions::from_mode(0o755)).unwrap();
    }
    let src = w.write("m.c", "int main(void){return 0;}\n");
    let exe = w.join("m");
    let r = run_c17(&[&format!("-B{}", s(&dir)), &s(&src), "-o", &s(&exe)]);
    assert!(r.success, "{}", r.stderr);
    assert!(
        dir.join("as-used").exists(),
        "the -B directory's as was not run"
    );
    assert!(dir.join("ld-used").exists(), "-B did not reach the link");
}
