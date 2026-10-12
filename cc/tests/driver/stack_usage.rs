//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// `-fstack-usage`: one `.su` file per translation unit, named and laid out as
// gcc 13 does, so that a c17 and a gcc build can be diffed function by
// function.
//
// The convention was measured against gcc 13.3 (x86_64-linux-gnu and
// aarch64-linux-gnu) on probes of each kind of frame:
//
// - x86-64 counts every byte `%rsp` moves below the caller's: the return
//   address, the saved `%rbp`, each pushed callee-saved register, the `subq`
//   and, under realignment, the most the `andq` can take. `int f(int a)
//   {return a+1;}` is 16 at -O0 (return address and `%rbp`) and 8 at -O2.
// - AArch64 counts the frame the prologue allocates, frame record and saved
//   registers included; the same leaf is 16 at -O0 and 0 at -O2.
// - `static`: the frame is all there is. `dynamic,bounded`: the stack pointer
//   also moves by a known amount after the prologue -- an outgoing argument
//   area -- and the largest such move is added. gcc -O0 on x86-64 pushes the
//   stacked arguments of `g(a,1,2,3,4,5,6,7,8)` and says `64 dynamic,bounded`
//   (16 + a 16-byte frame + 32 pushed). `dynamic`: `alloca` or a VLA.
//
// c17's frames are its own, so the numbers below are c17's layout counted by
// that convention; the leaf and the stacked-argument function come out as
// gcc's do.
//

use super::WorkDir;
use plib::testing::get_binary_path;
use std::path::Path;
use std::process::{Command, Output, Stdio};

/// Run c17 in `dir` with `args`, `stdin` on its standard input.
fn c17_in(dir: &Path, args: &[&str], stdin: &str) -> Output {
    use std::io::Write;
    let mut child = Command::new(get_binary_path("c17"))
        .args(args)
        .current_dir(dir)
        .env("LC_ALL", "C")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("spawn c17");
    child
        .stdin
        .take()
        .expect("stdin")
        .write_all(stdin.as_bytes())
        .expect("write stdin");
    child.wait_with_output().expect("wait for c17")
}

#[track_caller]
fn c17_ok(dir: &Path, args: &[&str]) {
    let out = c17_in(dir, args, "");
    assert!(
        out.status.success(),
        "c17 {args:?}: {}",
        String::from_utf8_lossy(&out.stderr)
    );
}

/// Every `.su` file under `dir`, relative to it, sorted.
fn su_files(dir: &Path) -> Vec<String> {
    fn walk(root: &Path, dir: &Path, found: &mut Vec<String>) {
        for entry in std::fs::read_dir(dir).expect("read_dir").flatten() {
            let path = entry.path();
            if path.is_dir() {
                walk(root, &path, found);
            } else if path.extension().is_some_and(|e| e == "su") {
                let rel = path.strip_prefix(root).expect("under root");
                found.push(rel.to_string_lossy().into_owned());
            }
        }
    }
    let mut found = Vec::new();
    walk(dir, dir, &mut found);
    found.sort();
    found
}

const HELPER: &str = "int helper(void) { return 1; }\n";
const MAIN: &str = "int helper(void);\nint main(void) { return helper(); }\n";

/// A work dir holding `x.c` (a helper), `b.c` (a `main` calling it), the
/// same helper as `sub/x.c`, and empty `out/` and `out.d/` directories.
fn naming_dir(tag: &str) -> WorkDir {
    let w = WorkDir::new(&format!("su_{tag}"));
    w.write("x.c", HELPER);
    w.write("b.c", MAIN);
    for d in ["sub", "out", "out.d"] {
        std::fs::create_dir(w.join(d)).expect("mkdir");
    }
    w.write("sub/x.c", HELPER);
    w
}

/// The `.su` files gcc 13 writes for each command line: beside the object or
/// assembly `-o` names, its suffix replaced; for the source's basename in
/// the current directory when nothing names one; and when linking, as
/// `<output>-<source>.su`, the output `a` when it is `a.out` or not named,
/// and a `.exe` suffix dropped. `-o -` and `-o /dev/null` name nothing.
#[test]
fn stack_usage_file_names() {
    let cases: &[(&[&str], &[&str])] = &[
        (&["-c", "x.c"], &["x.su"]),
        (&["-c", "x.c", "-o", "out/y.o"], &["out/y.su"]),
        (&["-c", "x.c", "-o", "out/y"], &["out/y.su"]),
        (&["-c", "x.c", "-o", "out/y.o.o"], &["out/y.o.su"]),
        (&["-c", "x.c", "-o", "out.d/y"], &["out.d/y.su"]),
        (&["-c", "sub/x.c"], &["x.su"]),
        (&["-c", "sub/x.c", "-o", "out/y.o"], &["out/y.su"]),
        (&["-S", "x.c"], &["x.su"]),
        (&["-S", "x.c", "-o", "out/z.s"], &["out/z.su"]),
        (&["-S", "x.c", "-o", "-"], &["x.su"]),
        (&["-c", "x.c", "-o", "/dev/null"], &["x.su"]),
        (&["-S", "x.c", "-o", "/dev/null"], &["x.su"]),
        (&["-c", "x.c", "b.c"], &["b.su", "x.su"]),
        (
            &["x.c", "b.c", "-o", "out/prog"],
            &["out/prog-b.su", "out/prog-x.su"],
        ),
        (&["x.c", "b.c"], &["a-b.su", "a-x.su"]),
        (&["sub/x.c", "b.c"], &["a-b.su", "a-x.su"]),
        (
            &["x.c", "b.c", "-o", "out/prog.exe"],
            &["out/prog-b.su", "out/prog-x.su"],
        ),
        (
            &["x.c", "b.c", "-o", "out/prog.bin"],
            &["out/prog.bin-b.su", "out/prog.bin-x.su"],
        ),
        (
            &["x.c", "b.c", "-o", "out/a.out"],
            &["out/a-b.su", "out/a-x.su"],
        ),
        (
            &["x.c", "b.c", "-o", "out/p.out"],
            &["out/p.out-b.su", "out/p.out-x.su"],
        ),
        (&["-fno-stack-usage", "-c", "x.c"], &[]),
        (&["-E", "x.c", "-o", "out/x.i"], &[]),
        (&["-fsyntax-only", "x.c"], &[]),
    ];
    for (n, (args, expected)) in cases.iter().enumerate() {
        let w = naming_dir(&format!("name{n}"));
        let mut argv = vec!["-fstack-usage"];
        argv.extend_from_slice(args);
        c17_ok(w.path(), &argv);
        assert_eq!(su_files(w.path()), *expected, "c17 {argv:?}");
    }
    // The last of `-fstack-usage` and `-fno-stack-usage` wins.
    let w = naming_dir("last");
    c17_ok(
        w.path(),
        &["-fno-stack-usage", "-fstack-usage", "-c", "x.c"],
    );
    assert_eq!(su_files(w.path()), ["x.su"]);
}

/// A record names its function as the source operand was spelled on the
/// command line, `<stdin>` for standard input, whose file is `-.su`.
#[test]
fn stack_usage_names_the_source_as_given() {
    let w = naming_dir("given");
    c17_ok(w.path(), &["-fstack-usage", "-c", "sub/x.c"]);
    let text = std::fs::read_to_string(w.join("x.su")).expect("x.su");
    assert!(text.starts_with("sub/x.c:1:5:helper\t"), "{text}");

    let w = naming_dir("stdin");
    let out = c17_in(w.path(), &["-fstack-usage", "-c", "-x", "c", "-"], HELPER);
    assert!(
        out.status.success(),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert_eq!(su_files(w.path()), ["-.su"]);
    let text = std::fs::read_to_string(w.join("-.su")).expect("-.su");
    assert!(text.starts_with("<stdin>:1:5:helper\t"), "{text}");
}

/// One function of each kind: a leaf; a static function with a local
/// array; one passing three arguments on the stack; one with a VLA; one
/// with `alloca`; one keeping six values across a call; and `main`.
const KINDS: &str = "\
int leaf(int a) { return a + 1; }
static int locals(int n) { volatile char buf[100]; buf[n] = 1; return buf[0]; }
extern int g(int, int, int, int, int, int, int, int, int);
int stacked(int a) { return g(a, 1, 2, 3, 4, 5, 6, 7, 8) + 1; }
int vla(int n) { char p[n]; p[0] = 1; return p[n - 1]; }
extern void use(void *);
int al(int n) { char *p = __builtin_alloca(n); use(p); return 0; }
int keeps(int a) { int x = g(a, a, a, a, a, a, 0, 0, 0); return x + a; }
int main(void) { return locals(3); }
";

/// The `.su` file c17 writes for `src`, compiled as `t.c` with `-S` and
/// `flags`.
fn su_text(tag: &str, src: &str, flags: &[&str]) -> String {
    let w = WorkDir::new(&format!("su_{tag}"));
    w.write("t.c", src);
    let mut argv = vec!["-fstack-usage", "-S", "t.c"];
    argv.extend_from_slice(flags);
    c17_ok(w.path(), &argv);
    std::fs::read_to_string(w.join("t.su")).expect("t.su written")
}

const X86_64: &str = "--target=x86_64-unknown-linux-gnu";
const AARCH64: &str = "--target=aarch64-unknown-linux-gnu";

/// x86-64 at -O0, each figure read off c17's prologue: 16 for the return
/// address and `%rbp`, 8 per pushed register, the `subq`, and for
/// `dynamic,bounded` the 32-byte outgoing argument area.
#[test]
fn stack_usage_x86_64_frames() {
    assert_eq!(
        su_text("x86_o0", KINDS, &[X86_64, "-O0"]),
        "t.c:1:5:leaf\t16\tstatic\n\
         t.c:2:12:locals\t128\tstatic\n\
         t.c:4:5:stacked\t64\tdynamic,bounded\n\
         t.c:5:5:vla\t32\tdynamic\n\
         t.c:7:5:al\t32\tdynamic\n\
         t.c:8:5:keeps\t112\tdynamic,bounded\n\
         t.c:9:5:main\t16\tstatic\n"
    );
    // Optimized, `locals` is inlined into `main` and emitted nowhere, so
    // it has no record, as in gcc.
    assert_eq!(
        su_text("x86_o2", KINDS, &[X86_64, "-O2"]),
        "t.c:1:5:leaf\t16\tstatic\n\
         t.c:4:5:stacked\t64\tdynamic,bounded\n\
         t.c:5:5:vla\t32\tdynamic\n\
         t.c:7:5:al\t32\tdynamic\n\
         t.c:8:5:keeps\t64\tdynamic,bounded\n\
         t.c:9:5:main\t128\tstatic\n"
    );
}

/// AArch64 at -O0: the frame the prologue's `stp ..., [sp, #-N]!` takes,
/// and the 16-byte area each call with a stacked argument carves below it.
#[test]
fn stack_usage_aarch64_frames() {
    assert_eq!(
        su_text("a64_o0", KINDS, &[AARCH64, "-O0"]),
        "t.c:1:5:leaf\t16\tstatic\n\
         t.c:2:12:locals\t128\tstatic\n\
         t.c:4:5:stacked\t48\tdynamic,bounded\n\
         t.c:5:5:vla\t32\tdynamic\n\
         t.c:7:5:al\t48\tdynamic\n\
         t.c:8:5:keeps\t96\tdynamic,bounded\n\
         t.c:9:5:main\t16\tstatic\n"
    );
}

/// An over-aligned frame on x86-64 counts the most its `andq` can take:
/// the alignment less what `%rsp` is known to be aligned to before it --
/// 8 here, after one push. gcc counts the same worst case.
#[test]
fn stack_usage_x86_64_realigned_frame() {
    let src = "\
extern void use(void *);
int realign(void) { _Alignas(64) char b[64]; use(b); return 0; }
";
    // 16 + 1 push (the aligned base) + a 128-byte `subq` + 56.
    assert_eq!(
        su_text("realign", src, &[X86_64, "-O0"]),
        "t.c:2:5:realign\t208\tstatic\n"
    );
}

/// The position is the declarator's identifier, its column counted in
/// bytes (a tab is one); a `#line` renames the file and a function defined
/// in a header is named by the header.
#[test]
fn stack_usage_positions() {
    let w = WorkDir::new("su_pos");
    w.write("h.h", "static int in_header(void) { return 0; }\n");
    w.write(
        "t.c",
        "#include \"h.h\"\n\
         \tint\ttabbed(void) { return in_header(); }\n\
         #line 50 \"fake.c\"\n\
         int\n  renamed  (void) { return 0; }\n",
    );
    c17_ok(w.path(), &["-fstack-usage", "-S", "t.c", X86_64, "-O0"]);
    let text = std::fs::read_to_string(w.join("t.su")).expect("t.su");
    let heads: Vec<&str> = text.lines().filter_map(|l| l.split('\t').next()).collect();
    assert_eq!(
        heads,
        [
            "h.h:1:12:in_header",
            "t.c:2:6:tabbed",
            "fake.c:51:3:renamed"
        ],
        "{text}"
    );
}

/// gcc's refusals of a bad `-Wstack-usage=` value.
#[test]
fn stack_usage_warning_option_values() {
    let w = naming_dir("wvalues");
    for (opt, text) in [
        (
            "-Wstack-usage=abc",
            "error: argument to '-Wstack-usage=' should be a non-negative integer \
             optionally followed by a size unit",
        ),
        (
            "-Wstack-usage=-1",
            "error: argument to '-Wstack-usage=' should be a non-negative integer \
             optionally followed by a size unit",
        ),
        (
            "-Wstack-usage=",
            "error: missing argument to '-Wstack-usage='",
        ),
        (
            "-Wno-stack-usage=5",
            "error: unrecognized command-line option '-Wno-stack-usage=5'",
        ),
    ] {
        let out = c17_in(w.path(), &["-c", "x.c", opt], "");
        let stderr = String::from_utf8_lossy(&out.stderr);
        assert!(!out.status.success(), "{opt}");
        assert!(stderr.contains(text), "{opt}: {stderr}");
    }
    for opt in ["-Wstack-usage=1KiB", "-Wno-stack-usage"] {
        c17_ok(w.path(), &["-c", "x.c", opt]);
    }
}
