//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// gcc driver flags real build systems pass, each of which used to stop c17
// before it compiled anything: `-pedantic`, `-ansi`, `-x LANG`, the `-g`
// levels and formats, and the `-m` flags that choose or tune for a CPU.
// Also the `__PIC__`/`__PIE__` macros, which describe the code generated.
//

use crate::common::{preprocess_text, run_c17};
use std::path::{Path, PathBuf};

/// A scratch directory holding `name` with `content`.
fn scratch(name: &str, content: &str) -> (plib::tmp::TempDir, PathBuf) {
    let dir = plib::tmp::Builder::new()
        .prefix("c17_gcc_flags_")
        .tempdir()
        .expect("tempdir");
    let path = dir.path().join(name);
    std::fs::write(&path, content).expect("write");
    (dir, path)
}

const MAIN: &str = "int main(void) { return 0; }\n";

/// Compile `src` (written as `name`) to an object with `flags`.
fn compile_with(name: &str, src: &str, flags: &[&str]) -> crate::common::C17Run {
    let (dir, path) = scratch(name, src);
    let obj = dir.path().join("out.o");
    let mut args: Vec<&str> = flags.to_vec();
    args.extend(["-c", "-o", obj.to_str().unwrap(), path.to_str().unwrap()]);
    run_c17(&args)
}

#[test]
fn gcc_flags_pedantic_and_ansi_are_accepted() {
    for flags in [
        &["-pedantic"][..],
        &["-pedantic-errors"],
        &["-pedantic", "-pedantic-errors"],
        &["-Wpedantic"],
    ] {
        let r = compile_with("ped.c", MAIN, flags);
        assert!(r.success, "{flags:?}: {}", r.stderr);
    }
    // `-ansi` is `-std=c90`, reported as any older revision is.
    let r = compile_with("ansi.c", MAIN, &["-ansi"]);
    assert!(r.success, "{}", r.stderr);
    assert!(r.stderr.contains("'-std=c90' ignored"), "{}", r.stderr);
}

#[test]
fn gcc_flags_x_selects_the_language() {
    // A header compiled as C.
    let r = compile_with("hdr.h", MAIN, &["-x", "c"]);
    assert!(r.success, "{}", r.stderr);
    let r = compile_with("hdr.h", MAIN, &["-xc"]);
    assert!(r.success, "{}", r.stderr);

    // Assembly that needs the preprocessor, under a suffix that says neither.
    let r = compile_with(
        "code.asm",
        "#define NAME c17_x_sym\n.globl NAME\nNAME:\n    ret\n",
        &["-x", "assembler-with-cpp"],
    );
    assert!(r.success, "{}", r.stderr);

    // `-x none` goes back to suffixes, so a later `.h` is a header again,
    // which gcc would precompile and c17 refuses.
    let (dir, path) = scratch("later.h", MAIN);
    let c = dir.path().join("first.c");
    std::fs::write(&c, MAIN).unwrap();
    let obj = dir.path().join("o.o");
    let r = run_c17(&[
        "-x",
        "c",
        c.to_str().unwrap(),
        "-x",
        "none",
        path.to_str().unwrap(),
        "-c",
        "-o",
        obj.to_str().unwrap(),
    ]);
    assert!(!r.success, "{}", r.stderr);
    assert!(
        r.stderr.contains("c17 does not write precompiled headers"),
        "{}",
        r.stderr
    );

    // A language c17 does not compile is an error, not a guess.
    let r = compile_with("c.c", MAIN, &["-x", "c++"]);
    assert!(!r.success);
    assert!(
        r.stderr.contains("language not recognized: c++"),
        "{}",
        r.stderr
    );
}

/// The debug levels, on an ELF and a Mach-O target: the section is
/// `.debug_info` in ELF and `__DWARF,__debug_info` in Mach-O, which the
/// test once took for "no debug information" on macOS.
#[test]
fn gcc_flags_debug_levels() {
    let (dir, path) = scratch("dbg.c", "int f(int x) { return x + 1; }\n");
    let asm = dir.path().join("dbg.s");
    for (target, section) in [
        ("--target=x86_64-unknown-linux-gnu", ".section .debug_info"),
        (
            "--target=aarch64-apple-darwin",
            ".section __DWARF,__debug_info",
        ),
    ] {
        for (flags, want_debug) in [
            (&["-g3"][..], true),
            (&["-ggdb"], true),
            (&["-gdwarf-4"], true),
            (&["-g2", "-gsplit-dwarf"], true),
            (&["-g", "-g0"], false),
            (&["-g0", "-g1"], true),
        ] {
            let mut args = vec![target];
            args.extend(flags);
            args.extend(["-S", "-o", asm.to_str().unwrap(), path.to_str().unwrap()]);
            let r = run_c17(&args);
            assert!(r.success, "{target} {flags:?}: {}", r.stderr);
            let text = std::fs::read_to_string(&asm).unwrap();
            assert_eq!(text.contains(section), want_debug, "{target} {flags:?}");
        }
    }
}

#[test]
fn gcc_flags_cpu_selection_is_accepted() {
    let mut flags = vec!["-march=native", "-mtune=generic", "-mcpu=native"];
    if cfg!(target_arch = "x86_64") {
        flags.extend([
            "-m64",
            "-march=x86-64",
            "-march=x86-64-v2",
            "-msse2",
            "-mfpmath=sse",
        ]);
    }
    for flag in flags {
        let r = compile_with("cpu.c", MAIN, &[flag]);
        assert!(r.success, "{flag}: {}", r.stderr);
    }
    // An instruction-set extension or an ABI change is still refused.
    for flag in ["-mavx2", "-mno-red-zone", "-mgeneral-regs-only"] {
        let r = compile_with("cpu.c", MAIN, &[flag]);
        assert!(!r.success, "{flag} accepted");
        assert!(
            r.stderr.contains("unsupported machine flag"),
            "{}",
            r.stderr
        );
    }
}

/// The `-fpic` family -- `-fpic`, `-fPIC`, `-fpie`, `-fPIE` and their four
/// `-fno-` forms -- is one option: the last one given wins, and any `-fno-`
/// form turns position independence off altogether, PIE included. The
/// `__PIC__`/`__pic__` and `__PIE__`/`__pie__` macros say what it chose: 1
/// for the lower-case spellings, 2 for the upper-case ones and for the PIE
/// default. `-pie`, `-no-pie`, `-static-pie` and `-shared` are link options
/// and change none of them. Every row is gcc 13's answer.
///
/// `-fno-pic` was refused as an unrecognized option, and `-fPIC -fno-pie`
/// still claimed `__PIC__`.
#[test]
fn gcc_flags_pic_family_last_one_wins() {
    let probe = "PIC=__PIC__ pic=__pic__ PIE=__PIE__ pie=__pie__\n";
    let defined = |flags: &[&str]| {
        let r = preprocess_text("pic_probe", probe, flags);
        assert!(r.success, "{flags:?}: {}", r.stderr);
        assert!(r.stderr.is_empty(), "{flags:?}: {}", r.stderr);
        r.stdout
    };
    const NONE: &str = "PIC=__PIC__ pic=__pic__ PIE=__PIE__ pie=__pie__";
    const PIC1: &str = "PIC=1 pic=1 PIE=__PIE__ pie=__pie__";
    const PIC2: &str = "PIC=2 pic=2 PIE=__PIE__ pie=__pie__";
    const PIE1: &str = "PIC=1 pic=1 PIE=1 pie=1";
    const PIE2: &str = "PIC=2 pic=2 PIE=2 pie=2";
    if cfg!(target_os = "linux") {
        for (flags, want) in [
            (&[][..], PIE2),
            (&["-fpic"], PIC1),
            (&["-fPIC"], PIC2),
            (&["-fpie"], PIE1),
            (&["-fPIE"], PIE2),
            (&["-fno-pic"], NONE),
            (&["-fno-PIC"], NONE),
            (&["-fno-pie"], NONE),
            (&["-fno-PIE"], NONE),
            (&["-fPIC", "-fno-pie"], NONE),
            (&["-fpie", "-fno-pic"], NONE),
            (&["-fno-pic", "-fpie"], PIE1),
            (&["-fPIC", "-fpie"], PIE1),
            (&["-fpie", "-fPIC"], PIC2),
            (&["-fno-pie", "-fPIC"], PIC2),
            (&["-fpic", "-fPIE"], PIE2),
            (&["-pie"], PIE2),
            (&["-no-pie"], PIE2),
            (&["-static-pie"], PIE2),
            (&["-shared"], PIE2),
            (&["-fno-pic", "-pie"], NONE),
            (&["-shared", "-fno-pic"], NONE),
        ] {
            let out = defined(flags);
            assert!(out.contains(want), "{flags:?}: want {want}, got {out}");
        }
    }
    if cfg!(target_os = "macos") {
        // Mach-O code is position independent whatever was asked, and
        // clang says so.
        for flag in ["-fno-pic", "-fno-pie"] {
            let out = defined(&[flag]);
            assert!(out.contains("PIC=2 pic=2"), "{flag}: {out}");
        }
    }
}

/// Code built with `-fno-pic` links into a fixed-address executable and
/// runs: a global defined in the unit is reached directly, one defined in
/// the other unit through a copy relocation, as gcc's code reaches it.
#[cfg(target_os = "linux")]
#[test]
fn gcc_flags_fno_pic_links_into_a_no_pie_executable() {
    let dir = plib::tmp::Builder::new()
        .prefix("c17_nopic_")
        .tempdir()
        .expect("tempdir");
    let p = |name: &str| dir.path().join(name).to_string_lossy().into_owned();
    std::fs::write(
        p("a.c"),
        "extern int other; extern int (*other_fn)(int);\n\
         static int local = 5; int mine = 7;\n\
         int twice(int x) { return 2 * x; }\n\
         int (*pick(int x))(int) { return x ? twice : other_fn; }\n\
         int main(void) { int *p = &mine; return pick(1)(other) + local + *p + pick(0)(1); }\n",
    )
    .unwrap();
    std::fs::write(
        p("b.c"),
        "int other = 10;\nstatic int three(int x) { return x + 2; }\n\
         int (*other_fn)(int) = three;\n",
    )
    .unwrap();
    for flags in [
        &["-fno-pic"][..],
        &["-fno-PIE", "-O2"],
        &["-fPIC", "-fno-pic"],
    ] {
        for unit in ["a", "b"] {
            let mut args = flags.to_vec();
            let (obj, src) = (p(&format!("{unit}.o")), p(&format!("{unit}.c")));
            args.extend(["-c", "-o", &obj, &src]);
            let r = run_c17(&args);
            assert!(r.success, "{flags:?}: {}", r.stderr);
            assert!(r.stderr.is_empty(), "{flags:?}: {}", r.stderr);
        }
        let r = run_c17(&["-no-pie", "-o", &p("prog"), &p("a.o"), &p("b.o")]);
        assert!(r.success, "{flags:?}: {}", r.stderr);
        assert_eq!(elf_linkage(Path::new(&p("prog"))).e_type, 2, "{flags:?}");
        let status = std::process::Command::new(p("prog")).status().unwrap();
        assert_eq!(status.code(), Some(20 + 5 + 7 + 3), "{flags:?}");
    }
}

/// `-Wl,...` reaches the linker as written and where it was written. It was
/// split at its commas and moved to the end of the link line, so
/// `-Wl,--whole-archive` arrived as a driver option the host `cc` rejects,
/// `-Wl,-soname,x` as two unrelated words, and nothing kept its place
/// relative to the archive it governs.
#[cfg(target_os = "linux")]
#[test]
fn gcc_flags_linker_flags_keep_form_and_position() {
    let dir = plib::tmp::Builder::new()
        .prefix("c17_wl_")
        .tempdir()
        .expect("tempdir");
    let p = |name: &str| dir.path().join(name).to_string_lossy().into_owned();

    // A member nothing references: only `--whole-archive` brings it in, and
    // its constructor then sets the exit status main returns.
    std::fs::write(
        p("member.c"),
        "extern int c17_flag;\n__attribute__((constructor)) static void init(void) { c17_flag = 42; }\n",
    )
    .unwrap();
    let r = run_c17(&["-c", "-o", &p("member.o"), &p("member.c")]);
    assert!(r.success, "{}", r.stderr);
    let status = std::process::Command::new("ar")
        .args(["rcs", &p("libmember.a"), &p("member.o")])
        .status()
        .expect("ar");
    assert!(status.success());
    std::fs::write(
        p("main.c"),
        "int c17_flag;\nint main(void) { return c17_flag; }\n",
    )
    .unwrap();

    let r = run_c17(&[
        "-o",
        &p("whole"),
        &p("main.c"),
        "-Wl,--whole-archive",
        &p("libmember.a"),
        "-Wl,--no-whole-archive",
        "-Wl,--as-needed",
        "-Xlinker",
        "-z",
        "-Xlinker",
        "now",
    ]);
    assert!(r.success, "{}", r.stderr);
    let code = std::process::Command::new(p("whole"))
        .status()
        .unwrap()
        .code();
    assert_eq!(code, Some(42), "the archive member was not linked whole");

    // Without the flag the member stays out.
    let r = run_c17(&["-o", &p("plain"), &p("main.c"), &p("libmember.a")]);
    assert!(r.success, "{}", r.stderr);
    let code = std::process::Command::new(p("plain"))
        .status()
        .unwrap()
        .code();
    assert_eq!(code, Some(0));

    // A shared object named through `-Wl,-soname,...`.
    std::fs::write(p("lib.c"), "int c17_lib(void) { return 7; }\n").unwrap();
    let r = run_c17(&[
        "-shared",
        "-fPIC",
        "-o",
        &p("libsoname.so"),
        "-Wl,-soname,libc17test.so.1",
        &p("lib.c"),
    ]);
    assert!(r.success, "{}", r.stderr);
    let dynamic = std::process::Command::new("readelf")
        .args(["-d", &p("libsoname.so")])
        .output();
    if let Ok(out) = dynamic {
        let text = String::from_utf8_lossy(&out.stdout);
        assert!(text.contains("libc17test.so.1"), "{text}");
    }
}

/// `-fvisibility=hidden` keeps a shared object's own functions to itself: a
/// call inside it binds to its own definition even when the executable
/// exports one of the same name. It was ignored, so the library's symbols
/// were exported and the call bound, through the PLT, to the executable's --
/// which is how a CPython extension carrying its own parser ran the
/// interpreter's instead (test_peg_generator).
#[cfg(target_os = "linux")]
#[test]
fn gcc_flags_fvisibility_hidden_binds_locally() {
    let dir = plib::tmp::Builder::new()
        .prefix("c17_vis_")
        .tempdir()
        .expect("tempdir");
    let p = |name: &str| dir.path().join(name).to_string_lossy().into_owned();
    std::fs::write(
        p("lib.c"),
        "int helper(void) { return 1; }\n\
         __attribute__((visibility(\"default\"))) int lib_entry(void) { return helper(); }\n",
    )
    .unwrap();
    std::fs::write(
        p("main.c"),
        "int helper(void) { return 2; }\nint lib_entry(void);\n\
         int main(void) { return lib_entry() * 10 + helper(); }\n",
    )
    .unwrap();
    let r = run_c17(&[
        "-shared",
        "-fPIC",
        "-fvisibility=hidden",
        "-o",
        &p("libvis.so"),
        &p("lib.c"),
    ]);
    assert!(r.success, "{}", r.stderr);
    let r = run_c17(&[
        "-rdynamic",
        "-o",
        &p("main"),
        &p("main.c"),
        &p("libvis.so"),
        &format!("-Wl,-rpath,{}", dir.path().display()),
    ]);
    assert!(r.success, "{}", r.stderr);
    let code = std::process::Command::new(p("main"))
        .status()
        .unwrap()
        .code();
    assert_eq!(code, Some(12), "the library called the executable's helper");

    if let Ok(out) = std::process::Command::new("nm")
        .args(["-D", "--defined-only", &p("libvis.so")])
        .output()
    {
        let text = String::from_utf8_lossy(&out.stdout);
        assert!(text.contains("lib_entry"), "{text}");
        assert!(!text.contains("helper"), "{text}");
    }

    let r = compile_with("vis.c", MAIN, &["-fvisibility=bogus"]);
    assert!(!r.success);
    assert!(
        r.stderr.contains("unrecognized visibility value"),
        "{}",
        r.stderr
    );
}

/// Under `-fvisibility=hidden` an object keeps the visibility its `extern`
/// declaration gave it: CPython declares every exported object
/// `PyAPI_DATA(...)`, `visibility("default")`, and defines it without the
/// attribute. Without this its extension modules could not find
/// `PyFloat_Type`.
#[cfg(target_os = "linux")]
#[test]
fn gcc_flags_object_visibility_comes_from_its_declaration() {
    let (dir, path) = scratch(
        "objvis.c",
        "extern __attribute__((visibility(\"default\"))) int pub_obj;\nint pub_obj = 1;\n\
         int hid_obj = 2;\nint late = 3;\nextern int late __attribute__((visibility(\"default\")));\n",
    );
    let asm = dir.path().join("objvis.s");
    let r = run_c17(&[
        "-fvisibility=hidden",
        "-S",
        "-o",
        asm.to_str().unwrap(),
        path.to_str().unwrap(),
    ]);
    assert!(r.success, "{}", r.stderr);
    let text = std::fs::read_to_string(&asm).unwrap();
    assert!(text.contains(".hidden hid_obj"), "{text}");
    assert!(!text.contains(".hidden pub_obj"), "{text}");
    assert!(!text.contains(".hidden late"), "{text}");
}

/// `-msse3` .. `-msse4.2`, `-mpopcnt` and `-march=x86-64-v2` define the
/// feature macros gcc's do, each level implying the ones below it -- and
/// gcc's `-msse4.2` implying POPCNT. Without them, only the SSE2 baseline.
#[cfg(target_arch = "x86_64")]
#[test]
fn gcc_flags_x86_simd_levels_define_feature_macros() {
    let probe = "s3=__SSE3__ ss3=__SSSE3__ s41=__SSE4_1__ s42=__SSE4_2__ pc=__POPCNT__\n";
    let defined = |flags: &[&str]| {
        let r = preprocess_text("isa_probe", probe, flags);
        assert!(r.success, "{}", r.stderr);
        r.stdout
    };
    let none = "s3=__SSE3__ ss3=__SSSE3__ s41=__SSE4_1__ s42=__SSE4_2__ pc=__POPCNT__";
    assert!(defined(&[]).contains(none));
    assert!(defined(&["-msse3"]).contains("s3=1 ss3=__SSSE3__"));
    assert!(defined(&["-msse4.1"]).contains("s3=1 ss3=1 s41=1 s42=__SSE4_2__ pc=__POPCNT__"));
    let all = "s3=1 ss3=1 s41=1 s42=1 pc=1";
    assert!(defined(&["-msse4.2"]).contains(all));
    assert!(defined(&["-march=x86-64-v2"]).contains(all));
    assert!(defined(&["-mpopcnt"])
        .contains("s3=__SSE3__ ss3=__SSSE3__ s41=__SSE4_1__ s42=__SSE4_2__ pc=1"));
}

/// The last `-march=` wins, and `-m`/`-mno-` options apply on top of it
/// in command-line order wherever they stand -- the macros gcc 13 defines.
#[cfg(target_arch = "x86_64")]
#[test]
fn gcc_flags_x86_march_last_wins_and_mno_applies() {
    let probe = "s3=__SSE3__ ss3=__SSSE3__ s41=__SSE4_1__ s42=__SSE4_2__ pc=__POPCNT__\n";
    let defined = |flags: &[&str]| {
        let r = preprocess_text("isa_order_probe", probe, flags);
        assert!(r.success, "{flags:?}: {}", r.stderr);
        r.stdout
    };
    let none = "s3=__SSE3__ ss3=__SSSE3__ s41=__SSE4_1__ s42=__SSE4_2__ pc=__POPCNT__";
    let all = "s3=1 ss3=1 s41=1 s42=1 pc=1";
    let cases: &[(&[&str], &str)] = &[
        (&["-march=x86-64-v2", "-march=x86-64"], none),
        (&["-march=x86-64", "-march=x86-64-v2"], all),
        (&["-march=haswell"], all),
        (&["-msse4.2", "-march=x86-64"], all),
        (
            &["-mno-sse4.2", "-march=x86-64-v2"],
            "s3=1 ss3=1 s41=1 s42=__SSE4_2__ pc=1",
        ),
        (
            &["-march=x86-64-v2", "-mno-popcnt"],
            "s3=1 ss3=1 s41=1 s42=1 pc=__POPCNT__",
        ),
        (
            &["-msse4.2", "-mno-sse4.1"],
            "s3=1 ss3=1 s41=__SSE4_1__ s42=__SSE4_2__ pc=__POPCNT__",
        ),
        (&["-mno-sse4.1", "-msse4.2"], all),
        (&["-msse4.2", "-mno-sse4.2"], none),
    ];
    for (flags, want) in cases {
        let got = defined(flags);
        assert!(got.contains(want), "{flags:?}: {got}");
    }
}

// ---------------------------------------------------------------------------
// Static links
// ---------------------------------------------------------------------------

/// What an ELF executable's headers say about how it was linked.
#[cfg(target_os = "linux")]
struct ElfLinkage {
    /// `e_type`: 2 for a fixed-address executable, 3 for a PIE or shared object.
    e_type: u16,
    /// Whether a `PT_INTERP` program header names a dynamic loader.
    has_interp: bool,
}

/// Read `e_type` and look for `PT_INTERP` in a native-endian ELF64 file.
#[cfg(target_os = "linux")]
fn elf_linkage(path: &std::path::Path) -> ElfLinkage {
    const PT_INTERP: u32 = 3;
    let b = std::fs::read(path).expect("read executable");
    assert_eq!(&b[..4], b"\x7fELF", "not an ELF file");
    let u16_at = |o: usize| u16::from_ne_bytes(b[o..o + 2].try_into().unwrap());
    let u32_at = |o: usize| u32::from_ne_bytes(b[o..o + 4].try_into().unwrap());
    let u64_at = |o: usize| u64::from_ne_bytes(b[o..o + 8].try_into().unwrap());
    let phoff = u64_at(32) as usize;
    let phentsize = u16_at(54) as usize;
    let phnum = u16_at(56) as usize;
    let has_interp = (0..phnum).any(|i| u32_at(phoff + i * phentsize) == PT_INTERP);
    ElfLinkage {
        e_type: u16_at(16),
        has_interp,
    }
}

/// `-static` links a program with no dynamic loader that runs, whatever PIE
/// request came before it -- gcc's answer to `-pie -static` is a static,
/// fixed-address executable -- and `-static-pie` links a static PIE.
#[cfg(target_os = "linux")]
#[test]
fn gcc_flags_static_links_without_an_interpreter() {
    let (dir, path) = scratch("st.c", "int main(void) { return 7; }\n");
    let exe = dir.path().join("st");
    for (flags, e_type) in [
        (&["-static"][..], 2u16),
        (&["-pie", "-static"], 2),
        (&["-static", "-pie"], 2),
        (&["-static-pie"], 3),
    ] {
        let _ = std::fs::remove_file(&exe);
        let mut args = flags.to_vec();
        args.extend(["-o", exe.to_str().unwrap(), path.to_str().unwrap()]);
        let r = run_c17(&args);
        assert!(r.success, "{flags:?}: {}", r.stderr);
        let elf = elf_linkage(&exe);
        assert!(!elf.has_interp, "{flags:?}: has a PT_INTERP");
        assert_eq!(elf.e_type, e_type, "{flags:?}: e_type");
        let status = std::process::Command::new(&exe).status().expect("run");
        assert_eq!(status.code(), Some(7), "{flags:?}");
    }
}

// ---------------------------------------------------------------------------
// Leaf frame pointers
// ---------------------------------------------------------------------------

/// `-m[no-]omit-leaf-frame-pointer` is accepted on both architectures
/// (Ubuntu 24.04's default CFLAGS pass `-mno-omit-leaf-frame-pointer`), and
/// what the first asks for is already so: a leaf function at -O2 still sets
/// up its frame pointer.
#[test]
fn gcc_flags_leaf_frame_pointer_flags_are_accepted() {
    let src = "int leaf(int x) { return x + 1; }\n";
    for (target, frame_setup) in [
        ("--target=x86_64-unknown-linux-gnu", "movq %rsp, %rbp"),
        ("--target=aarch64-unknown-linux-gnu", "mov x29, sp"),
    ] {
        for flag in ["-mno-omit-leaf-frame-pointer", "-momit-leaf-frame-pointer"] {
            let asm = crate::common::asm_for_at("c17_leaf_fp_", src, &[target, "-O2", flag]);
            assert!(asm.contains(frame_setup), "{target} {flag}:\n{asm}");
        }
    }
    // And on the host, all the way to an object.
    let r = compile_with("leaf.c", src, &["-mno-omit-leaf-frame-pointer"]);
    assert!(r.success, "{}", r.stderr);
}

// ---------------------------------------------------------------------------
// gcc driver queries
// ---------------------------------------------------------------------------

/// The host's triple in gcc's spelling.
fn host_gcc_triple() -> &'static str {
    if cfg!(all(target_os = "linux", target_arch = "x86_64")) {
        "x86_64-linux-gnu"
    } else if cfg!(all(target_os = "linux", target_arch = "aarch64")) {
        "aarch64-linux-gnu"
    } else if cfg!(all(target_os = "macos", target_arch = "aarch64")) {
        "arm64-apple-darwin"
    } else if cfg!(all(target_os = "macos", target_arch = "x86_64")) {
        "x86_64-apple-darwin"
    } else if cfg!(all(target_os = "freebsd", target_arch = "x86_64")) {
        "x86_64-unknown-freebsd"
    } else {
        "aarch64-unknown-freebsd"
    }
}

#[test]
fn gcc_flags_dumpmachine_names_the_target() {
    let r = run_c17(&["-dumpmachine"]);
    assert!(r.success, "{}", r.stderr);
    assert_eq!(r.stdout, format!("{}\n", host_gcc_triple()));

    for (target, want) in [
        ("--target=aarch64-unknown-linux-gnu", "aarch64-linux-gnu\n"),
        ("--target=x86_64-unknown-linux-gnu", "x86_64-linux-gnu\n"),
        ("--target=aarch64-apple-darwin", "arm64-apple-darwin\n"),
        ("--target=x86_64-apple-darwin", "x86_64-apple-darwin\n"),
        (
            "--target=x86_64-unknown-freebsd",
            "x86_64-unknown-freebsd\n",
        ),
    ] {
        // Before and after the query, and in both spellings of `--target`.
        let r = run_c17(&[target, "-dumpmachine"]);
        assert!(r.success, "{target}: {}", r.stderr);
        assert_eq!(r.stdout, want, "{target}");
        let r = run_c17(&["-dumpmachine", target]);
        assert_eq!(r.stdout, want, "{target}");
        let (flag, value) = target.split_once('=').unwrap();
        let r = run_c17(&["-dumpmachine", flag, value]);
        assert_eq!(r.stdout, want, "{target}");
    }

    let r = run_c17(&["--target=sparc-sun-solaris", "-dumpmachine"]);
    assert!(!r.success);
    assert!(r.stderr.contains("unsupported target"), "{}", r.stderr);
}

/// `-dumpversion` and `-dumpfullversion` agree with `__GNUC__`,
/// `__GNUC_MINOR__` and `__GNUC_PATCHLEVEL__`: configure scripts compare the
/// two, and a version from one that the macros contradict picks code paths
/// for a compiler that is not there.
#[test]
fn gcc_flags_dumpversion_agrees_with_the_gnuc_macros() {
    let macros = preprocess_text(
        "gnuc_ver",
        "__GNUC__.__GNUC_MINOR__.__GNUC_PATCHLEVEL__\n",
        &["-P"],
    );
    assert!(macros.success, "{}", macros.stderr);
    let full = macros.stdout.split_whitespace().collect::<String>();
    let major = full.split('.').next().unwrap().to_string();

    let r = run_c17(&["-dumpversion"]);
    assert!(r.success, "{}", r.stderr);
    assert_eq!(r.stdout, format!("{major}\n"));

    let r = run_c17(&["-dumpfullversion"]);
    assert!(r.success, "{}", r.stderr);
    assert_eq!(r.stdout, format!("{full}\n"));
}

/// gcc spells it with one dash; c17 already knew the two-dash form.
#[test]
fn gcc_flags_print_multiarch_single_dash() {
    let two = run_c17(&["--print-multiarch"]);
    let one = run_c17(&["-print-multiarch"]);
    assert!(one.success, "{}", one.stderr);
    assert_eq!(one.stdout, two.stdout);
    if cfg!(target_os = "linux") {
        assert_eq!(one.stdout, format!("{}\n", host_gcc_triple()));
    }
}

/// The queries about the link step are the host driver's to answer, since it
/// is the host driver that links: c17 prints exactly what `cc` prints.
#[test]
fn gcc_flags_link_queries_are_forwarded_to_the_host_driver() {
    for (query, host) in [
        ("-print-file-name=", "-print-file-name="),
        ("--print-file-name=", "-print-file-name="),
        ("-print-file-name=libc.a", "-print-file-name=libc.a"),
        (
            "-print-file-name=no-such-c17-file",
            "-print-file-name=no-such-c17-file",
        ),
        ("-print-search-dirs", "-print-search-dirs"),
        ("-print-libgcc-file-name", "-print-libgcc-file-name"),
        ("--print-search-dirs", "-print-search-dirs"),
        ("-print-multi-os-directory", "-print-multi-os-directory"),
    ] {
        let want = std::process::Command::new("cc")
            .arg(host)
            .output()
            .expect("run host cc");
        let r = run_c17(&[query]);
        assert_eq!(r.success, want.status.success(), "{query}: {}", r.stderr);
        assert_eq!(r.stdout, String::from_utf8_lossy(&want.stdout), "{query}");
    }
}

/// Bare `-v` is gcc's version banner on stderr, which libtool and autoconf
/// run and log; with operands it is still c17's verbose compile.
#[test]
fn gcc_flags_bare_v_prints_a_version_banner() {
    let r = run_c17(&["-v"]);
    assert!(r.success, "{}", r.stderr);
    assert!(r.stdout.is_empty(), "{}", r.stdout);
    assert!(
        r.stderr
            .contains(&format!("c17 version {}", env!("CARGO_PKG_VERSION"))),
        "{}",
        r.stderr
    );
    assert!(
        r.stderr
            .contains(&format!("Target: {}\n", host_gcc_triple())),
        "{}",
        r.stderr
    );
    let full = run_c17(&["-dumpfullversion"]).stdout;
    assert!(
        r.stderr.contains(&format!("gcc version {} ", full.trim())),
        "{}",
        r.stderr
    );

    let r = run_c17(&["--target=aarch64-unknown-linux-gnu", "-v"]);
    assert!(
        r.stderr.contains("Target: aarch64-linux-gnu\n"),
        "{}",
        r.stderr
    );

    // With an operand, `-v` keeps its verbose meaning and compiles.
    let r = compile_with("v.c", MAIN, &["-v"]);
    assert!(r.success, "{}", r.stderr);
}

/// The directories between `start` and gcc's "End of search list." line of a
/// `-v` stderr, one per line, as perl's h2ph and CMake read them.
fn search_list<'a>(stderr: &'a str, start: &str) -> Vec<&'a str> {
    stderr
        .lines()
        .skip_while(|l| *l != start)
        .skip(1)
        .take_while(|l| l.starts_with(' '))
        .map(str::trim)
        .collect()
}

/// `-v` with an operand to preprocess prints gcc's include search list on
/// stderr. perl's h2ph runs `cc -v -E - </dev/null` and converts the headers
/// it finds in those directories; with no list it converted only
/// /usr/include, so Debian's perl shipped a syslog.ph requiring a stdarg.ph
/// nothing had made. The bundled headers' slot names the directory c17
/// answers for `-print-file-name=include`.
#[test]
fn gcc_flags_v_prints_the_include_search_list() {
    let (dir, src) = scratch("empty.c", "");
    let quote = dir.path().join("q");
    let angle = dir.path().join("a");
    std::fs::create_dir(&quote).unwrap();
    std::fs::create_dir(&angle).unwrap();
    let r = run_c17(&[
        "-v",
        "-E",
        "-iquote",
        quote.to_str().unwrap(),
        "-I",
        angle.to_str().unwrap(),
        src.to_str().unwrap(),
    ]);
    assert!(r.success, "{}", r.stderr);
    assert!(r.stderr.contains("End of search list.\n"), "{}", r.stderr);
    let quoted = search_list(&r.stderr, "#include \"...\" search starts here:");
    assert_eq!(quoted, [quote.to_str().unwrap()], "{}", r.stderr);
    let system = search_list(&r.stderr, "#include <...> search starts here:");
    assert_eq!(
        system.first(),
        Some(&angle.to_str().unwrap()),
        "{}",
        r.stderr
    );

    let include = run_c17(&["-print-file-name=include"]).stdout;
    let include = include.trim();
    let bundled = Path::new(include).is_absolute() && Path::new(include).is_dir();
    assert_eq!(system.contains(&include), bundled, "{}", r.stderr);
    if cfg!(target_os = "linux") {
        // The bundled headers come ahead of the system's, as c17 searches.
        assert_eq!(system.last(), Some(&"/usr/include"), "{}", r.stderr);
        if bundled {
            assert!(system[1..].starts_with(&[include]), "{}", r.stderr);
        }
    }

    // -nostdinc drops the bundled headers and the target's directories, and
    // the list says so.
    let r = run_c17(&["-v", "-E", "-nostdinc", src.to_str().unwrap()]);
    assert!(r.success, "{}", r.stderr);
    let system = search_list(&r.stderr, "#include <...> search starts here:");
    assert!(system.is_empty(), "{}", r.stderr);
}

// ---------------------------------------------------------------------------
// Response files
// ---------------------------------------------------------------------------

/// `@file` reads arguments from `file` with gcc's quoting rules, expands
/// nested `@file`s, and leaves an unreadable one as a literal argument.
#[test]
fn gcc_flags_response_files_are_expanded() {
    let (dir, src) = scratch("rsp.c", "A B C D\n");
    let inner = dir.path().join("inner.rsp");
    std::fs::write(&inner, "-DC=3\n").unwrap();
    let outer = dir.path().join("outer.rsp");
    std::fs::write(
        &outer,
        format!(
            "-DA=1 '-DB=two  words'\n  \"-DD=a\\\"q\\\"\"\t@{}\n",
            inner.display()
        ),
    )
    .unwrap();
    let outer_arg = format!("@{}", outer.display());
    let r = run_c17(&["-E", "-P", &outer_arg, src.to_str().unwrap()]);
    assert!(r.success, "{}", r.stderr);
    assert_eq!(
        r.stdout.split_whitespace().collect::<Vec<_>>(),
        ["1", "two", "words", "3", "a\"q\""],
        "{}",
        r.stdout
    );

    // Backslash escapes outside quotes: an escaped space joins words.
    std::fs::write(&outer, "-DA=x\\ y\n").unwrap();
    let r = run_c17(&["-E", "-P", &outer_arg, src.to_str().unwrap()]);
    assert!(r.success, "{}", r.stderr);
    assert!(r.stdout.starts_with("x y B"), "{}", r.stdout);

    // A file that cannot be read stays a literal operand, reported as one:
    // a linker input that does not exist, which is an error as in gcc.
    let missing = format!("@{}", dir.path().join("missing.rsp").display());
    let r = run_c17(&["-E", &missing, src.to_str().unwrap()]);
    assert!(!r.success, "{}", r.stderr);
    assert!(
        r.stderr
            .contains(&format!("{missing}: linker input file not found")),
        "{}",
        r.stderr
    );

    // A response file that includes itself is refused, not followed forever.
    let cycle = dir.path().join("cycle.rsp");
    std::fs::write(&cycle, format!("-DA=1 @{}\n", cycle.display())).unwrap();
    let cycle_arg = format!("@{}", cycle.display());
    let r = run_c17(&["-E", &cycle_arg, src.to_str().unwrap()]);
    assert!(!r.success);
    assert!(r.stderr.contains("response file"), "{}", r.stderr);
}

/// The link line is read from the expanded arguments too: the operands and
/// the `-l` order in a response file reach the link step.
#[test]
fn gcc_flags_response_file_drives_a_link() {
    let (dir, src) = scratch(
        "rsplink.c",
        "#include <math.h>\nint main(void) { return (int)sqrt(49.0); }\n",
    );
    let exe = dir.path().join("rsplink");
    let rsp = dir.path().join("link.rsp");
    std::fs::write(
        &rsp,
        format!("-o '{}' '{}' -lm\n", exe.display(), src.display()),
    )
    .unwrap();
    let r = run_c17(&[&format!("@{}", rsp.display())]);
    assert!(r.success, "{}", r.stderr);
    let status = std::process::Command::new(&exe).status().expect("run");
    assert_eq!(status.code(), Some(7));
}

/// An input c17 does not compile is a linker input, and one that does not
/// exist is an error even when nothing is linked, as in gcc ("linker input
/// file not found"): a build that names a file it never made must fail,
/// not succeed with a warning. One that exists is only unused.
#[test]
fn gcc_flags_missing_linker_input_is_an_error_without_linking() {
    let (dir, src) = scratch("t.c", MAIN);
    let missing = dir.path().join("nofile.zz");
    let present = dir.path().join("present.zz");
    std::fs::write(&present, "").expect("write");
    let obj = dir.path().join("t.o");
    let (src, obj) = (src.to_str().unwrap(), obj.to_str().unwrap());
    for mode in [&["-c", "-o", obj][..], &["-E"]] {
        let mut args = mode.to_vec();
        args.extend([missing.to_str().unwrap(), src]);
        let run = run_c17(&args);
        assert!(!run.success, "{mode:?}: {}", run.stderr);
        assert!(
            run.stderr.contains("linker input file not found"),
            "{mode:?}: {}",
            run.stderr
        );
        let mut args = mode.to_vec();
        args.extend([present.to_str().unwrap(), src]);
        let run = run_c17(&args);
        assert!(run.success, "{mode:?} existing: {}", run.stderr);
    }
}

/// A missing object or archive is the same error when nothing is linked,
/// under every mode that stops short of the link; the source operands are
/// still compiled, as in gcc.
#[test]
fn gcc_flags_missing_object_is_an_error_without_linking() {
    let (dir, src) = scratch("t.c", MAIN);
    let out = dir.path().join("t.out");
    let (src, out) = (src.to_str().unwrap(), out.to_str().unwrap());
    for name in ["gone.o", "libgone.a"] {
        let missing = dir.path().join(name);
        let missing = missing.to_str().unwrap();
        for mode in ["-c", "-S", "-E"] {
            let run = run_c17(&[mode, "-o", out, missing, src]);
            assert!(!run.success, "{mode} {name}: {}", run.stderr);
            assert!(
                run.stderr.contains(&format!(
                    "error: {missing}: linker input file not found: No such file or directory"
                )),
                "{mode} {name}: {}",
                run.stderr
            );
            assert!(
                Path::new(out).exists(),
                "{mode} {name}: source not compiled"
            );
            std::fs::remove_file(out).expect("remove");
        }
    }
}

/// `-fcf-protection` is honoured, not ignored: every level gcc has is
/// accepted without the "unrecognized option" warning, as is
/// `-fno-cf-protection`, and a level gcc does not have is an error in its
/// words.
#[test]
fn gcc_flags_cf_protection() {
    for flag in [
        "-fcf-protection",
        "-fcf-protection=full",
        "-fcf-protection=branch",
        "-fcf-protection=return",
        "-fcf-protection=none",
        "-fcf-protection=check",
        "-fno-cf-protection",
    ] {
        let r = compile_with("cet.c", MAIN, &[flag]);
        assert!(r.success, "{flag}: {}", r.stderr);
        assert!(!r.stderr.contains("unrecognized"), "{flag}: {}", r.stderr);
    }
    let r = compile_with("cet.c", MAIN, &["-fcf-protection=bogus"]);
    assert!(!r.success);
    assert!(
        r.stderr
            .contains("unknown Control-Flow Protection Level: bogus"),
        "{}",
        r.stderr
    );
}

/// `-ftls-model=` takes gcc's four model names, and refuses anything else in
/// gcc's words. It was ignored with a warning.
#[test]
fn gcc_flags_tls_model() {
    let src = "_Thread_local int t;\nint main(void) { return t; }\n";
    for model in [
        "global-dynamic",
        "local-dynamic",
        "initial-exec",
        "local-exec",
    ] {
        let flag = format!("-ftls-model={model}");
        let r = compile_with("tls.c", src, &[&flag]);
        assert!(r.success, "{flag}: {}", r.stderr);
        assert!(r.stderr.is_empty(), "{flag}: {}", r.stderr);
    }
    let r = compile_with("tls.c", src, &["-ftls-model=bogus"]);
    assert!(!r.success);
    assert!(
        r.stderr.contains("error: unknown TLS model 'bogus'")
            && r.stderr.contains(
                "note: valid arguments to '-ftls-model=' are: \
                 global-dynamic initial-exec local-dynamic local-exec"
            ),
        "{}",
        r.stderr
    );
    let r = compile_with("tls.c", src, &["-ftls-model="]);
    assert!(!r.success);
    assert!(
        r.stderr
            .contains("error: missing argument to '-ftls-model='"),
        "{}",
        r.stderr
    );
}

/// An option c17 does not know is refused as gcc refuses it, under c17's
/// name and naming the option as it was written: configure's `-qversion`
/// probe came back as clap's `error: unexpected argument '-q' found`, with a
/// usage block and no `c17:` to say which program spoke.
#[test]
fn gcc_flags_unknown_option_is_refused_in_gccs_words() {
    for (args, culprit) in [
        (&["-qversion"][..], "-qversion"),
        (&["-version"], "-version"),
        (&["--no-such-c17-option"], "--no-such-c17-option"),
        // The culprit is the argument that held the unknown letter, wherever
        // it stands, not a value that happens to contain it.
        (&["-I/q", "-c", "-no-gcc", "x.c"], "-no-gcc"),
    ] {
        let r = run_c17(args);
        assert!(!r.success, "{args:?}");
        assert_eq!(
            r.stderr,
            format!("c17: error: unrecognized command-line option '{culprit}'\n"),
            "{args:?}"
        );
    }
    // A missing value, in gcc's words too.
    let r = run_c17(&["x.c", "-o"]);
    assert!(!r.success);
    assert_eq!(r.stderr, "c17: error: missing argument to '-o'\n");
    // Nothing to compile.
    let r = run_c17(&["-c"]);
    assert!(!r.success);
    assert_eq!(r.stderr, "c17: fatal error: no input files\n");
    // Every other refusal from the parser still says who is speaking.
    let r = run_c17(&["-Obogus", "x.c"]);
    assert!(!r.success);
    assert!(r.stderr.starts_with("c17: error: "), "{}", r.stderr);
    assert!(!r.stderr.contains("Usage:"), "{}", r.stderr);
}
