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

    // `-x none` goes back to suffixes, so a later `.h` is not compiled.
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
    assert!(r.stderr.contains("unrecognized file type"), "{}", r.stderr);

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

/// `__PIC__`/`__pic__` whenever the code is position independent, and
/// `__PIE__`/`__pie__` when it is for a PIE -- the macros a `.S` file or an
/// inline asm statement tests before choosing a GOT access.
#[test]
fn gcc_flags_pic_macros_follow_the_code() {
    let probe = "PIC=__PIC__ pic=__pic__ PIE=__PIE__ pie=__pie__\n";
    let defined = |flags: &[&str]| {
        let r = preprocess_text("pic_probe", probe, flags);
        assert!(r.success, "{}", r.stderr);
        r.stdout
    };
    let out = defined(&["-fPIC", "-fno-pie"]);
    assert!(out.contains("PIC=2 pic=2 PIE=__PIE__"), "{out}");
    let out = defined(&["-fpie"]);
    assert!(out.contains("PIC=2 pic=2 PIE=2 pie=2"), "{out}");
    if cfg!(target_os = "linux") {
        // PIE is the Linux default, as it is for gcc there.
        let out = defined(&[]);
        assert!(out.contains("PIC=2 pic=2 PIE=2 pie=2"), "{out}");
        let out = defined(&["-fno-pie"]);
        assert!(out.contains("PIC=__PIC__"), "{out}");
    }
    if cfg!(target_os = "macos") {
        let out = defined(&["-fno-pie"]);
        assert!(out.contains("PIC=2"), "Mach-O is always PIC: {out}");
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
