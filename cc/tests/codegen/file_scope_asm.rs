//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// GNU basic asm at file scope, end to end: `.symver` versioning a shared
// object's symbols (the xz-utils and libxcrypt idiom), a function written in
// a file-scope asm and called from C, and C definitions placed after an asm
// that switched sections.
// Cases that only inspect assembly are in
// `cc/test_asm/codegen_file_scope_asm.rs`.
//

use crate::common::{
    asm_for_at, compile_and_run_everywhere, compile_expect_error, preprocess_text, run_c17,
};

/// `.symver` in file-scope asm gives a shared object versioned symbols: one
/// default version, `@@FSASM_2`, that a new link binds to, and one older,
/// `@FSASM_1`, kept for binaries linked against it. c17 dropped the asm, so
/// the library exported neither and the link below failed.
#[cfg(target_os = "linux")]
#[test]
fn file_scope_asm_symver_versions_shared_object_symbols() {
    let dir = plib::tmp::Builder::new()
        .prefix("c17_fsasm_symver_")
        .tempdir()
        .expect("tempdir");
    let p = |f: &str| dir.path().join(f).to_string_lossy().into_owned();
    std::fs::write(
        p("lib.c"),
        r#"
int fsasm_get_v1(void) { return 1; }
int fsasm_get_v2(void) { return 2; }
__asm__(".symver fsasm_get_v1, fsasm_get@FSASM_1");
__asm__(".symver fsasm_get_v2, " "fsasm_get@@FSASM_2");
"#,
    )
    .unwrap();
    std::fs::write(
        p("lib.map"),
        "FSASM_1 { global: fsasm_get; local: *; };\nFSASM_2 { global: fsasm_get; } FSASM_1;\n",
    )
    .unwrap();
    std::fs::write(
        p("main.c"),
        "int fsasm_get(void);\nint main(void) { return fsasm_get() == 2 ? 0 : 1; }\n",
    )
    .unwrap();

    let script = format!("-Wl,--version-script={}", p("lib.map"));
    let r = run_c17(&[
        "-shared",
        "-fPIC",
        "-o",
        &p("libfsasm.so"),
        &script,
        &p("lib.c"),
    ]);
    assert!(r.success, "{}", r.stderr);
    assert!(!r.stderr.contains("warning"), "{}", r.stderr);

    let syms = std::process::Command::new("readelf")
        .args(["--dyn-syms", "-W", &p("libfsasm.so")])
        .output()
        .expect("readelf");
    let text = String::from_utf8_lossy(&syms.stdout);
    assert!(text.contains("fsasm_get@@FSASM_2"), "{text}");
    assert!(text.contains("fsasm_get@FSASM_1"), "{text}");

    let lib_dir = format!("-L{}", dir.path().display());
    let r = run_c17(&["-o", &p("main"), &p("main.c"), &lib_dir, "-lfsasm"]);
    assert!(r.success, "{}", r.stderr);
    let run = std::process::Command::new(p("main"))
        .env("LD_LIBRARY_PATH", dir.path())
        .status()
        .expect("run main");
    assert_eq!(run.code(), Some(0));
}

/// A function written in a file-scope asm is defined in `.text` and C calls
/// it. The asm label gives it one name on ELF and Mach-O alike.
#[test]
fn file_scope_asm_defines_a_function_c_calls() {
    let src = r#"
int fsasm_seven(void) __asm__("fsasm_seven_sym");
#if defined(__x86_64__)
#define BODY "movl $7, %eax\n\tret\n"
#elif defined(__aarch64__)
#define BODY "mov w0, #7\n\tret\n"
#endif
#if defined(__ELF__)
#define TYPE ".type fsasm_seven_sym, %function\n"
#else
#define TYPE ""
#endif
__asm__(".text\n"
        ".globl fsasm_seven_sym\n"
        TYPE
        ".p2align 2\n"
        "fsasm_seven_sym:\n\t" BODY);
int main(void) { return fsasm_seven() == 7 ? 0 : 1; }
"#;
    compile_and_run_everywhere("fsasm_function", src);
}

/// An asm that leaves the assembler in a data section, then a C function;
/// an asm that leaves it in the text section, then initialized C data. The
/// function must still run (a function in a non-executable section faults)
/// and the object must still be writable (one in text faults on the store).
#[test]
fn file_scope_asm_section_switch_then_c_definitions() {
    let src = r#"
extern int fsasm_word __asm__("fsasm_word_sym");
__asm__(".data\n.globl fsasm_word_sym\n.p2align 2\nfsasm_word_sym:\n\t.long 41\n");
__attribute__((noinline)) int fsasm_after_fn(int x) { return x + 1; }
__asm__(".text\n");
int fsasm_after_data = 3;
int main(void) {
    fsasm_after_data += fsasm_word;
    fsasm_word = fsasm_after_fn(fsasm_word);
    if (fsasm_after_data != 44 || fsasm_word != 42)
        return 1;
    return 0;
}
"#;
    compile_and_run_everywhere("fsasm_sections", src);
}

/// On the host, in the assembly itself: the asm text sits between the
/// definitions around it, and each C definition after it is preceded by its
/// own section directive.
#[test]
fn file_scope_asm_placement_in_host_assembly() {
    let src = r##"
int fsasm_before(void) { return 1; }
__asm__("# fsasm one\n.data");
int fsasm_fn(void) { return 2; }
__asm__("# fsasm two\n.text");
int fsasm_obj = 5;
"##;
    let asm = asm_for_at("c17_fsasm_host_", src, &[]);
    let at = |needle: &str| {
        asm.find(needle)
            .unwrap_or_else(|| panic!("no {needle:?} in:\n{asm}"))
    };
    let (before, one, func, two, obj) = (
        at("fsasm_before:"),
        at("# fsasm one\n.data\n"),
        at("fsasm_fn:"),
        at("# fsasm two\n.text\n"),
        at("fsasm_obj:"),
    );
    assert!(
        before < one && one < func && func < two && two < obj,
        "out of order:\n{asm}"
    );
    let between_text = &asm[one..func];
    assert!(
        between_text.contains("\n.text\n") || between_text.contains("__TEXT,__text"),
        "no text section before fsasm_fn:\n{asm}"
    );
    let between_data = &asm[two..obj];
    assert!(
        between_data.contains("\n.data\n") || between_data.contains("__DATA,__data"),
        "no data section before fsasm_obj:\n{asm}"
    );
}

/// gcc takes only basic asm at file scope; operands are an error where its
/// `)` was expected.
#[test]
fn file_scope_extended_asm_is_rejected() {
    compile_expect_error(
        "fsasm_extended",
        "int x;\n__asm__(\"nop\" : \"=r\"(x));\n",
        "expected ')' before ':' token",
    );
    compile_expect_error(
        "fsasm_volatile",
        "__asm__ volatile (\"nop\");\n",
        "expected '(' before 'volatile'",
    );
}

/// Preprocessing leaves a file-scope asm as written, and `symver` is still
/// no attribute c17 knows.
#[test]
fn file_scope_asm_preprocessing_is_unchanged() {
    let r = preprocess_text(
        "fsasm_pp",
        "#define V \"@@V2\"\n__asm__(\".symver a, b\" V);\n\
         #if __has_attribute(__symver__) || __has_attribute(symver)\nbad\n#endif\n",
        &[],
    );
    assert!(r.success, "{}", r.stderr);
    assert!(
        r.stdout.contains("__asm__(\".symver a, b\" \"@@V2\");"),
        "{}",
        r.stdout
    );
    assert!(!r.stdout.contains("bad"), "{}", r.stdout);
}
