//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Objects placed in named sections: which declarations reach one, and the
// flags the section gets when several objects share it. Every expectation was
// read off gcc 13's assembly for the same source. ELF triples throughout, so
// each case runs on any host.
//

use super::asm_probe::{asm_for_with, section_of, AARCH64_LINUX, X86_64_LINUX};
use crate::test_compile::compile;

/// Block-scope statics in named sections, as Linux writes them: its
/// `DO_ONCE` flags in `.data.once`, its dynamic-debug descriptors in
/// `__dyndbg`, and its computed-goto tables in a section whose name carries
/// its own flags and a `#` to comment out the compiler's. The attributes were
/// parsed and dropped, so every one landed in `.data`/`.bss`/`.data.rel.ro`.
#[test]
fn codegen_block_scope_static_keeps_its_symbol_attributes() {
    let src = r#"
int f(int x)
{
    static int once __attribute__((__section__(".data.once")));
    static struct { const char *fmt; int flags; } d
        __attribute__((__aligned__(8), __section__("__dyndbg"))) = { "x", 0 };
    static int keep __attribute__((used, section("kept"))) = 1;
    if (!once) { once = 1; return d.flags + x; }
    return x;
}
int run(int i)
{
    static const void *const jt[2]
        __attribute__((__section__(".rodata..c_jump_table,\"a\",@progbits #"))) = { &&a, &&b };
    goto *jt[i & 1];
a: return 1;
b: return 2;
}
"#;
    for triple in [X86_64_LINUX, AARCH64_LINUX] {
        for opt in ["-O0", "-O2"] {
            let asm = asm_for_with("static_local_section", triple, src, &[opt]);
            let at = |name: &str| section_of(&asm, name).unwrap_or("<none>").to_string();
            assert_eq!(
                at("f.once.0"),
                ".section .data.once,\"aw\"",
                "{triple} {opt}:\n{asm}"
            );
            assert_eq!(
                at("f.d.1"),
                ".section __dyndbg,\"aw\"",
                "{triple} {opt}:\n{asm}"
            );
            assert_eq!(
                at("f.keep.2"),
                ".section kept,\"aw\"",
                "{triple} {opt}:\n{asm}"
            );
            assert_eq!(
                at("run.jt.3"),
                ".section .rodata..c_jump_table,\"a\",@progbits #",
                "{triple} {opt}:\n{asm}"
            );
        }
    }
}

/// An attribute written right after `struct TAG` in a declaration that only
/// refers to the structure belongs to the object declared: Linux's
/// `static struct trace_event_call __used __section("_ftrace_events")
/// *__event_x = &event_x;`. Only the layout attributes were kept, as if they
/// described the type, so the pointer landed in `.data`. After a
/// definition's closing brace an attribute is still the type's, and gcc
/// ignores `section` there (with a warning).
#[test]
fn codegen_attribute_after_struct_reference_is_the_declarations() {
    let src = r#"
struct ev { int id; };
static struct ev event_x = { 1 };
static struct ev __attribute__((__used__)) __attribute__((__section__("_ftrace_events"))) *__event_x = &event_x;
struct ev __attribute__((section("s2"))) a2, b2 = { 2 };
struct ev __attribute__((weak)) wv = { 2 };
struct ev __attribute__((aligned(64))) av = { 2 };
union u1 __attribute__((section("s5"))) *up;
struct ev2 { int id; } __attribute__((section("s3"))) dv = { 3 };
struct __attribute__((section("s7"))) ev *p7;
void f(void) { static struct ev __attribute__((used, section("s6"))) *lp = &event_x; }
"#;
    let asm = asm_for_with("struct_ref_attrs", X86_64_LINUX, src, &["-O2"]);
    let at = |name: &str| section_of(&asm, name).unwrap_or("<none>").to_string();
    assert_eq!(at("__event_x"), ".section _ftrace_events,\"aw\"", "{asm}");
    // Every declarator of the declaration, as with `int`.
    assert_eq!(at("a2"), ".section s2,\"aw\"", "{asm}");
    assert_eq!(at("b2"), ".section s2,\"aw\"", "{asm}");
    assert_eq!(at("up"), ".section s5,\"aw\"", "{asm}");
    assert_eq!(at("f.lp.0"), ".section s6,\"aw\"", "{asm}");
    assert!(asm.contains(".weak wv"), "{asm}");
    assert!(asm.contains(".p2align 6\nav:"), "{asm}");
    // The definition's trailing attribute and the one before the tag are
    // the type's: no section for either object.
    assert_eq!(at("dv"), ".data", "{asm}");
    assert!(!asm.contains("s3"), "{asm}");
    assert!(!asm.contains("s7"), "{asm}");
}

/// Objects sharing a named section give it one set of flags, as gcc's
/// `get_section` does: a `const` object needing relocations is read-only data
/// in position-dependent code and writable-until-relocated (`.data.rel.ro`)
/// data in position-independent code, and a read-only object beside it
/// follows it there. Two flag sets for one section made `as` stop with
/// "changed section attributes" -- the kernel's `__initconst` tables.
#[test]
fn codegen_named_section_flags_agree_across_its_objects() {
    let src = r#"
extern int f(void);
struct d { int (*cb)(void); const char *s; };
static const struct d tab[] __attribute__((__section__(".init.rodata"), used)) = { { f, "x" } };
static const char str[] __attribute__((__section__(".init.rodata"), used)) = "hello";
static const char str2[] __attribute__((__section__(".init.rodata"), used)) = "hello";
static const char alone[] __attribute__((__section__("ro_alone"), used)) = "hello";
static int z6 __attribute__((__section__("s6"), used));
static const int z7 __attribute__((__section__("s7"), used));
static __thread int t5 __attribute__((__section__("s5"), used)) = 1;
"#;
    for triple in [X86_64_LINUX, AARCH64_LINUX] {
        for (pic, flags) in [
            ("-fno-pic", "\"a\""),
            ("-fPIC", "\"aw\""),
            ("-fPIE", "\"aw\""),
        ] {
            let asm = asm_for_with("named_section_flags", triple, src, &["-O2", pic]);
            let at = |name: &str| section_of(&asm, name).unwrap_or("<none>").to_string();
            let init = format!(".section .init.rodata,{flags}");
            for name in ["tab", "str", "str2"] {
                assert_eq!(at(name), init, "{triple} {pic} {name}:\n{asm}");
            }
            assert_eq!(
                at("alone"),
                ".section ro_alone,\"a\"",
                "{triple} {pic}:\n{asm}"
            );
            assert_eq!(at("z6"), ".section s6,\"aw\"", "{triple} {pic}:\n{asm}");
            assert_eq!(at("z7"), ".section s7,\"a\"", "{triple} {pic}:\n{asm}");
            assert_eq!(
                at("t5"),
                ".section s5,\"awT\",@progbits",
                "{triple} {pic}:\n{asm}"
            );
        }
    }
}

/// What one section cannot hold: writable data beside read-only data, data
/// beside code, thread-local beside ordinary data. gcc reports each section
/// once, at the object that met an earlier claim -- functions are claimed
/// first, then objects from the end of the unit back.
#[test]
fn codegen_named_section_type_conflicts_are_errors() {
    let cases: &[(&str, &str)] = &[
        (
            "static char wr[] __attribute__((section(\"s3\"), used)) = \"hello\";\n\
             static const char ro[] __attribute__((section(\"s3\"), used)) = \"hello\";\n",
            "'wr' causes a section type conflict with 'ro'",
        ),
        (
            "extern int g(void);\n\
             static int * const rel __attribute__((section(\"s1\"), used)) = 0;\n\
             static int (* const rel2)(void) __attribute__((section(\"s1\"), used)) = g;\n\
             static int wr __attribute__((section(\"s1\"), used)) = 1;\n",
            "'rel2' causes a section type conflict with 'wr'",
        ),
        (
            "static int wr __attribute__((section(\"s1\"), used)) = 1;\n\
             __attribute__((section(\"s1\"))) int fn(void) { return 0; }\n",
            "'wr' causes a section type conflict with 'fn'",
        ),
        (
            "static __thread int t __attribute__((section(\"s1\"), used)) = 1;\n\
             static int d __attribute__((section(\"s1\"), used)) = 1;\n",
            "'t' causes a section type conflict with 'd'",
        ),
        (
            "void f(void) { static int wr __attribute__((section(\"s1\"), used)) = 1; }\n\
             static const int ro __attribute__((section(\"s1\"), used)) = 1;\n",
            "'wr' causes a section type conflict with 'ro'",
        ),
    ];
    for (src, expected) in cases {
        for pic in ["-fno-pic", "-fPIC"] {
            let target = format!("--target={X86_64_LINUX}");
            let c = compile("section_conflict", src, &[&target, pic]);
            assert!(!c.success, "{pic} accepted:\n{src}");
            assert!(
                c.stderr.contains(expected),
                "{pic}: want {expected:?}:\n{}",
                c.stderr
            );
            assert_eq!(
                c.stderr.matches("section type conflict").count(),
                1,
                "{}",
                c.stderr
            );
        }
    }
}

/// `retain`: the definition gets a section of its own, as under
/// `-ffunction-sections` and `-fdata-sections`, carrying `SHF_GNU_RETAIN`
/// (`R`) so `ld --gc-sections` keeps it; in a section the program named, that
/// section is marked so. A tentative definition is no common symbol, and a
/// block-scope static is retained too. gcc 13's flags, with binutils 2.36 or
/// later. The attribute was ignored with a warning.
#[test]
fn codegen_retain_marks_its_own_section() {
    let src = r#"
__attribute__((retain, used)) int rv = 1;
__attribute__((__retain__)) int rz;
__attribute__((retain)) int rt;
__attribute__((retain)) const int rc = 5;
__attribute__((retain)) const char *const rp = "x";
__attribute__((retain)) __thread int rtl = 3;
__attribute__((retain)) __thread int rtz;
__attribute__((retain, used, section("mysec"))) int sv = 3;
__attribute__((retain)) void rf(void) {}
__attribute__((retain, section("fsec"))) void sf(void) {}
extern int ext;
int g(void) { static int s __attribute__((retain)) = 1; return s++ + ext; }
"#;
    let want = [
        ("rv", ".section .data.rv,\"awR\""),
        ("rz", ".section .bss.rz,\"awR\",@nobits"),
        ("rt", ".section .bss.rt,\"awR\",@nobits"),
        ("rc", ".section .rodata.rc,\"aR\""),
        ("rp", ".section .data.rel.ro.rp,\"awR\""),
        ("rtl", ".section .tdata.rtl,\"awTR\",@progbits"),
        ("rtz", ".section .tbss.rtz,\"awTR\",@nobits"),
        ("sv", ".section mysec,\"awR\""),
        ("rf", ".section .text.rf,\"axR\""),
        ("sf", ".section fsec,\"axR\""),
        ("g.s.0", ".section .data.g.s.0,\"awR\""),
    ];
    for triple in [X86_64_LINUX, AARCH64_LINUX] {
        for flags in [&["-O0"][..], &["-O2", "-fcommon"], &["-O2", "-fPIC"]] {
            let mut all = vec![triple];
            all.extend_from_slice(flags);
            let target = format!("--target={triple}");
            let mut args = vec![target.as_str()];
            args.extend_from_slice(flags);
            let c = compile("retain", src, &args);
            assert!(c.stderr.is_empty(), "{all:?}:\n{}", c.stderr);
            let asm = c.asm.expect("compiled");
            for (name, section) in want {
                assert_eq!(
                    section_of(&asm, name),
                    Some(section),
                    "{all:?} {name}:\n{asm}"
                );
            }
            assert_eq!(section_of(&asm, "ext"), None, "{all:?}:\n{asm}");
        }
    }
}
