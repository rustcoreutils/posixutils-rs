//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// How the object file lays out and links to its symbols, as the assembly
// shows it: `-ffunction-sections`, `-fdata-sections`, `-fcommon` and
// `-fno-plt`. The cases that must link or run are in
// `tests/codegen/object_layout.rs`.
//

use super::asm_probe::{asm_for_with, body_of, AARCH64_DARWIN, AARCH64_LINUX, X86_64_LINUX};

const DEFINITIONS: &str = r#"
int t;
static int s;
int z = 0;
int i = 1;
const int c = 2;
int *const pc = &i;
__thread int tt;
__thread int ti = 3;
__attribute__((section(".mine"))) int named = 4;
int f(void) { return t + s + z + i + c + *pc + tt + ti + named; }
static int sf(void) { return 5; }
int (*fp)(void) = sf;
"#;

/// The `.section` lines of `asm`, trimmed.
fn sections(asm: &str) -> Vec<&str> {
    asm.lines()
        .map(str::trim)
        .filter(|l| l.starts_with(".section"))
        .collect()
}

/// `-ffunction-sections` and `-fdata-sections` put each function and each
/// object in a section of its own, named as gcc names it -- the section it
/// would share, `.`, its name -- so `ld --gc-sections` can drop what nothing
/// uses. A named section stays the program's, and Mach-O, which has no use
/// for it, is left alone as clang leaves it.
#[test]
fn codegen_function_and_data_sections_name_each_symbol() {
    let flags = ["-O0", "-ffunction-sections", "-fdata-sections"];
    for triple in [X86_64_LINUX, AARCH64_LINUX] {
        let asm = asm_for_with("own_sections", triple, DEFINITIONS, &flags);
        let found = sections(&asm);
        for want in [
            ".section .text.f,\"ax\"",
            ".section .text.sf,\"ax\"",
            ".section .bss.t,\"aw\",@nobits",
            ".section .bss.s,\"aw\",@nobits",
            ".section .bss.z,\"aw\",@nobits",
            ".section .data.i,\"aw\"",
            ".section .rodata.c,\"a\"",
            ".section .data.rel.ro.pc,\"aw\"",
            ".section .tbss.tt,\"awT\",@nobits",
            ".section .tdata.ti,\"awT\",@progbits",
            ".section .mine,\"aw\"",
            ".section .data.fp,\"aw\"",
        ] {
            assert!(found.contains(&want), "{triple}: no {want}:\n{asm}");
        }
        // Without the options, nothing is in a section of its own.
        let asm = asm_for_with("shared_sections", triple, DEFINITIONS, &["-O0"]);
        assert!(!asm.contains(".text.f"), "{triple}:\n{asm}");
        assert!(!asm.contains(".data.i"), "{triple}:\n{asm}");
    }
    let asm = asm_for_with("own_sections_macho", AARCH64_DARWIN, DEFINITIONS, &flags);
    assert!(!asm.contains(".text.f"), "{asm}");
    assert!(!asm.contains(".data.i"), "{asm}");
}

/// `-fcommon` makes a tentative definition a common symbol, as gcc does,
/// beside an initialized object, a static one, a thread-local one and a
/// weak one, which stay definitions; `-fno-common`, the default, makes it an
/// ordinary definition. A common symbol may be merged with another unit's
/// definition, so even a PIE reaches it through the GOT.
#[test]
fn codegen_fcommon_makes_tentative_definitions_common() {
    let src = r#"
int t;
int arr[10];
int z = 0;
static int s;
__thread int tt;
__attribute__((weak)) int w;
__attribute__((visibility("hidden"))) int h;
int get(void) { return t + arr[1] + z + s + tt + w + h; }
"#;
    for triple in [X86_64_LINUX, AARCH64_LINUX] {
        let asm = asm_for_with("fcommon", triple, src, &["-O2", "-fcommon"]);
        let lines: Vec<&str> = asm.lines().map(str::trim).collect();
        for want in [".comm t,4,4", ".comm h,4,4", ".hidden h"] {
            assert!(lines.contains(&want), "{triple}: no {want}:\n{asm}");
        }
        assert!(
            lines.iter().any(|l| l.starts_with(".comm arr,40,")),
            "{triple}:\n{asm}"
        );
        for name in ["z", "tt", "w"] {
            assert!(
                !lines
                    .iter()
                    .any(|l| l.starts_with(&format!(".comm {name},"))),
                "{triple}: {name} made common:\n{asm}"
            );
        }
        let get = body_of(&asm, "get");
        let got = if triple == X86_64_LINUX {
            "t@GOTPCREL"
        } else {
            ":got:t"
        };
        assert!(get.contains(got), "{triple}:\n{get}");

        let asm = asm_for_with("fno_common", triple, src, &["-O2"]);
        assert!(!asm.contains(".comm t,"), "{triple}:\n{asm}");
    }
}

/// `-fno-plt` calls a function that may be defined elsewhere through its
/// GOT entry, as gcc does: x86-64 in every kind of code, aarch64 in
/// position-independent code. A function the output defines is still
/// called directly, as is everything without the option.
#[test]
fn codegen_fno_plt_calls_through_the_got() {
    let src = r#"
extern int ext(int);
int defd(int x) { return x * 3; }
int call(int x) { return ext(x) + defd(x); }
"#;
    let flags = |pic: &'static str| ["-O1", "-fno-inline", pic, "-fno-plt"];
    for pic in ["-fPIE", "-fno-pie"] {
        let asm = asm_for_with("noplt_x86", X86_64_LINUX, src, &flags(pic));
        let call = body_of(&asm, "call");
        assert!(call.contains("call *ext@GOTPCREL(%rip)"), "{pic}:\n{call}");
        assert!(!call.contains("defd@GOTPCREL"), "{pic}:\n{call}");
    }
    let asm = asm_for_with("noplt_x86_pic", X86_64_LINUX, src, &flags("-fPIC"));
    let call = body_of(&asm, "call");
    assert!(call.contains("call *defd@GOTPCREL(%rip)"), "{call}");

    let asm = asm_for_with("noplt_a64", AARCH64_LINUX, src, &flags("-fPIE"));
    let call = body_of(&asm, "call");
    assert!(
        call.contains(":got:ext") && call.contains("blr x16"),
        "{call}"
    );
    assert!(call.contains("bl defd"), "{call}");
    let asm = asm_for_with("noplt_a64_abs", AARCH64_LINUX, src, &flags("-fno-pie"));
    assert!(body_of(&asm, "call").contains("bl ext"), "{asm}");

    for triple in [X86_64_LINUX, AARCH64_LINUX] {
        let asm = asm_for_with("plt", triple, src, &["-O1", "-fno-inline", "-fPIE"]);
        let call = body_of(&asm, "call");
        assert!(!call.contains("ext@GOTPCREL"), "{triple}:\n{call}");
        assert!(!call.contains(":got:ext"), "{triple}:\n{call}");
    }
}
