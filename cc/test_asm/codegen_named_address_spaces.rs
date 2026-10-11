//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// gcc's x86-64 named address spaces, `__seg_fs` and `__seg_gs`: an access
// to an object in one goes through the segment, as `%fs:`/`%gs:` on the
// memory operand, whatever the addressing mode; taking its address does
// not. glibc's `THREAD_SELF` reads `%fs:0x10` so, and the Linux kernel's
// per-CPU variables are `%gs:` accesses when `CONFIG_USE_X86_SEG_SUPPORT`.
// The cases that run stay in `tests/codegen/named_address_spaces.rs`.
//

use super::asm_probe::{asm_for_with, body_of, X86_64_LINUX};

const SRC: &str = "\
int __seg_fs fs;
int __seg_gs gs;
extern int __seg_gs ext;
int rd(int *p) { return *(int __seg_gs *)(unsigned long)p; }
long rd2(long __seg_fs *p) { return p[3]; }
void wr(int __seg_gs *p, int v) { *p = v; }
int rdg(void) { return gs + ext; }
void wrg(int v) { gs = v; }
struct st { int a, b; };
int rds(struct st __seg_gs *s) { return s->b; }
char rdc(const char __seg_fs *s, long i) { return s[i]; }
unsigned short rdh(unsigned short __seg_gs *p) { return *p; }
double rdd(double __seg_gs *p) { return *p; }
void wrd(float __seg_fs *p, float v) { *p = v; }
int *addr(void) { return (int *)&gs; }
int *conv(int __seg_gs *p) { return (int *)p; }
void *self(void) { return *(void *__seg_fs *)0x10; }
";

/// Every load and store through a `__seg_fs`/`__seg_gs` lvalue carries the
/// segment; an address computed from one does not.
#[test]
fn codegen_named_address_space_accesses_use_the_segment() {
    for opt in ["-O0", "-O2"] {
        let asm = asm_for_with("named_as", X86_64_LINUX, SRC, &[opt, "-fno-pie"]);
        for (func, want) in [
            ("rd", "%gs:("),
            ("rd2", "%fs:"),
            ("wr", "%gs:("),
            ("rdg", "%gs:gs(%rip)"),
            ("rdg", "%gs:ext(%rip)"),
            ("wrg", "%gs:gs(%rip)"),
            ("rds", "%gs:"),
            ("rdc", "%fs:"),
            ("rdh", "%gs:("),
            ("rdd", "%gs:("),
            ("wrd", "%fs:("),
            ("self", "%fs:"),
        ] {
            let body = body_of(&asm, func);
            assert!(body.contains(want), "{opt} {func}: no {want}:\n{body}");
        }
        for func in ["addr", "conv"] {
            let body = body_of(&asm, func);
            assert!(
                !body.contains("%gs") && !body.contains("%fs"),
                "{opt} {func}: an address is no access:\n{body}"
            );
        }
        // The objects themselves are ordinary data.
        assert!(asm.contains("\ngs:\n") && asm.contains("\nfs:\n"), "{opt}");
    }
    let o2 = asm_for_with("named_as", X86_64_LINUX, SRC, &["-O2", "-fno-pie"]);
    assert!(body_of(&o2, "rds").contains("%gs:4("), "{o2}");
    assert!(body_of(&o2, "self").contains("%fs:16"), "{o2}");
}

/// Under `-fPIC` an external object in a segment is still reached by its
/// offset from the segment base: the GOT supplies the address, and only
/// the access through it is prefixed.
#[test]
fn codegen_named_address_space_through_the_got() {
    let asm = asm_for_with("named_as_pic", X86_64_LINUX, SRC, &["-O2", "-fPIC"]);
    let body = body_of(&asm, "rdg");
    assert!(body.contains("ext@GOTPCREL(%rip)"), "{body}");
    assert!(!body.contains("%gs:ext@GOTPCREL"), "{body}");
    assert!(body.contains("%gs:("), "{body}");
}

/// An access through the segment is never removed, merged or forwarded:
/// what the segment base points to is not memory the compiler can see.
#[test]
fn codegen_named_address_space_accesses_are_kept() {
    let src = "int rd2(int __seg_gs *p) { int a = *p; int b = *p; return a + b; }\n\
               void st2(int __seg_gs *p) { *p = 1; *p = 2; }\n\
               int fwd(int __seg_gs *p, int *q) { *q = 5; return *(int __seg_gs *)q; }\n";
    let asm = asm_for_with("named_as_kept", X86_64_LINUX, src, &["-O2"]);
    assert_eq!(body_of(&asm, "rd2").matches("%gs:").count(), 2, "{asm}");
    assert_eq!(body_of(&asm, "st2").matches("%gs:").count(), 2, "{asm}");
    assert!(body_of(&asm, "fwd").contains("%gs:("), "{asm}");
}

/// An asm memory operand in a segment is written with it: the Linux
/// kernel's per-CPU asm, under `CC_HAS_NAMED_AS`, puts no `%%gs:` in its
/// templates and leaves the prefix to the `"m"` operand
/// (`__percpu_arg(x)` is plain `%x`).
#[test]
fn codegen_named_address_space_asm_memory_operands() {
    let src = "unsigned long pcpu;\n\
               unsigned long rd(void) { unsigned long v;\n\
               asm(\"movq %1, %0\" : \"=r\"(v) : \"m\"(*(unsigned long __seg_gs *)(unsigned long)&pcpu));\n\
               return v; }\n\
               void add(unsigned long x) {\n\
               asm(\"addq %1, %0\" : \"+m\"(*(unsigned long __seg_gs *)(unsigned long)&pcpu) : \"r\"(x)); }\n\
               void wr(unsigned long __seg_fs *p, unsigned long x) {\n\
               asm(\"movq %1, %0\" : \"=m\"(*p) : \"r\"(x)); }\n\
               void plain(unsigned long *p, unsigned long x) {\n\
               asm(\"movq %1, %0\" : \"=m\"(*p) : \"r\"(x)); }\n";
    for opt in ["-O0", "-O2"] {
        let asm = asm_for_with("named_as_asm", X86_64_LINUX, src, &[opt, "-fno-pie"]);
        for (func, want) in [("rd", "%gs:"), ("add", "%gs:"), ("wr", "%fs:(")] {
            let body = body_of(&asm, func);
            assert_eq!(
                body.matches(want).count(),
                1,
                "{opt} {func}: {want}\n{body}"
            );
        }
        let body = body_of(&asm, "plain");
        assert!(
            !body.contains("%fs") && !body.contains("%gs"),
            "{opt}\n{body}"
        );
    }
    let asm = asm_for_with("named_as_asm", X86_64_LINUX, src, &["-O2", "-fno-pie"]);
    assert!(body_of(&asm, "rd").contains("%gs:pcpu(%rip)"), "{asm}");
    assert!(body_of(&asm, "add").contains(", %gs:pcpu(%rip)"), "{asm}");
}

/// Mach-O spells the same accesses with its symbol prefix.
#[test]
fn codegen_named_address_space_on_darwin() {
    let asm = asm_for_with("named_as_darwin", "x86_64-apple-darwin", SRC, &["-O2"]);
    assert!(body_of(&asm, "rdg").contains("%gs:_gs(%rip)"), "{asm}");
    assert!(body_of(&asm, "rd").contains("%gs:("), "{asm}");
}
