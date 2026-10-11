//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// The x86-64 options a kernel build passes, as the assembly shows them:
// the code model, the stack's alignment, code without SSE or the x87, the
// retpoline and return-thunk mitigations, patchable entries, zeroed
// registers and the stack guard. The cases that must assemble, link or run
// stay in `tests/codegen/kernel.rs`.
//

use super::asm_probe::{asm_for_with, body_of, X86_64_LINUX};

/// `-mcmodel=kernel` puts code and data in the top 2 GB, where an absolute
/// address fits only a sign-extended 32-bit immediate: gcc writes `movq
/// $sym, %rax` (`R_X86_64_32S`) where the small model's `movl $sym, %eax`
/// (`R_X86_64_32`) would zero-extend to an address below 4 GB. Loads and
/// stores stay RIP-relative, and nothing goes through the GOT.
#[test]
fn codegen_kernel_code_model_sign_extends_addresses() {
    let src = "int g[4];\nextern int ext[];\nstatic int s;\n\
               int *addr(void) { return &g[1]; }\n\
               void *fn_addr(void) { return (void *)addr; }\n\
               long ext_addr(void) { return (long)&ext[3]; }\n\
               int load(void) { return s + ext[2]; }\n";
    for opt in ["-O0", "-O2"] {
        let asm = asm_for_with(
            "kernel_model",
            X86_64_LINUX,
            src,
            &[opt, "-fno-pie", "-mcmodel=kernel"],
        );
        for (func, sym) in [("addr", "g"), ("fn_addr", "addr"), ("ext_addr", "ext")] {
            let body = body_of(&asm, func);
            assert!(
                body.contains(&format!("movq ${sym}, %r")),
                "{opt} {func}: expected a sign-extended immediate:\n{body}"
            );
            assert!(
                !body.contains(&format!("movl ${sym}")),
                "{opt} {func}: a zero-extended immediate cannot reach the top 2 GB:\n{body}"
            );
        }
        let body = body_of(&asm, "load");
        assert!(body.contains("s(%rip)"), "{opt}:\n{body}");
        assert!(!asm.contains("@GOT"), "{opt}:\n{asm}");
        // The small model keeps its zero-extended form.
        let small = asm_for_with("small_model", X86_64_LINUX, src, &[opt, "-fno-pie"]);
        assert!(body_of(&small, "addr").contains("movl $g, %e"), "{opt}");
        let explicit = asm_for_with(
            "small_model",
            X86_64_LINUX,
            src,
            &[opt, "-fno-pie", "-mcmodel=small"],
        );
        assert_eq!(body_of(&explicit, "addr"), body_of(&small, "addr"), "{opt}");
    }
}

/// The displacements `line` addresses relative to `base`, e.g. `(%rbp)`.
fn displacements(line: &str, base: &str) -> Vec<i64> {
    let mut found = Vec::new();
    let mut rest = line;
    while let Some(at) = rest.find(base) {
        let before = &rest[..at];
        let start = before
            .rfind(|c: char| !(c.is_ascii_digit() || c == '-'))
            .map_or(0, |i| i + 1);
        found.push(before[start..].parse::<i64>().unwrap_or(0));
        rest = &rest[at + base.len()..];
    }
    found
}

/// The deepest byte below `%rbp` a function body addresses, and the bytes
/// its prologue moved `%rsp` below `%rbp`: the pushes after `movq %rsp,
/// %rbp` and the `subq $N, %rsp`.
fn frame_reach(body: &str) -> (i64, i64) {
    let mut reserved = 0;
    let mut framed = false;
    let mut deepest = 0;
    for line in body.lines().map(str::trim) {
        if line == "movq %rsp, %rbp" {
            framed = true;
        } else if framed && line.starts_with("pushq ") {
            reserved += 8;
        }
        if let Some(n) = line
            .strip_prefix("subq $")
            .and_then(|r| r.strip_suffix(", %rsp"))
        {
            reserved += n.parse::<i64>().unwrap();
        }
        for off in displacements(line, "(%rbp)") {
            deepest = deepest.max(-off);
        }
    }
    (deepest, reserved)
}

/// `-mno-red-zone` is what c17 does already: a function reserves its frame
/// before it writes a slot, leaf or not, so nothing lives below `%rsp`
/// where an interrupt would overwrite it. Not in a leaf with locals, nor a
/// variadic function's register save area, nor around inline asm, nor in
/// a stack-protected frame -- and `-mred-zone`, which only permits the
/// zone, changes nothing.
#[test]
fn codegen_kernel_no_red_zone_below_rsp() {
    let src = r#"
#include <stdarg.h>
int leaf(int a, int b) { volatile int t[8]; for (int i = 0; i < 8; i++) t[i] = a + i * b; return t[3] + t[5]; }
long va(int n, ...) { va_list ap; va_start(ap, n); long s = 0; while (n--) s += va_arg(ap, long); va_end(ap); return s; }
unsigned long rd(void) { unsigned long v[4]; for (int i = 0; i < 4; i++) asm volatile("rdtsc; shl $32, %%rdx; or %%rdx, %%rax" : "=a"(v[i]) :: "rdx"); return v[0] ^ v[3]; }
struct big { long x[12]; };
long copy(struct big *p) { struct big b = *p; b.x[3]++; *p = b; return b.x[0]; }
void guarded(char *out) { char buf[64]; for (int i = 0; i < 64; i++) buf[i] = out[i]; out[0] = buf[63]; }
"#;
    for opt in ["-O0", "-O2"] {
        let flags = [opt, "-fno-pie", "-mno-red-zone", "-fstack-protector-all"];
        let asm = asm_for_with("no_red_zone", X86_64_LINUX, src, &flags);
        for func in ["leaf", "va", "rd", "copy", "guarded"] {
            let body = body_of(&asm, func);
            assert!(
                body.lines()
                    .all(|l| displacements(l, "(%rsp)").iter().all(|&d| d >= 0)),
                "{opt} {func}: a slot below %rsp:\n{body}"
            );
            let (deepest, reserved) = frame_reach(body);
            assert!(
                deepest <= reserved,
                "{opt} {func}: -{deepest}(%rbp) lies below the {reserved} bytes reserved:\n{body}"
            );
        }
        let red = asm_for_with(
            "no_red_zone",
            X86_64_LINUX,
            src,
            &[opt, "-fno-pie", "-mred-zone", "-fstack-protector-all"],
        );
        assert_eq!(red, asm, "{opt}: -mred-zone changed the code");
    }
}

/// The kernel's floating-point options: no SSE, MMX, 3DNow!, AVX or x87.
const NO_FP: &[&str] = &[
    "-mno-sse",
    "-mno-mmx",
    "-mno-sse2",
    "-mno-3dnow",
    "-mno-avx",
    "-mno-80387",
    "-mno-fp-ret-in-387",
];

/// Whether `asm` names a floating-point or vector register.
fn names_fp_register(asm: &str) -> bool {
    asm.lines().any(|l| {
        let l = l.trim();
        !l.starts_with('.') && (l.contains("%xmm") || l.contains("%st") || l.contains("%mm"))
    })
}

/// Integer code built without SSE or the x87 touches neither: a kernel
/// saves neither on entry. gcc's `-mno-sse` variadic prologue saves the six
/// general argument registers and no XMM register; `va_copy` and a block
/// copy move their eightbytes through a general register where c17 used
/// `%xmm15`; and `-mskip-rax-setup` leaves `%al` alone at a variadic call,
/// as gcc does once SSE is off (and only then).
#[test]
fn codegen_kernel_integer_code_without_fp_registers() {
    let src = r#"
#include <stdarg.h>
extern int vfmt(const char *f, va_list ap);
extern int pr(const char *f, ...);
int fmt(const char *f, ...) { va_list ap; va_start(ap, f); int r = vfmt(f, ap); va_end(ap); return r; }
long copy(const char *f, ...) { va_list ap, aq; va_start(ap, f); va_copy(aq, ap); long r = va_arg(aq, long); va_end(aq); va_end(ap); return r; }
struct big { long a[9]; };
extern void take(struct big);
void pass(struct big *p) { take(*p); }
void assign(struct big *a, struct big *b) { *a = *b; }
int call(void) { return pr("x", 1); }
"#;
    for opt in ["-O0", "-O2"] {
        let mut flags = vec![opt, "-fno-pie", "-mskip-rax-setup"];
        flags.extend_from_slice(NO_FP);
        let asm = asm_for_with("no_fp", X86_64_LINUX, src, &flags);
        assert!(!names_fp_register(&asm), "{opt}:\n{asm}");
        assert!(
            body_of(&asm, "fmt").contains("%r9"),
            "{opt}: no GP save area"
        );
        assert!(!body_of(&asm, "call").contains("%al"), "{opt}: %al set");
        // With SSE, the variadic call still counts its vector registers.
        let sse = asm_for_with(
            "no_fp",
            X86_64_LINUX,
            src,
            &[opt, "-fno-pie", "-mskip-rax-setup"],
        );
        assert!(body_of(&sse, "call").contains("%al"), "{opt}:\n{sse}");
        assert!(body_of(&sse, "fmt").contains("%xmm7"), "{opt}:\n{sse}");
    }
}

/// What a function without SSE or the x87 cannot have, refused in gcc's
/// words where gcc refuses it too -- a floating value returned or read
/// with `va_arg` from a register the ISA lacks -- and in c17's where gcc
/// would compute it on the x87 or in software, which c17 does not. What
/// needs no register at all -- an initializer, `sizeof`, an address, a
/// function the optimizer leaves unused -- is no error.
#[test]
fn codegen_kernel_fp_without_its_registers() {
    let cases: &[(&str, &str)] = &[
        (
            "double ret(double a) { return a; }",
            "error: SSE register return with SSE disabled",
        ),
        (
            "extern double get(void);\nint use(void) { return get() > 0; }",
            "error: SSE register return with SSE disabled",
        ),
        (
            "struct S { double d; long x; };\nstruct S mk(void) { struct S s = {0}; return s; }",
            "error: SSE register return with SSE disabled",
        ),
        (
            "long double ld(long double a) { return a * 2; }",
            "error: x87 register return with x87 disabled",
        ),
        (
            "#include <stdarg.h>\nint va(int n, ...) { va_list ap; va_start(ap, n); \
             double d = va_arg(ap, double); va_end(ap); return d > 0; }",
            "error: SSE register argument with SSE disabled",
        ),
        (
            "int conv(int x) { double d = x; d *= 2.5; return (int)d; }",
            "error: floating-point code with SSE disabled is not supported",
        ),
        (
            "extern void take(double);\nvoid call(void) { take(1.0); }",
            "error: floating-point code with SSE disabled is not supported",
        ),
    ];
    for (src, want) in cases {
        let r = crate::test_compile::compile(
            "no_fp",
            src,
            &[&["--target=x86_64-unknown-linux-gnu", "-O2"][..], NO_FP].concat(),
        );
        assert!(!r.success, "{src}: accepted");
        assert!(r.stderr.contains(want), "{src}:\n{}", r.stderr);
    }
    let fine = "float gf = 1.5f;\ndouble g;\ndouble *addr(void) { return &g; }\n\
                int sz(void) { return sizeof(double); }\n\
                int folded(void) { return (int)(2.5 * 4); }\n";
    // An unused static function is no code at -O2; at -O0 gcc compiles it,
    // and refuses it.
    let unused = "static double unused(double x) { return x * 2; }\n";
    for (opt, src) in [
        ("-O0", fine.to_string()),
        ("-O2", format!("{fine}{unused}")),
    ] {
        let mut flags = vec![opt];
        flags.extend_from_slice(NO_FP);
        let asm = asm_for_with("no_fp_fine", X86_64_LINUX, &src, &flags);
        assert!(!names_fp_register(&asm), "{opt}:\n{asm}");
    }
    // `-mno-80387` alone leaves SSE: `double` is fine, `long double` is not.
    let r = crate::test_compile::compile(
        "no_x87",
        "double f(double a) { return a * 2; }\nlong double g(long double a) { return a; }",
        &["--target=x86_64-unknown-linux-gnu", "-msoft-float"],
    );
    assert!(
        r.stderr
            .contains("2:1: error: x87 register return with x87 disabled")
            && !r.stderr.contains("1:1: error"),
        "{}",
        r.stderr
    );
}

/// `-falign-functions=N[:M[:N2[:M2]]]` writes gcc 13's `.p2align`s before
/// each function: `N` rounded up to a power of two, padded by at most
/// `M - 1` bytes, and on x86-64 an 8-byte second alignment after a limited
/// first unless `N2` gives one; 0 and the bare option are the target's
/// default (16 on x86-64, 16 padded by at most 11 on aarch64), and 1 or
/// `-fno-align-functions` none. A function with an `aligned` attribute
/// keeps its own alignment alone. Each row is gcc 13's output.
#[test]
fn codegen_kernel_function_alignment() {
    let src = "void f(void) {}\n__attribute__((aligned(64))) void g(void) {}\n";
    let rows: &[(&str, &[&str])] = &[
        ("-falign-functions", &[".p2align 4"]),
        ("-falign-functions=0", &[".p2align 4"]),
        ("-falign-functions=1", &[]),
        ("-fno-align-functions", &[]),
        ("-falign-functions=16", &[".p2align 4"]),
        ("-falign-functions=32", &[".p2align 5"]),
        ("-falign-functions=24", &[".p2align 5,,23", ".p2align 3"]),
        ("-falign-functions=16:15", &[".p2align 4,,14", ".p2align 3"]),
        ("-falign-functions=16:1", &[".p2align 4"]),
        (
            "-falign-functions=32:8:16",
            &[".p2align 5,,7", ".p2align 4"],
        ),
        (
            "-falign-functions=32:8:16:4",
            &[".p2align 5,,7", ".p2align 4,,3"],
        ),
        ("-falign-functions=8:3", &[".p2align 3,,2"]),
        ("-falign-functions=3", &[".p2align 2,,2"]),
    ];
    let before = |asm: &str, func: &str| -> Vec<String> {
        let lines: Vec<&str> = asm.lines().map(str::trim).collect();
        let at = lines.iter().position(|l| *l == format!("{func}:")).unwrap();
        let mut found: Vec<String> = lines[..at]
            .iter()
            .rev()
            .take_while(|l| !l.is_empty() && !l.starts_with(".type") && !l.starts_with(".globl"))
            .filter(|l| l.starts_with(".p2align"))
            .map(|l| l.to_string())
            .collect();
        found.reverse();
        found
    };
    for (flag, want) in rows {
        let asm = asm_for_with("falign", X86_64_LINUX, src, &["-O0", flag]);
        assert_eq!(before(&asm, "f"), *want, "{flag}:\n{asm}");
        assert_eq!(before(&asm, "g"), [".p2align 6"], "{flag}:\n{asm}");
    }
    let plain = asm_for_with("falign", X86_64_LINUX, src, &["-O0"]);
    assert!(before(&plain, "f").is_empty(), "{plain}");
    for (flag, want) in [
        ("-falign-functions", &[".p2align 2", ".p2align 4,,11"][..]),
        ("-falign-functions=24", &[".p2align 2", ".p2align 5,,23"]),
        ("-falign-functions=16:4", &[".p2align 2", ".p2align 4,,3"]),
    ] {
        let asm = asm_for_with(
            "falign",
            super::asm_probe::AARCH64_LINUX,
            src,
            &["-O0", flag],
        );
        assert_eq!(before(&asm, "f"), want, "aarch64 {flag}:\n{asm}");
    }
    // gcc 14's minimum alignment applies to every function, before the
    // `-falign-functions` padding; an `aligned` function above it keeps
    // its own.
    let asm = asm_for_with(
        "falign",
        X86_64_LINUX,
        src,
        &["-O0", "-fmin-function-alignment=32", "-falign-functions=16"],
    );
    assert_eq!(before(&asm, "f"), [".p2align 5", ".p2align 4"], "{asm}");
    assert_eq!(before(&asm, "g"), [".p2align 6"], "{asm}");
}

/// `-fno-delete-null-pointer-checks` asks for what c17 does: no pass
/// concludes a pointer is non-null from its being dereferenced, so the
/// test after `*p` stays.
#[test]
fn codegen_kernel_null_checks_survive_dereference() {
    let src = "int f(int *p) { int v = *p; if (!p) return -1; return v; }\n";
    for flags in [&["-O2"][..], &["-O2", "-fno-delete-null-pointer-checks"]] {
        let asm = asm_for_with("null_checks", X86_64_LINUX, src, flags);
        let body = body_of(&asm, "f");
        assert!(
            body.contains("$-1"),
            "{flags:?}: the null test is gone:\n{body}"
        );
    }
}

/// `-mpreferred-stack-boundary=3`: the stack is only 8-byte aligned at a
/// call, so a function with a 16-byte aligned local realigns its frame as
/// it would for a 32-byte one, as gcc does; one without keeps its plain
/// frame.
#[test]
fn codegen_kernel_eight_byte_stack_boundary() {
    let src = "extern void use(void *);\n\
               void f(void) { __attribute__((aligned(16))) char b[16]; use(b); }\n\
               void g(void) { long x; use(&x); }\n";
    for opt in ["-O0", "-O2"] {
        let asm = asm_for_with(
            "boundary",
            X86_64_LINUX,
            src,
            &[opt, "-mno-sse", "-mpreferred-stack-boundary=3"],
        );
        assert!(
            body_of(&asm, "f").contains("andq $-16, %rsp"),
            "{opt}:\n{asm}"
        );
        assert!(!body_of(&asm, "g").contains("andq"), "{opt}:\n{asm}");
        let normal = asm_for_with("boundary", X86_64_LINUX, src, &[opt]);
        assert!(!body_of(&normal, "f").contains("andq"), "{opt}:\n{normal}");
    }
}

/// `-ffreestanding` takes the library functions' builtins away, as gcc's
/// implies `-fno-builtin`: `strlen` of a constant is a call, not its
/// length. `-fhosted` after it gives them back.
#[test]
fn codegen_kernel_freestanding_calls_the_library() {
    let src = "extern unsigned long strlen(const char *);\n\
               unsigned long f(void) { return strlen(\"four\"); }\n";
    for (flags, calls) in [
        (&["-O2"][..], false),
        (&["-O2", "-ffreestanding"], true),
        (&["-O2", "-ffreestanding", "-fhosted"], false),
    ] {
        let asm = asm_for_with("freestanding", X86_64_LINUX, src, flags);
        assert_eq!(
            body_of(&asm, "f").contains("call strlen"),
            calls,
            "{flags:?}:\n{asm}"
        );
    }
}

/// The Spectre v2 and straight-line-speculation mitigations, as gcc 13
/// writes them: `-mindirect-branch=thunk-extern` calls and jumps through
/// `__x86_indirect_thunk_<reg>` (with `-mindirect-branch-cs-prefix`, a `cs`
/// prefix on its own line for `%r8`..`%r15`), `-mfunction-return=
/// thunk-extern` returns through `__x86_return_thunk`, `thunk` defines the
/// thunks it uses in comdat sections of their own, `thunk-inline` writes
/// the retpoline at the branch, and `-mharden-sls=all` follows each `ret`
/// and `jmp *%reg` with an `int3`.
#[test]
fn codegen_kernel_retpolines_and_sls() {
    let src = "extern void (*fp)(void);\n\
               int call(int (*f)(int), int x) { fp(); return f(x) + 1; }\n";
    let asm_of = |flags: &[&str]| {
        let mut all = vec!["-O2", "-fno-pie"];
        all.extend_from_slice(flags);
        asm_for_with("retpoline", X86_64_LINUX, src, &all)
    };
    let names_indirect = |body: &str| {
        body.lines()
            .any(|l| l.contains("call *") || l.contains("jmp *"))
    };

    let plain = asm_of(&[]);
    assert!(names_indirect(body_of(&plain, "call")), "{plain}");

    let ext = asm_of(&[
        "-mindirect-branch=thunk-extern",
        "-mfunction-return=thunk-extern",
    ]);
    let body = body_of(&ext, "call");
    assert!(!names_indirect(body), "{body}");
    assert!(body.contains("call __x86_indirect_thunk_"), "{body}");
    assert!(
        body.contains("jmp __x86_return_thunk") && !body.contains("ret\n"),
        "{body}"
    );
    assert!(
        !ext.contains(".section .text.__x86_"),
        "thunk-extern defines nothing:\n{ext}"
    );

    let cs = asm_of(&[
        "-mindirect-branch=thunk-extern",
        "-mindirect-branch-cs-prefix",
    ]);
    let lines: Vec<&str> = body_of(&cs, "call").lines().map(str::trim).collect();
    for (i, line) in lines.iter().enumerate() {
        if let Some(reg) = line.strip_prefix("call __x86_indirect_thunk_") {
            let high = reg.starts_with("r") && reg[1..].starts_with(|c: char| c.is_ascii_digit());
            assert_eq!(lines[i - 1] == "cs", high, "{cs}");
        }
    }

    let own = asm_of(&["-mindirect-branch=thunk", "-mfunction-return=thunk"]);
    assert!(own.contains("jmp __x86_return_thunk"), "{own}");
    for name in ["__x86_return_thunk", "__x86_indirect_thunk_"] {
        assert!(
            own.contains(&format!(".section .text.{name}")) && own.contains(",comdat"),
            "{name}:\n{own}"
        );
    }
    assert!(own.contains("pause") && own.contains("lfence"), "{own}");

    let inline = asm_of(&[
        "-mindirect-branch=thunk-inline",
        "-mfunction-return=thunk-inline",
    ]);
    let body = body_of(&inline, "call");
    assert!(!names_indirect(body) && !body.contains("__x86_"), "{body}");
    assert!(
        body.contains("lfence") && body.contains("lea 8(%rsp), %rsp"),
        "{body}"
    );

    let sls = asm_of(&["-mharden-sls=all"]);
    let lines: Vec<&str> = body_of(&sls, "call").lines().map(str::trim).collect();
    let ret = lines.iter().position(|l| *l == "ret").unwrap();
    assert_eq!(lines[ret + 1], "int3", "{sls}");
    let none = asm_of(&["-mharden-sls=all", "-mharden-sls=none"]);
    assert!(!none.contains("int3"), "{none}");
}

/// `-mharden-sls=indirect-jmp` stops speculation past every indirect jump,
/// the ones made through a retpoline thunk too: gcc 13 follows `jmp
/// __x86_indirect_thunk_<reg>` with an `int3`, as it does `jmp *%reg`.
/// The return thunk's jump, a return, takes none.
#[test]
fn codegen_kernel_sls_after_thunk_jumps() {
    let src = "void h(int i) {\n\
               static void *t[] = { &&a, &&b };\n\
               goto *t[i];\n\
               a: __asm__ volatile(\"nop\");\n\
               b: return;\n\
               }\n";
    for flags in [
        &["-mharden-sls=all", "-mindirect-branch=thunk-extern"][..],
        &["-mharden-sls=indirect-jmp", "-mindirect-branch=thunk"],
        &[
            "-mharden-sls=all",
            "-mindirect-branch=thunk-extern",
            "-mindirect-branch-cs-prefix",
        ],
    ] {
        let mut all = vec!["-O2", "-fno-pie", "-mfunction-return=thunk-extern"];
        all.extend_from_slice(flags);
        let asm = asm_for_with("sls_thunk", X86_64_LINUX, src, &all);
        let lines: Vec<&str> = body_of(&asm, "h").lines().map(str::trim).collect();
        let jmp = lines
            .iter()
            .position(|l| l.starts_with("jmp __x86_indirect_thunk_"))
            .unwrap_or_else(|| panic!("{flags:?}: no thunk jump:\n{asm}"));
        assert_eq!(lines[jmp + 1], "int3", "{flags:?}:\n{asm}");
        let ret = lines
            .iter()
            .position(|l| *l == "jmp __x86_return_thunk")
            .unwrap_or_else(|| panic!("{flags:?}: no return thunk:\n{asm}"));
        assert_ne!(lines.get(ret + 1), Some(&"int3"), "{flags:?}:\n{asm}");
    }
}

/// `-fpatchable-function-entry=N,M` puts `M` nops before the function's
/// label and `N - M` after it -- after `endbr64` -- and records where they
/// start in `__patchable_function_entries`, a section linked to the
/// function's, as gcc 13 does; with `M` 0 the record follows the label.
#[test]
fn codegen_kernel_patchable_function_entry() {
    let src = "int f(int x) { return x + 1; }\n";
    let lines_of = |flags: &[&str]| -> Vec<String> {
        let mut all = vec!["-O2", "-fno-pie"];
        all.extend_from_slice(flags);
        let asm = asm_for_with("patchable", X86_64_LINUX, src, &all);
        asm.lines()
            .map(|l| l.trim().replace('\t', " "))
            .filter(|l| !l.starts_with(".cfi") && !l.starts_with(".loc"))
            .collect()
    };
    let nops_around = |lines: &[String]| -> (usize, usize) {
        let at = lines.iter().position(|l| l == "f:").unwrap();
        let before = lines[..at].iter().rev().take_while(|l| *l == "nop").count();
        let after = lines[at + 1..]
            .iter()
            .skip_while(|l| {
                *l == "endbr64"
                    || l.starts_with(".section")
                    || l.starts_with(".p2align")
                    || l.starts_with(".quad")
                    || *l == ".text"
                    || l.starts_with(".LPFE")
            })
            .take_while(|l| *l == "nop")
            .count();
        (before, after)
    };
    let lines = lines_of(&["-fpatchable-function-entry=16,16"]);
    assert_eq!(nops_around(&lines), (16, 0), "{lines:#?}");
    let record = lines
        .iter()
        .position(|l| {
            l.starts_with(".section __patchable_function_entries,\"awo\",@progbits,.LPFE")
        })
        .expect("the record");
    assert!(lines[record + 2].starts_with(".quad .LPFE"), "{lines:#?}");
    let lines = lines_of(&["-fpatchable-function-entry=4,2", "-fcf-protection=branch"]);
    assert_eq!(nops_around(&lines), (2, 2), "{lines:#?}");
    let at = lines.iter().position(|l| l == "f:").unwrap();
    assert_eq!(lines[at + 1], "endbr64", "{lines:#?}");
    let lines = lines_of(&["-fpatchable-function-entry=3"]);
    assert_eq!(nops_around(&lines), (0, 3), "{lines:#?}");
    let at = lines.iter().position(|l| l == "f:").unwrap();
    assert!(
        lines[at + 1].starts_with(".section __patchable"),
        "{lines:#?}"
    );
    assert!(!lines_of(&["-fpatchable-function-entry=0"])
        .iter()
        .any(|l| l.contains("__patchable")));
}

/// `-fzero-call-used-regs=` clears call-clobbered registers before each
/// return, as gcc 13 chooses them: `used-gpr` the general registers the
/// function uses, `all-gpr` every one, `all` the x87 stack and every SSE
/// register too, `-arg` only the argument registers; never the one the
/// value is returned in.
#[test]
fn codegen_kernel_zero_call_used_regs() {
    let src = "long leaf(long a, long b, long c) { return a * b + c; }\n\
               void v(void) {}\n\
               double d(double x) { return x * 2; }\n";
    let cleared = |flags: &str, func: &str| -> Vec<String> {
        let asm = asm_for_with("zero_regs", X86_64_LINUX, src, &["-O2", "-fno-pie", flags]);
        let body = body_of(&asm, func);
        let lines: Vec<&str> = body.lines().map(str::trim).collect();
        let ret = lines.iter().rposition(|l| *l == "ret").unwrap();
        let mut regs: Vec<String> = lines[..ret]
            .iter()
            .rev()
            .take_while(|l| {
                l.starts_with("xorl")
                    || l.starts_with("pxor")
                    || l.starts_with("fldz")
                    || l.starts_with("fstp")
            })
            .map(|l| {
                l.split_whitespace()
                    .last()
                    .unwrap()
                    .trim_start_matches('%')
                    .to_string()
            })
            .collect();
        regs.reverse();
        regs.dedup();
        regs
    };
    let all_gpr = ["edx", "ecx", "esi", "edi", "r8d", "r9d", "r10d", "r11d"];
    assert_eq!(cleared("-fzero-call-used-regs=all-gpr", "leaf"), all_gpr);
    let mut void_gpr = vec!["eax"];
    void_gpr.extend(all_gpr);
    assert_eq!(cleared("-fzero-call-used-regs=all-gpr", "v"), void_gpr);
    assert_eq!(
        cleared("-fzero-call-used-regs=all-gpr-arg", "v"),
        ["eax", "edx", "ecx", "esi", "edi", "r8d", "r9d"]
    );
    // `used-gpr`: what the leaf touches, its arguments among them.
    let used = cleared("-fzero-call-used-regs=used-gpr", "leaf");
    assert!(
        used.contains(&"esi".to_string()) && used.contains(&"edi".to_string()),
        "{used:?}"
    );
    assert!(!used.contains(&"eax".to_string()), "{used:?}");
    assert!(cleared("-fzero-call-used-regs=used-gpr", "v").is_empty());
    // `all`: the x87 stack first, then every register but %xmm0, which
    // holds the result.
    let all = cleared("-fzero-call-used-regs=all", "d");
    assert_eq!(all.first().map(String::as_str), Some("fldz"), "{all:?}");
    assert!(
        all.contains(&"xmm15".to_string()) && !all.contains(&"xmm0".to_string()),
        "{all:?}"
    );
    assert!(cleared("-fzero-call-used-regs=skip", "leaf").is_empty());
}

/// Where the stack protector reads its guard, as gcc 13 reads it: glibc's
/// `%fs:40` by default; under `-mcmodel=kernel` the kernel's per-CPU
/// `%gs:40` (`scripts/gcc-x86_64-has-stack-protector.sh` greps for `%gs`);
/// `-mstack-protector-guard-reg=` and `-offset=` move it, `-symbol=` reads
/// `%seg:sym(%rip)` (through the GOT under `-fpic`), and `=global` the
/// global `__stack_chk_guard`.
#[test]
fn codegen_kernel_stack_guard_placement() {
    let src = "void g(char *);\nvoid f(void) { char b[64]; g(b); }\n";
    let guard = |flags: &[&str]| -> String {
        let mut all = vec!["-O2", "-fstack-protector-strong"];
        all.extend_from_slice(flags);
        let asm = asm_for_with("guard", X86_64_LINUX, src, &all);
        body_of(&asm, "f")
            .lines()
            .map(str::trim)
            .find(|l| l.starts_with("cmpq"))
            .unwrap_or_else(|| panic!("{flags:?}: no check:\n{asm}"))
            .to_string()
    };
    assert_eq!(guard(&["-fno-pie"]), "cmpq %fs:40, %r11");
    assert_eq!(guard(&["-fno-pie", "-mcmodel=kernel"]), "cmpq %gs:40, %r11");
    assert_eq!(
        guard(&["-mstack-protector-guard-reg=gs"]),
        "cmpq %gs:40, %r11"
    );
    assert_eq!(
        guard(&[
            "-fno-pie",
            "-mcmodel=kernel",
            "-mstack-protector-guard-offset=1234"
        ]),
        "cmpq %gs:1234, %r11"
    );
    assert_eq!(
        guard(&[
            "-fno-pie",
            "-mstack-protector-guard-reg=gs",
            "-mstack-protector-guard-symbol=__stack_chk_guard"
        ]),
        "cmpq %gs:__stack_chk_guard(%rip), %r11"
    );
    assert_eq!(
        guard(&["-fno-pie", "-mstack-protector-guard=global"]),
        "cmpq __stack_chk_guard(%rip), %r11"
    );
    assert_eq!(
        guard(&[
            "-fno-pie",
            "-mstack-protector-guard=global",
            "-mcmodel=kernel"
        ]),
        "cmpq __stack_chk_guard(%rip), %r11"
    );
    assert_eq!(
        guard(&["-fpic", "-mstack-protector-guard-symbol=foo"]),
        "cmpq %fs:(%r10), %r11"
    );
    // `-fstack-protector` at -O0 guards a large character array nothing
    // uses, as gcc's unoptimized frame holds it: the kernel's own probe.
    let probe = asm_for_with(
        "guard_probe",
        X86_64_LINUX,
        "int foo(void) { char X[200]; return 3; }\n",
        &["-O0", "-mcmodel=kernel", "-fno-pie", "-fstack-protector"],
    );
    assert!(probe.contains("%gs"), "{probe}");
}

/// `indirect_branch(...)` and `function_return(...)` are one function's own
/// `-mindirect-branch=` and `-mfunction-return=`, as the kernel's
/// `__noinitretpoline` uses them: a function asking `keep` calls and
/// returns directly in a retpoline build, and one asking `thunk` alone in
/// a plain build defines the thunks it uses. A value gcc does not know is
/// dropped with its warning.
#[test]
fn codegen_kernel_branch_attributes() {
    let src = "extern void (*fp)(void);\n\
               __attribute__((indirect_branch(\"keep\"), function_return(\"keep\")))\n\
               void kept(void) { fp(); }\n\
               __attribute__((__indirect_branch__(\"thunk\"), __function_return__(\"thunk\")))\n\
               void own(void) { fp(); }\n\
               void plain(void) { fp(); }\n";
    let asm = asm_for_with(
        "branch_attrs",
        X86_64_LINUX,
        src,
        &[
            "-O2",
            "-fno-pie",
            "-mindirect-branch=thunk-extern",
            "-mfunction-return=thunk-extern",
        ],
    );
    let kept = body_of(&asm, "kept");
    assert!(kept.contains("call *%") && kept.contains("ret"), "{kept}");
    let plain = body_of(&asm, "plain");
    assert!(
        plain.contains("call __x86_indirect_thunk_") && plain.contains("jmp __x86_return_thunk"),
        "{plain}"
    );
    assert!(
        asm.contains(".section .text.__x86_indirect_thunk_"),
        "{asm}"
    );
    assert!(asm.contains(".section .text.__x86_return_thunk"), "{asm}");
    let plain_build = asm_for_with("branch_attrs", X86_64_LINUX, src, &["-O2", "-fno-pie"]);
    assert!(
        body_of(&plain_build, "own").contains("call __x86_indirect_thunk_"),
        "{plain_build}"
    );
    assert!(
        body_of(&plain_build, "plain").contains("call *%"),
        "{plain_build}"
    );

    let r = crate::test_compile::compile(
        "branch_attr_bad",
        "__attribute__((indirect_branch(\"bogus\"))) void f(void) {}\n\
         __attribute__((function_return(1))) void g(void) {}\n",
        &["--target=x86_64-unknown-linux-gnu"],
    );
    assert!(r.success, "{}", r.stderr);
    assert!(
        r.stderr.contains(
            "warning: argument to 'indirect_branch' attribute is not \
             (keep|thunk|thunk-inline|thunk-extern)"
        ) && r
            .stderr
            .contains("warning: 'function_return' attribute requires a string constant argument"),
        "{}",
        r.stderr
    );
}
