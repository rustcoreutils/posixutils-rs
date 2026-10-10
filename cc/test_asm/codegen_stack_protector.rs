//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// `-fstack-protector` and its levels: which functions get a canary, as gcc
// chooses them, and the guard and failure sequences each target uses. That a
// smashed canary aborts the program is `tests/codegen/stack_protector.rs`.
//

use super::asm_probe::{
    asm_for_with, assert_body_contains, assert_body_lacks, body_of, count_in_body, AARCH64_DARWIN,
    AARCH64_LINUX, X86_64_LINUX,
};

const X86_64_DARWIN: &str = "x86_64-apple-darwin";

/// One function per rule of gcc's selection. What gcc 13 protects of these,
/// at -O0 and -O2 alike, is [`GCC_PROTECTS`].
const SELECTION: &str = r#"
extern void use(void *);
extern int g(int);
extern void *alloca(unsigned long);
struct sc8 { char c[8]; int x; };
struct si { int a[2]; };
struct plain { int a, b; };
int no_locals(int x) { return x + 1; }
int scalar_local(int x) { int y = g(x); return y; }
int addr_taken(int x) { int y = x; use(&y); return y; }
int char7(int i) { char b[7]; b[i] = 1; use(b); return b[0]; }
int char8(int i) { char b[8]; b[i] = 1; use(b); return b[0]; }
int uchar8(int i) { unsigned char b[8]; b[i] = 1; use(b); return b[0]; }
int schar8(int i) { signed char b[8]; b[i] = 1; use(b); return b[0]; }
int int_arr(int i) { int b[8]; b[i] = 1; use(b); return b[0]; }
int char2d(int i) { char b[2][8]; b[i][0] = 1; use(b); return b[0][0]; }
int struct_char8(int i) { struct sc8 s; s.c[i] = 1; use(&s); return s.c[0]; }
int struct_int_arr(int i) { struct si s; s.a[i] = 1; use(&s); return s.a[0]; }
int struct_plain(int i) { struct plain s; s.a = i; s.b = g(i); return s.a + s.b; }
int struct_plain_addr(int i) { struct plain s; s.a = i; use(&s); return s.a; }
int vla(int n) { char b[n]; use(b); return b[0]; }
int alloca_fn(int n) { char *b = alloca(n); use(b); return b[0]; }
int char_arr_noescape(int i) { char b[16]; b[i & 15] = 1; return b[3]; }
int static_char_arr(int i) { static char b[16]; b[i] = 1; return b[0]; }
__attribute__((stack_protect)) int explicit_attr(int x) { return x + 2; }
__attribute__((no_stack_protector)) int no_attr(int i) { char b[64]; b[i] = 1; use(b); return b[0]; }
int union_char(int i) { union { char c[8]; int x; } u; u.c[i] = 1; use(&u); return u.x; }
int param_addr(int x) { use(&x); return x; }
int short_arr(int i) { short b[8]; b[i] = 1; use(b); return b[0]; }
int char_str_init(int i) { char b[] = "hello world!"; use(b); return b[i]; }
int char4_x2(int i) { char a[4]; char b[4]; a[i] = 1; b[i] = 2; use(a); use(b); return a[0] + b[0]; }
"#;

/// Every function [`SELECTION`] defines.
const ALL_FUNCTIONS: &[&str] = &[
    "no_locals",
    "scalar_local",
    "addr_taken",
    "char7",
    "char8",
    "uchar8",
    "schar8",
    "int_arr",
    "char2d",
    "struct_char8",
    "struct_int_arr",
    "struct_plain",
    "struct_plain_addr",
    "vla",
    "alloca_fn",
    "char_arr_noescape",
    "static_char_arr",
    "explicit_attr",
    "no_attr",
    "union_char",
    "param_addr",
    "short_arr",
    "char_str_init",
    "char4_x2",
];

/// What gcc 13 protects of [`SELECTION`] under each option, read off
/// `gcc -S` at -O0 and at -O2 (`__stack_chk_fail` in the body).
const GCC_PROTECTS: &[(&str, &[&str])] = &[
    (
        "-fstack-protector",
        &[
            "char8",
            "uchar8",
            "schar8",
            "struct_char8",
            "vla",
            "alloca_fn",
            "char_arr_noescape",
            "explicit_attr",
            "union_char",
            "char_str_init",
        ],
    ),
    (
        "-fstack-protector-strong",
        &[
            "addr_taken",
            "char7",
            "char8",
            "uchar8",
            "schar8",
            "int_arr",
            "char2d",
            "struct_char8",
            "struct_int_arr",
            "struct_plain_addr",
            "vla",
            "alloca_fn",
            "char_arr_noescape",
            "explicit_attr",
            "union_char",
            "short_arr",
            "char_str_init",
            "char4_x2",
        ],
    ),
    ("-fstack-protector-explicit", &["explicit_attr"]),
];

/// The functions of [`SELECTION`] with a canary check, in its order.
fn protected(asm: &str) -> Vec<&'static str> {
    ALL_FUNCTIONS
        .iter()
        .copied()
        .filter(|f| body_of(asm, f).contains("__stack_chk_fail"))
        .collect()
}

/// Each level protects what gcc's does, on both targets and at -O0 and -O2.
#[test]
fn stack_protector_selection_matches_gcc() {
    for triple in [X86_64_LINUX, AARCH64_LINUX] {
        for opt in ["-O0", "-O2"] {
            for (flag, want) in GCC_PROTECTS {
                let asm = asm_for_with("ssp_select", triple, SELECTION, &[flag, opt]);
                let want: Vec<&str> = ALL_FUNCTIONS
                    .iter()
                    .copied()
                    .filter(|f| want.contains(f))
                    .collect();
                assert_eq!(protected(&asm), want, "{triple} {flag} {opt}");
            }
            // `-all` protects everything but what `no_stack_protector` exempts.
            let asm = asm_for_with(
                "ssp_select_all",
                triple,
                SELECTION,
                &["-fstack-protector-all", opt],
            );
            let want: Vec<&str> = ALL_FUNCTIONS
                .iter()
                .copied()
                .filter(|f| *f != "no_attr")
                .collect();
            assert_eq!(protected(&asm), want, "{triple} -all {opt}");
        }
    }
}

/// No option, `-fno-stack-protector` after any level, and `stack_protect`
/// with no level: no function has a canary, as in gcc.
#[test]
fn stack_protector_off() {
    for flags in [
        &[][..],
        &["-fstack-protector-all", "-fno-stack-protector"][..],
        &["-fstack-protector-strong", "-fno-stack-protector"][..],
    ] {
        let asm = asm_for_with("ssp_off", X86_64_LINUX, SELECTION, flags);
        assert!(
            !asm.contains("__stack_chk"),
            "{flags:?} left a canary:\n{asm}"
        );
    }
    // The last level named wins.
    let asm = asm_for_with(
        "ssp_last",
        X86_64_LINUX,
        SELECTION,
        &[
            "-fno-stack-protector",
            "-fstack-protector-all",
            "-fstack-protector",
        ],
    );
    assert!(!protected(&asm).contains(&"no_locals"), "-all was replaced");
    assert!(protected(&asm).contains(&"char8"));
}

const ONE_ARRAY: &str = r#"
extern void use(void *);
int f(int i) { char buf[16]; int x = i * 3; buf[i] = 1; use(buf); use(&x); return buf[0] + x; }
"#;

/// x86-64 Linux reads glibc's guard at `%fs:40`, as gcc does, and calls the
/// failure function through the PLT.
#[test]
fn stack_protector_x86_64_linux_sequence() {
    for opt in ["-O0", "-O2"] {
        let asm = asm_for_with(
            "ssp_x86",
            X86_64_LINUX,
            ONE_ARRAY,
            &["-fstack-protector", opt],
        );
        let why = "x86-64 Linux canary";
        assert_eq!(
            count_in_body(&asm, "f", "%fs:40"),
            2,
            "{why}: set and check\n{asm}"
        );
        assert_body_contains(&asm, "f", "call __stack_chk_fail@PLT", why);
        assert_body_lacks(&asm, "f", "__stack_chk_guard", why);
    }
}

/// Darwin's guard is the global `__stack_chk_guard`, through the GOT.
#[test]
fn stack_protector_x86_64_darwin_sequence() {
    let asm = asm_for_with(
        "ssp_x86_darwin",
        X86_64_DARWIN,
        ONE_ARRAY,
        &["-fstack-protector"],
    );
    let why = "x86-64 Darwin canary";
    assert_eq!(
        count_in_body(&asm, "f", "___stack_chk_guard@GOTPCREL(%rip)"),
        2,
        "{why}\n{asm}"
    );
    assert_body_contains(&asm, "f", "call ___stack_chk_fail", why);
    assert_body_lacks(&asm, "f", "%fs:", why);
}

/// aarch64 reads the global `__stack_chk_guard` through the GOT -- glibc's
/// lives in the dynamic loader, so a direct reference cannot reach it --
/// and Darwin spells the same thing with its own relocations.
#[test]
fn stack_protector_aarch64_sequences() {
    let asm = asm_for_with("ssp_a64", AARCH64_LINUX, ONE_ARRAY, &["-fstack-protector"]);
    let why = "aarch64 Linux canary";
    assert_eq!(
        count_in_body(&asm, "f", ":got:__stack_chk_guard"),
        2,
        "{why}\n{asm}"
    );
    assert_eq!(
        count_in_body(&asm, "f", ":got_lo12:__stack_chk_guard"),
        2,
        "{why}"
    );
    assert_body_contains(&asm, "f", "bl __stack_chk_fail", why);

    let asm = asm_for_with(
        "ssp_a64_darwin",
        AARCH64_DARWIN,
        ONE_ARRAY,
        &["-fstack-protector"],
    );
    let why = "aarch64 Darwin canary";
    assert_eq!(
        count_in_body(&asm, "f", "___stack_chk_guard@GOTPAGEOFF"),
        2,
        "{why}\n{asm}"
    );
    assert_body_contains(&asm, "f", "bl ___stack_chk_fail", why);
}

/// Every return is checked, each with its own call; a path that ends in a
/// noreturn call is not, as in gcc.
#[test]
fn stack_protector_checks_every_return() {
    let src = r#"
extern void use(void *);
extern _Noreturn void die(void);
int f(int i) {
    char buf[16];
    buf[i] = 1;
    use(buf);
    if (buf[0]) return 1;
    if (buf[1]) die();
    return 2;
}
_Noreturn void g(int i) { char buf[16]; buf[i] = 1; use(buf); die(); }
"#;
    for (triple, ret) in [(X86_64_LINUX, "    ret\n"), (AARCH64_LINUX, "    ret\n")] {
        for opt in ["-O0", "-O2"] {
            let asm = asm_for_with("ssp_rets", triple, src, &["-fstack-protector", opt]);
            let rets = count_in_body(&asm, "f", ret);
            assert!(rets >= 1, "{triple} {opt}: no return\n{asm}");
            assert_eq!(
                count_in_body(&asm, "f", "__stack_chk_fail"),
                rets,
                "{triple} {opt}: one check per return\n{asm}"
            );
            assert_body_lacks(&asm, "g", "__stack_chk_fail", "g never returns");
        }
    }
}

/// The canary is the highest object in an x86-64 frame, right under the
/// saved registers, so an array that overruns reaches it before anything the
/// epilogue reloads; and the arrays sit right under it, `char` ones first,
/// whatever order they were declared in, so an overrun crosses no scalar on
/// the way, as in gcc.
#[test]
fn stack_protector_x86_64_canary_tops_the_locals() {
    let src = r#"
extern void use(void *);
int f(int i) {
    int x = i * 3;
    int w[2] = { i, i };
    char buf[16];
    buf[i] = 1;
    use(&x); use(w); use(buf);
    return buf[0] + x + w[1];
}
"#;
    for opt in ["-O0", "-O2"] {
        let flags = ["-fstack-protector-all", "-fverbose-asm", opt];
        let asm = asm_for_with("ssp_top", X86_64_LINUX, src, &flags);
        let slot = canary_slot(&asm);
        // `-fverbose-asm` names the local each address belongs to.
        let at = |name: &str| {
            body_of(&asm, "f")
                .lines()
                .map(str::trim)
                .find(|l| l.contains(&format!("# {name}.")))
                .and_then(displacement)
                .unwrap_or_else(|| panic!("{opt}: no address of {name}:\n{asm}"))
        };
        assert_eq!(
            at("buf"),
            slot - 16,
            "{opt}: buf right under the canary\n{asm}"
        );
        assert_eq!(at("w"), slot - 24, "{opt}: then w\n{asm}");
        assert!(at("x") < at("w"), "{opt}: the scalar last\n{asm}");
    }
}

/// The `%rbp` displacement of the canary in `f`, after checking nothing the
/// body addresses lies above it.
fn canary_slot(asm: &str) -> i32 {
    let body = body_of(asm, "f");
    // Without any `-fverbose-asm` comment.
    let lines: Vec<&str> = body
        .lines()
        .map(|l| l.split(" #").next().unwrap_or(l).trim())
        .collect();
    let set = lines
        .iter()
        .position(|l| l.ends_with("%fs:40, %r11"))
        .unwrap_or_else(|| panic!("no guard load:\n{body}"));
    let slot = displacement(lines[set + 1]).unwrap_or_else(|| panic!("no store:\n{body}"));
    // Every object the body addresses; the epilogue's `leaq` that points
    // `%rsp` at the saved registers is not one.
    for l in lines.iter().filter(|l| !l.ends_with("%rsp")) {
        if let Some(d) = displacement(l) {
            assert!(
                d <= slot,
                "`{l}` is above the canary at {slot}(%rbp):\n{body}"
            );
        }
    }
    slot
}

/// The `%rbp` displacement a line addresses, if any.
fn displacement(line: &str) -> Option<i32> {
    let end = line.find("(%rbp)")?;
    let start = line[..end].rfind([' ', ','])? + 1;
    line[start..end].parse().ok()
}

/// Protection follows the function the code ends up in: an array inlined
/// into a caller protects the caller, and a caller that refuses protection
/// keeps refusing it with the array inlined, as gcc decides after inlining.
#[test]
fn stack_protector_follows_inlining() {
    let src = r#"
extern void use(void *);
static inline int callee(int i) { char b[16]; b[i] = 1; use(b); return b[0]; }
int caller(int i) { return callee(i) + 1; }
__attribute__((no_stack_protector)) int refuser(int i) { return callee(i) + 2; }
"#;
    let asm = asm_for_with(
        "ssp_inline",
        X86_64_LINUX,
        src,
        &["-fstack-protector", "-O2"],
    );
    assert_body_lacks(&asm, "caller", "call callee", "callee inlined");
    assert_body_contains(
        &asm,
        "caller",
        "__stack_chk_fail",
        "array inlined into caller",
    );
    assert_body_lacks(&asm, "refuser", "__stack_chk_fail", "no_stack_protector");
}

/// `optimize` naming a stack-protector option sets that function's level,
/// as gcc's does: glibc's `inhibit_stack_protector` on its ifunc resolvers
/// and its early startup code, `optimize("-fno-stack-protector")`. Written
/// with or without `-f`, in one string with commas or in several. Other
/// `optimize` options leave the level alone.
const OPTIMIZE_ATTR: &str = r#"
void use(char *);
__attribute__((optimize("-fno-stack-protector"))) void a(void) { char b[200]; use(b); }
__attribute__((__optimize__("no-stack-protector"))) void b(void) { char b[200]; use(b); }
__attribute__((optimize("-fstack-protector-all"))) void c(void) { int x; use((char*)&x); }
__attribute__((optimize("O2,stack-protector-strong"))) void d(void) { int x[2]; use((char*)x); }
__attribute__((optimize("-O2", "-fstack-protector-all"))) void e(void) { int x; use((char*)&x); }
__attribute__((optimize("-fno-stack-protector"), stack_protect)) void f(void) { char b[200]; use(b); }
__attribute__((optimize("-fstack-protector-explicit"))) void g(void) { char b[200]; use(b); }
__attribute__((optimize("-fstack-protector-explicit"), stack_protect)) void h(void) { int x; use((char*)&x); }
__attribute__((optimize("-O3", "-fno-omit-frame-pointer"))) void i(void) { char b[200]; use(b); }
void j(void) __attribute__((optimize("-fno-stack-protector")));
void j(void) { char b[200]; use(b); }
void plain(void) { char b[200]; use(b); }
void small(void) { int x; use((char*)&x); }
"#;

/// What gcc 13 protects of [`OPTIMIZE_ATTR`] under each level, read off
/// `gcc -O2 -S`.
#[test]
fn stack_protector_optimize_attribute_sets_the_level() {
    let funcs = [
        "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "plain", "small",
    ];
    for (level, want) in [
        ("-fno-stack-protector", &["c", "d", "e", "h"][..]),
        ("-fstack-protector", &["c", "d", "e", "h", "i", "plain"]),
        (
            "-fstack-protector-strong",
            &["c", "d", "e", "h", "i", "plain", "small"],
        ),
        (
            "-fstack-protector-all",
            &["c", "d", "e", "h", "i", "plain", "small"],
        ),
    ] {
        for triple in [X86_64_LINUX, AARCH64_LINUX] {
            for opt in ["-O0", "-O2"] {
                let asm = asm_for_with("ssp_optimize", triple, OPTIMIZE_ATTR, &[level, opt]);
                let got: Vec<&str> = funcs
                    .iter()
                    .copied()
                    .filter(|f| body_of(&asm, f).contains("__stack_chk_fail"))
                    .collect();
                assert_eq!(got, want, "{triple} {level} {opt}");
            }
        }
    }
}

/// Plain `-fstack-protector` guards a 200-byte `char` buffer that is passed
/// out, copied into or indexed, as gcc does, under the kernel's `-fno-pie`
/// at -O0 and -O2 on both targets. (gcc at -O0 also guards a buffer nothing
/// touches; c17 gives that one no stack slot at any level, so there is
/// nothing there to guard.)
#[test]
fn stack_protector_guards_a_used_char_buffer() {
    let src = r#"
void use(char *);
void out(void) { char buf[200]; use(buf); }
void copied(const char *s) { char buf[200]; __builtin_memcpy(buf, s, 200); use(buf); }
int indexed(int i) { char buf[200]; buf[i] = 1; return buf[3]; }
"#;
    for triple in [X86_64_LINUX, AARCH64_LINUX] {
        for opt in ["-O0", "-O2"] {
            let asm = asm_for_with(
                "ssp_char200",
                triple,
                src,
                &["-fstack-protector", "-fno-pie", opt],
            );
            for f in ["out", "copied", "indexed"] {
                assert_body_contains(&asm, f, "__stack_chk_fail", &format!("{triple} {opt}"));
            }
        }
    }
}

/// `--param ssp-buffer-size=N` is plain `-fstack-protector`'s threshold: a
/// `char` array of N bytes or more, alone or in an aggregate, as gcc 13
/// chooses at -O2 on both targets. `-strong` does not read it, and
/// `alloca` is protected whatever it is.
#[test]
fn stack_protector_ssp_buffer_size() {
    let src = r#"
void use(void *);
void c3(void) { char b[3]; use(b); }
void c4(void) { char b[4]; use(b); }
void c8(void) { char b[8]; use(b); }
void c200(void) { char b[200]; use(b); }
void c300(void) { char b[300]; use(b); }
struct s4 { char c[4]; int x; };
void s4(void) { struct s4 s; use(&s); }
void i100(void) { int b[100]; use(b); }
void al(int n) { use(__builtin_alloca(n)); }
"#;
    let funcs = ["c3", "c4", "c8", "c200", "c300", "s4", "i100", "al"];
    let strong = &["c3", "c4", "c8", "c200", "c300", "s4", "i100", "al"][..];
    for (param, plain) in [
        (None, &["c8", "c200", "c300", "al"][..]),
        (Some("4"), &["c4", "c8", "c200", "c300", "s4", "al"]),
        (Some("300"), &["c300", "al"]),
        (Some("1"), &["c3", "c4", "c8", "c200", "c300", "s4", "al"]),
    ] {
        for (level, want) in [
            ("-fstack-protector", plain),
            ("-fstack-protector-strong", strong),
        ] {
            for triple in [X86_64_LINUX, AARCH64_LINUX] {
                let p = param.map(|n| format!("--param=ssp-buffer-size={n}"));
                let mut flags = vec![level, "-O2"];
                flags.extend(p.as_deref());
                let asm = asm_for_with("ssp_buffer_size", triple, src, &flags);
                let got: Vec<&str> = funcs
                    .iter()
                    .copied()
                    .filter(|f| body_of(&asm, f).contains("__stack_chk_fail"))
                    .collect();
                let want: Vec<&str> = funcs.iter().copied().filter(|f| want.contains(f)).collect();
                assert_eq!(got, want, "{triple} {flags:?}");
            }
        }
    }
}
