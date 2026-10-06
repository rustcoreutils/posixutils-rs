//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Symbols and storage, as the assembly shows them: linkage, sections,
// thread-local access models, labels and debug directives. The cases that
// must assemble, link or run stay in `tests/codegen/symbols.rs`.
//

use super::asm_probe::{
    asm_for_at, asm_for_with, body_of, AARCH64_DARWIN, AARCH64_LINUX, X86_64_LINUX,
};
#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
use super::asm_probe::{host_asm, section_of};

/// Test that non-PIE code uses direct RIP-relative addressing for globals.
/// With -fno-pie, local globals should use foo(%rip) not GOT.
#[cfg(target_arch = "x86_64")]
#[test]
fn codegen_global_accesses_use_rip_relative() {
    let asm = asm_for_at(
        "global_access_rip",
        r#"
int foo = 3;

int main(void) {
    return foo;
}
"#,
        &["-fno-pie"],
    );
    assert!(
        asm.contains("foo(%rip)"),
        "expected RIP-relative access in asm output"
    );
    assert!(
        !asm.contains("@GOTPCREL"),
        "unexpected GOTPCREL usage for local globals"
    );
}

#[test]
fn codegen_debug_file_loc() {
    // With -g
    let asm = asm_for_at(
        "file_loc_test",
        r#"
int main() {
    return 42;
}
"#,
        &["-g"],
    );
    assert!(asm.contains(".file 1"), "Missing .file directive with -g");
    assert!(asm.contains(".loc 1"), "Missing .loc directive with -g");
    assert!(asm.contains(".cfi_def_cfa"), "Missing .cfi_def_cfa with -g");
}

#[test]
fn codegen_no_debug_no_loc() {
    let asm = asm_for_at(
        "no_debug_test",
        r#"
int main() {
    return 42;
}
"#,
        &[],
    );
    // .file is emitted unconditionally
    assert!(
        asm.contains(".file 1"),
        "Missing .file directive (should be unconditional)"
    );
    // .loc should NOT be present without -g
    assert!(
        !asm.contains(".loc 1"),
        "Unexpected .loc directive without -g"
    );
}

/// Taking the address of a thread-local must not go through the GOT.
///
/// The behavioural test (`tests/codegen/symbols.rs`) can only prove the
/// address works on this host;
/// this pins the actual access sequence, which is where the defect was.
#[test]
fn codegen_thread_local_address_uses_the_tls_sequence() {
    let src = "_Thread_local int tv = 11;\nint *addr(void) { return &tv; }\n";
    let asm = asm_for_with("tls_addr_seq", X86_64_LINUX, src, &["-O2"]);
    let body = body_of(&asm, "addr");
    assert!(
        !body.contains("tv@GOTPCREL"),
        "a thread-local address must not come from the GOT:\n{body}"
    );
    assert!(
        body.contains("@tpoff") || body.contains("%fs:") || body.contains("@gottpoff"),
        "expected a TLS access sequence:\n{body}"
    );
}

/// A single declaration without `inline` demands an external definition.
///
/// C99 6.7.4p6 makes a definition an inline definition only if **all** the
/// file-scope declarations include `inline`, so one bare declaration is enough
/// to require the out-of-line copy. Missing this half of the rule left
/// CPython's `_decimal` with `undefined symbol: mpd_set_positive`: libmpdec
/// declares `void mpd_set_positive(mpd_t *);` in its header and defines it
/// `inline __attribute__((always_inline))` in the .c file.
///
/// The bare declaration is tested on both sides of the definition, since
/// neither position is decidable when the definition itself is parsed.
#[test]
fn codegen_a_bare_declaration_forces_an_external_definition() {
    let src = r#"
void before(int *r);
inline __attribute__((always_inline)) void before(int *r) { *r = 1; }

inline void after(int *r) { *r = 2; }
void after(int *r);

int use(int *a, int *b) { before(a); after(b); return 0; }
"#;

    for triple in [X86_64_LINUX, AARCH64_LINUX] {
        let asm = asm_for_with("bare_decl_inline", triple, src, &["-O"]);
        assert!(
            asm.contains(".globl before"),
            "{triple}: a bare declaration before the definition forces emission:\n{asm}"
        );
        assert!(
            asm.contains(".globl after"),
            "{triple}: a bare declaration after the definition forces emission:\n{asm}"
        );
    }
}

/// A thread-local store costs no more register traffic than a global one.
///
/// A TLS descriptor resolver preserves every register but the one it returns
/// through, so nothing live needs saving around it. The comparison is against
/// the identical function storing to an ordinary global, because c17 stores
/// floating-point arguments to the stack in the prologue regardless, and a
/// count taken in isolation cannot tell that baseline from a real spill.
///
/// Scope, honestly: this pins the *cost* of the sequence, and would catch a
/// regression that started saving registers around it. It is not a proof that
/// the sequence is excluded from the allocator's call-like set -- at this
/// optimization level the floating-point values are already stack-resident,
/// so flipping that switch does not change this function's output. The reason
/// to keep the sequence out of that set is recorded where the decision is
/// made, in `opcode_constraints`.
#[test]
fn codegen_tls_descriptor_costs_no_more_than_a_global_store() {
    let src = r#"
_Thread_local int tv;
int gv;
double keep_tls(double a, double b) { double s = a * b; tv = 1; return s + a; }
double keep_global(double a, double b) { double s = a * b; gv = 1; return s + a; }
"#;
    let asm = asm_for_with("tls_nospill", X86_64_LINUX, src, &["-O", "-fPIC"]);
    let tls = body_of(&asm, "keep_tls");
    let glob = body_of(&asm, "keep_global");

    assert!(
        tls.contains("@TLSCALL"),
        "expected the descriptor sequence:\n{tls}"
    );

    // Compared against the identical function storing to an ordinary global,
    // because c17 stores floating-point arguments to the stack in the prologue
    // regardless -- counting spills in isolation cannot tell that baseline
    // apart from a spill the sequence caused.
    let tls_fp = tls.matches("movsd").count();
    let glob_fp = glob.matches("movsd").count();
    assert_eq!(
        tls_fp, glob_fp,
        "the descriptor sequence moved {tls_fp} floating-point values where an \
         ordinary global store moves {glob_fp}; something is saving registers \
         around it:\n--- thread-local ---\n{tls}\n--- global ---\n{glob}"
    );
}

/// An executable keeps the cheapest model, `-fPIE` included.
///
/// This is the control for the test above, and it is the one that constrains
/// the fix: switching everything position-independent to Initial Exec would
/// satisfy that test and break this one, because PIE is position independent
/// yet still knows its own thread-locals' offsets at link time.
#[test]
fn codegen_local_exec_survives_for_a_plain_executable() {
    let src = r#"
_Thread_local int tv;
int read_tls(int x) { return x + tv; }
int *addr_tls(void) { return &tv; }
"#;
    for flags in [&["-O"][..], &["-O", "-fPIE"][..]] {
        let asm = asm_for_with("tls_le", X86_64_LINUX, src, flags);
        assert!(
            asm.contains("@TPOFF") && !asm.contains("@GOTTPOFF"),
            "x86_64 {flags:?}: an executable should use Local Exec:\n{asm}"
        );

        let asm = asm_for_with("tls_le", AARCH64_LINUX, src, flags);
        assert!(
            asm.contains(":tprel_hi12:") && !asm.contains("gottprel"),
            "aarch64 {flags:?}: an executable should use Local Exec:\n{asm}"
        );
    }
}

/// The address of a thread-local is a pointer, and one per block is enough.
///
/// Under the dynamic model the address is computed by a call, so the
/// computation is made explicit in the IR for the register allocator to see.
/// It was typed with the *accessed* type, which is what the instruction
/// carried -- so the address of a `__thread double` was a double as far as
/// the allocator was concerned, and was given an SSE register to live in.
/// And every reference produced its own computation, which under this model
/// is one resolver call per reference rather than per value.
#[test]
fn codegen_dynamic_tls_address_is_a_pointer_computed_once() {
    let src = r#"
__thread double dv;
__thread int iv;

double read_double(void) { return dv + dv * 2.0; }
int read_int(void) { return iv + iv; }
"#;
    let asm = asm_for_with("tls_addr_type", X86_64_LINUX, src, &["-O", "-fPIC"]);

    let body = body_of(&asm, "read_double");
    let calls = body.matches("@TLSCALL").count();
    assert_eq!(
        calls, 1,
        "two references to one thread-local should resolve it once, not {calls} times:\n{body}"
    );

    let body = body_of(&asm, "read_int");
    let calls = body.matches("@TLSCALL").count();
    assert_eq!(
        calls, 1,
        "two references to one thread-local should resolve it once, not {calls} times:\n{body}"
    );
}

/// ELF section flags were chosen from code-versus-data alone, so read-only
/// data asked for a named section came out `"aw"` and lost its page
/// protection. gcc emits `"a"`.
#[test]
#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
fn codegen_named_section_flags_follow_constness() {
    let asm = host_asm(
        "c17_section_flags_",
        r#"
__attribute__((section(".rodata.mine"))) const int ro = 5;
__attribute__((section(".data.mine"))) int rw = 6;
__attribute__((section(".text.mine"))) int code(void) { return 7; }
int main(void) { return ro + rw + code() - 18; }
"#,
    );
    assert!(
        asm.contains(".section .rodata.mine,\"a\"\n"),
        "const data in a named section must not be writable:\n{asm}"
    );
    assert!(
        asm.contains(".section .data.mine,\"aw\""),
        "mutable data in a named section must be writable:\n{asm}"
    );
    assert!(
        asm.contains(".section .text.mine,\"ax\""),
        "a function's named section must be executable:\n{asm}"
    );
}

/// An unreferenced static goes; one marked `used` stays.
///
/// Two defects met here. The prune ran only when something had *also* been
/// inlined, so a translation unit the inliner declined kept its dead statics
/// at every level. And `__attribute__((used))` was parsed, stored, and read by
/// nothing -- invisible until the prune started working.
#[test]
#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
fn codegen_used_static_survives_and_unused_does_not() {
    let asm = asm_for_at(
        "c17_used_static_",
        r#"
__attribute__((used)) static int kept(void) { return 7; }
static int dropped(void) { return 8; }
int main(void) { return 0; }
"#,
        &["-O2"],
    );
    assert!(asm.contains("\nkept:"), "`used` must keep it:\n{asm}");
    assert!(
        !asm.contains("\ndropped:"),
        "an unreferenced static must go:\n{asm}"
    );
}

/// An initialized global is a *definition*, not a common symbol.
///
/// `.comm` declares a common symbol, and those merge across translation
/// units: two definitions of one object linked silently, where C17 6.9p5
/// allows one external definition and gcc reports `multiple definition`.
/// gcc has defaulted to `-fno-common` since 10 and emits none at all.
///
/// A `const` object is the other half: BSS-class storage is writable, so a
/// zero-initialized `const` was not read-only. Both the tentative and the
/// initialized form belong in `.rodata`.
#[test]
#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
fn codegen_zero_initialized_globals_are_definitions() {
    let asm = asm_for_at(
        "c17_zero_init_def",
        "int tentative;\n\
         int explicit_zero = 0;\n\
         int nonzero = 5;\n\
         const int c_tentative;\n\
         const int c_zero = 0;\n\
         static int s_zero = 0;\n",
        &[],
    );

    // Nothing exported is a common symbol any more.
    for name in ["tentative", "explicit_zero", "c_tentative", "c_zero"] {
        assert!(
            !asm.contains(&format!(".comm {name},")),
            "`{name}` should be a definition, not a common symbol:\n{asm}"
        );
    }
    // A `const` object is read-only, whatever its initializer. Asked as
    // "which section directive was last before the label", since a file has
    // several `.rodata` chunks and the labels are spread across them.
    for name in ["c_tentative", "c_zero"] {
        assert_eq!(
            section_of(&asm, name),
            Some(".section .rodata"),
            "`{name}` should be in .rodata:\n{asm}"
        );
    }
    for name in ["tentative", "explicit_zero"] {
        assert_eq!(
            section_of(&asm, name),
            Some(".bss"),
            "`{name}` should be in .bss:\n{asm}"
        );
    }
    assert_eq!(section_of(&asm, "nonzero"), Some(".data"));
    // An internal-linkage one keeps its `.local`/`.comm` pair, which does not
    // export anything and so cannot collide.
    assert!(
        asm.contains(".local s_zero"),
        "a static keeps internal linkage:\n{asm}"
    );
}

/// A member of a *static* object past `i32` is reached through a
/// materialised displacement, and its address constant keeps the whole offset.
///
/// Such an object cannot be run -- no loader maps it, and past 2 GB the
/// default code model cannot link it either -- so the assembly is what is
/// asserted. The expected spellings are gcc's for the same source.
#[test]
fn codegen_static_member_offset_past_i32() {
    let src = "\
struct Big { char pad[5000000000L]; int x; long y; };
struct Huge { short buf[(1L << 62) - 256]; int a, b, c, d; };
struct Big gb;
struct Huge gh;
long *gy = &gb.y;
int *gd = &gh.d;
void set(long v) { gb.y = v; }
";
    for opt in ["-O0", "-O2"] {
        for triple in [X86_64_LINUX, AARCH64_LINUX] {
            let asm = asm_for_with("static_member_offset_past_i32", triple, src, &[opt]);
            for needle in [
                "gb+5000000008",
                "gh+9223372036854775308",
                ".zero 9223372036854775312",
            ] {
                assert!(
                    asm.contains(needle),
                    "{triple} {opt}: no `{needle}` in:\n{asm}"
                );
            }
            // 5000000008 is 0x1_2A05_F208: all three pieces must be emitted.
            let set = body_of(&asm, "set");
            let pieces: &[&str] = if triple == X86_64_LINUX {
                &["$5000000008"]
            } else {
                &["#61960", "#10757, lsl #16", "#1, lsl #32"]
            };
            for piece in pieces {
                assert!(
                    set.contains(piece),
                    "{triple} {opt}: `set` lacks `{piece}`:\n{set}"
                );
            }
        }
    }
}

/// An initialized file-scope `extern` declaration is a definition (C17
/// 6.9.2p1), and Mach-O emits it as one: each object labelled once, global
/// unless a prior `static` made it internal, thread-local as a descriptor,
/// and reached directly rather than through the GOT, as a defined symbol is.
#[test]
fn codegen_macho_initialized_extern_is_a_definition() {
    let src = r#"
extern int y = 5;
int y;
static int x;
extern int x = 7;
extern const char s[] = "hi";
extern _Thread_local int t = 1;
/* `s[i]`: a byte at a constant index folds, leaving no reference to `s`. */
int get(int i) { return y + x + s[i] + t; }
"#;
    let asm = asm_for_with("macho_extern_def", AARCH64_DARWIN, src, &["-O2", "-w"]);
    for name in ["_y", "_x", "_s", "_t", "_t$tlv$init"] {
        assert_eq!(
            asm.matches(&format!("\n{name}:\n")).count(),
            1,
            "{name} defined once:\n{asm}"
        );
    }
    assert!(
        asm.contains(".globl _y\n.p2align 2\n_y:\n    .long 5\n"),
        "external y = 5:\n{asm}"
    );
    assert!(
        asm.contains(".p2align 2\n_x:\n    .long 7\n") && !asm.contains(".globl _x"),
        "x = 7, local:\n{asm}"
    );
    assert!(
        asm.contains(".section __TEXT,__const\n.globl _s\n_s:\n    .ascii \"hi\"\n"),
        "external const s:\n{asm}"
    );
    assert!(
        asm.contains(".globl _t\n") && asm.contains("_t$tlv$init:\n    .long 1\n"),
        "external thread-local t = 1:\n{asm}"
    );
    let body = body_of(&asm, "get");
    for name in ["_y", "_x", "_s"] {
        assert!(
            body.contains(&format!("{name}@PAGEOFF")) && !body.contains(&format!("{name}@GOTPAGE")),
            "{name} reached directly:\n{body}"
        );
    }
}
