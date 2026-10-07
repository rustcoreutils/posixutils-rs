//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Inline assembly and attributes, as the assembly shows them: asm labels,
// aliases, weak and hidden symbols, sections. In process; the cases that
// assemble, link or run stay in `tests/codegen/asm_attributes.rs`.
//

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
use super::asm_probe::{asm_for_at, section_of};
use super::asm_probe::{asm_for_with, body_of, host_asm, AARCH64_LINUX, X86_64_LINUX};

/// The label reaches the assembly, on both ABIs, for calls and definitions.
///
/// The runtime test above only proves the two names resolve to one another;
/// this proves the *declared* name never appears as a symbol at all.
#[test]
fn codegen_asm_label_reaches_the_assembly() {
    let src = r#"
extern int myfn(int) __asm__("realfn");
int defrenamed(int x) __asm__("real_def");
int defrenamed(int x) { return x + 1; }
extern int my_var __asm__("real_var");

int call(int x) { return myfn(x) + my_var; }
"#;

    for triple in [X86_64_LINUX, AARCH64_LINUX] {
        let asm = asm_for_with("asm_label", triple, src, &["-O"]);
        assert!(
            asm.contains("realfn"),
            "{triple}: call should target the asm label:\n{asm}"
        );
        assert!(
            !asm.contains("myfn"),
            "{triple}: the declared name must not be emitted:\n{asm}"
        );
        assert!(
            asm.contains("real_def"),
            "{triple}: a definition's asm label should name its own label:\n{asm}"
        );
        assert!(
            !asm.contains("defrenamed"),
            "{triple}: the declared name must not be emitted:\n{asm}"
        );
        assert!(
            asm.contains("real_var"),
            "{triple}: a global's asm label should be the emitted symbol:\n{asm}"
        );
        assert!(
            !asm.contains("my_var"),
            "{triple}: the declared name must not be emitted:\n{asm}"
        );
    }
}

/// An asm label belongs to the declarator it was written on, and to nothing
/// else.
///
/// The label is collected wherever a type specifier can appear, but only a
/// file-scope declarator claims one. A label written anywhere else -- on a
/// block-scope declaration, on a struct definition, in a `for` initializer --
/// stayed pending and was claimed by whatever was declared next, so an
/// unrelated global was emitted under someone else's assembler name. The
/// symptom at link time is an undefined reference to a name that is plainly
/// defined in the source.
#[test]
fn codegen_asm_label_does_not_leak_to_the_next_declaration() {
    let src = r#"
extern int keep_me;

/* GCC's local register variable: a register constraint, not a rename. */
void with_register(void) { register long r __asm__("rdi"); (void)r; }
int after_register = 1;

void with_local(void) { int x __asm__("bogus_local"); (void)x; }
int after_local = 2;

void with_static_local(void) { static int x __asm__("bogus_static"); (void)x; }
int after_static_local = 3;

struct Tagged { int a; } __asm__("bogus_struct");
int after_struct = 4;

void with_for_init(void) { for (int i __asm__("bogus_for") = 0; i < 1; i++) ; }
int after_for_init = 5;

int keep_me = 6;
"#;

    for triple in [X86_64_LINUX, AARCH64_LINUX] {
        let asm = asm_for_with("asm_label_leak", triple, src, &["-O"]);
        for name in [
            "after_register",
            "after_local",
            "after_static_local",
            "after_struct",
            "after_for_init",
            "keep_me",
        ] {
            assert!(
                asm.contains(name),
                "{triple}: {name} should be emitted under its own name:\n{asm}"
            );
        }
        for stolen in [
            "rdi:",
            "bogus_local",
            "bogus_static",
            "bogus_struct",
            "bogus_for",
        ] {
            assert!(
                !asm.contains(stolen),
                "{triple}: a stray asm label was claimed by a later symbol ({stolen}):\n{asm}"
            );
        }
    }
}

/// A function designator tested for truth is tested as the pointer it decays
/// to, at the full width of an address (C17 6.3.2.1p4).
///
/// Typed as the function itself, the test had no width -- a function's is 0
/// -- which both back ends raised to 32 bits: `if (weak_fn)` compared the low
/// half of the address, so a function placed at a multiple of 4 GiB read as
/// absent. `cmpl`/`cmp w` against 0 is the defect; the whole register is the
/// fix.
#[test]
fn codegen_a_function_designator_is_tested_at_address_width() {
    // And comparing two of them, which is the same question at a second site.
    let src = "extern int wkfn(void) __attribute__((weak));\n\
               extern int other(void) __attribute__((weak));\n\
               int probe(void) { if (wkfn) return 1; return 0; }\n\
               int same(void) { return wkfn == other; }\n";
    let asm = asm_for_with("fn_truth", X86_64_LINUX, src, &["-O0"]);
    let body = body_of(&asm, "probe");
    assert!(body.contains("cmpq $0"), "{body}");
    assert!(!body.contains("cmpl $0"), "{body}");
    let body = body_of(&asm, "same");
    assert!(body.contains("cmpq"), "{body}");
    assert!(!body.contains("cmpl"), "{body}");
    // The address is compared at sixty-four bits. What that comparison
    // yields is an `int`, and a branch on it rightly tests thirty-two.
    let asm = asm_for_with("fn_truth_a64", AARCH64_LINUX, src, &["-O0"]);
    for f in ["probe", "same"] {
        let body = body_of(&asm, f);
        let lines: Vec<&str> = body.lines().map(str::trim).collect();
        let address_test = lines
            .iter()
            .position(|l| l.starts_with("cmp "))
            .unwrap_or_else(|| panic!("no comparison in {f}:\n{body}"));
        assert!(lines[address_test].starts_with("cmp x"), "{body}");
    }
}

/// Seven defects found by review of the symbol-attribute and overflow-builtin
/// work, each reproduced against `gcc -std=c17` before being fixed.
///
/// The attribute cases are grouped because they share a shape: an attribute
/// that reaches the *parser* but not the object file, or reaches an object it
/// was never written on.
///
/// The assembly half of `codegen_symbol_attributes_do_not_leak_or_vanish`
/// (`tests/codegen/asm_attributes.rs`), whose running half stays there:
/// `weak` on a declaration with no definition. Every spelling and position,
/// since only the trailing-attribute-on-a-function-declarator one was broken.
#[test]
fn codegen_symbol_attributes_weak_declaration_directives() {
    let declared = r#"
extern int missing_a(void) __attribute__((weak));
__attribute__((weak)) extern int missing_b(void);
int missing_c(void) __attribute__((weak));
extern int missing_var __attribute__((weak));
int main(void) {
    if (missing_a) return 1;
    if (missing_b) return 2;
    if (missing_c) return 3;
    if (&missing_var) return 4;
    return 0;
}
"#;

    // What c17 emits is c17's business, and is checked on every target: ELF
    // spells both sides `.weak`, Mach-O has `.weak_reference` for this one and
    // rejects `.weak` as an unknown directive.
    let asm = host_asm("c17_weak_decl_", declared);
    for sym in ["missing_a", "missing_b", "missing_c", "missing_var"] {
        assert!(
            asm.contains(&format!(".weak {sym}"))
                || asm.contains(&format!(".weak_reference _{sym}")),
            "no weak directive for {sym}:\n{asm}"
        );
    }
}

/// A declaration's `weak` and visibility reach the assembly only for a symbol
/// the code refers to. `.hidden f` on a name nothing uses still puts an
/// undefined hidden `f` in the symbol table, and the linker rejects that
/// outright: perl's `proto.h` declares `Perl_do_exec` hidden in every unit
/// and defines it nowhere, so miniperl failed to link with "hidden symbol
/// `Perl_do_exec' isn't defined".
#[test]
fn codegen_unreferenced_declaration_emits_no_symbol_directives() {
    let src = r#"
extern int unused_fn(void) __attribute__((visibility("hidden")));
extern int unused_var __attribute__((visibility("hidden")));
extern int unused_weak(void) __attribute__((weak));
extern int used_fn(void) __attribute__((visibility("hidden")));
extern int used_var __attribute__((visibility("hidden")));
extern int used_weak(void) __attribute__((weak));
int f(void) { return used_fn() + used_var + (used_weak ? 1 : 0); }
"#;
    for triple in [X86_64_LINUX, AARCH64_LINUX] {
        for opt in ["-O0", "-O2"] {
            let asm = asm_for_with("unref_decl_attrs", triple, src, &[opt]);
            for sym in ["unused_fn", "unused_var", "unused_weak"] {
                assert!(
                    !asm.contains(sym),
                    "{triple} {opt}: unreferenced {sym} reached the assembly:\n{asm}"
                );
            }
            for directive in [".hidden used_fn", ".hidden used_var", ".weak used_weak"] {
                assert!(
                    asm.contains(directive),
                    "{triple} {opt}: missing `{directive}`:\n{asm}"
                );
            }
        }
    }
}

/// A zero-initialized definition took the `.comm`/`.bss` fast path, which
/// returns before the `.weak` and visibility directives are emitted. A common
/// symbol carries neither, so both were silently lost -- a hidden variable
/// escaping as default visibility is an ABI change, not a cosmetic one.
#[test]
#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
fn codegen_zero_init_keeps_weak_and_visibility() {
    let asm = host_asm(
        "c17_zeroinit_attrs_",
        r#"
__attribute__((visibility("hidden"))) int hidden_zero;
__attribute__((weak)) int weak_zero;
__attribute__((visibility("hidden"))) int hidden_init = 7;
int plain_zero;
int main(void) { return hidden_zero + weak_zero + hidden_init - 7 + plain_zero; }
"#,
    );
    assert!(
        asm.contains(".hidden hidden_zero"),
        "zero-initialized hidden variable lost its visibility:\n{asm}"
    );
    assert!(
        asm.contains(".weak weak_zero"),
        "zero-initialized weak variable lost its weakness:\n{asm}"
    );
    assert!(
        asm.contains(".hidden hidden_init"),
        "initialized hidden variable lost its visibility:\n{asm}"
    );
    // The unattributed one still gets the fast path it was always entitled
    // to -- but as a *definition*. A common symbol merges with another
    // translation unit's definition of the same object, which C17 6.9p5 does
    // not allow and gcc reports.
    assert_eq!(
        section_of(&asm, "plain_zero"),
        Some(".bss"),
        "an unattributed zero-initialized global still belongs in .bss:\n{asm}"
    );
    assert!(
        !asm.contains(".comm plain_zero"),
        "it must not be a common symbol:\n{asm}"
    );
}

/// An attribute belongs to the declarator it follows, and a declaration may
/// hold several. At file scope the continuation loop went straight from
/// `parse_declarator` to symbol binding with no attribute parse at all, so
/// `int a, b __attribute__((section(".s")));` was not a misplaced attribute
/// but a syntax error. The same declaration inside a function was accepted,
/// because the block-scope loop parses attributes after every declarator.
///
/// The distinction the test pins is per-declarator: `weak`, `section`,
/// `visibility` and `used` name one symbol, so an attribute written on `b`
/// must not reach `a`.
#[test]
#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
fn codegen_attribute_binds_to_its_own_file_scope_declarator() {
    let asm = host_asm(
        "attr_later_declarator",
        r#"
int plain_a = 1, sect_b __attribute__((section(".mysec"))) = 2;
int plain_c = 3, weak_d __attribute__((weak)) = 4;
int plain_e = 5, hidden_f __attribute__((visibility("hidden"))) = 6;
int plain_g = 7, aligned_h __attribute__((aligned(64))) = 8;
"#,
    );

    // The attributed symbol got its attribute...
    assert!(
        asm.contains(".mysec"),
        "sect_b should land in .mysec:\n{asm}"
    );
    assert!(
        asm.contains(".weak weak_d") || asm.contains(".weak\tweak_d"),
        "weak_d should be weak:\n{asm}"
    );
    assert!(
        asm.contains(".hidden hidden_f") || asm.contains(".hidden\thidden_f"),
        "hidden_f should be hidden:\n{asm}"
    );

    // ...and its neighbours in the same declaration did not.
    for leaked in ["plain_a", "plain_c", "plain_e", "plain_g"] {
        assert!(
            !asm.contains(&format!(".weak {leaked}")),
            "{leaked} must not be weak:\n{asm}"
        );
        assert!(
            !asm.contains(&format!(".hidden {leaked}")),
            "{leaked} must not be hidden:\n{asm}"
        );
    }

    // A section directive names exactly one symbol: the one that asked.
    let mysec_line = asm
        .lines()
        .position(|l| l.contains(".mysec"))
        .expect("a .mysec directive");
    let after: String = asm
        .lines()
        .skip(mysec_line)
        .take(6)
        .collect::<Vec<_>>()
        .join("\n");
    assert!(
        after.contains("sect_b"),
        ".mysec should be followed by sect_b:\n{after}"
    );
    assert!(
        !after.contains("plain_a"),
        "plain_a must not share sect_b's section:\n{after}"
    );
}

/// A name that appears only inside an assembly template still refers to the
/// function: it reaches the assembler with no IR reference for the prune to
/// find.
#[test]
#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
fn codegen_asm_template_reference_keeps_a_static_alive() {
    let asm = asm_for_at(
        "c17_asm_names_static_",
        r#"
static int helper(void) { return 7; }
int main(void) { __asm__ volatile ("call helper" ::: "memory"); return 0; }
"#,
        &["-O2"],
    );
    assert!(
        asm.contains("\nhelper:"),
        "a static named only in an asm template must survive:\n{asm}"
    );
}

/// The directives an alias is made of, as gcc writes them on ELF: `.set` for
/// the value, `.globl` or `.weak` for its binding and nothing for a static
/// one, visibility on the alias itself, and no `.type`/`.size`, which the
/// assembler copies from the target.
#[test]
fn codegen_alias_attribute_asm() {
    let src = r#"
int f(void) { return 1; }
int g(void) __attribute__((alias("f")));
int w(void) __attribute__((weak, alias("f")));
int h(void) __attribute__((alias("f"), visibility("hidden")));
static int s = 5;
static int u __attribute__((alias("s")));
int *use(void) { return &u; }
"#;
    for triple in [X86_64_LINUX, AARCH64_LINUX] {
        let asm = asm_for_with("alias_asm", triple, src, &[]);
        let lines: Vec<&str> = asm.lines().map(str::trim).collect();
        for want in [
            ".globl g",
            ".set g, f",
            ".weak w",
            ".set w, f",
            ".globl h",
            ".hidden h",
            ".set h, f",
            ".set u, s",
        ] {
            assert!(lines.contains(&want), "{triple}: missing {want:?}:\n{asm}");
        }
        // A directive naming the symbol, however its operands continue.
        let names = |directive: &str, sym: &str| {
            lines.iter().any(|l| {
                let mut words = l.split([' ', '\t', ',']).filter(|w| !w.is_empty());
                words.next() == Some(directive) && words.next() == Some(sym)
            })
        };
        for (directive, sym) in [
            (".globl", "u"),
            (".globl", "w"),
            (".type", "g"),
            (".size", "g"),
        ] {
            assert!(
                !names(directive, sym),
                "{triple}: unexpected {directive} {sym}:\n{asm}"
            );
        }
    }
}

/// The directives an indirect function is made of, as gcc writes them on
/// ELF: `.globl` unless static, visibility, `.type @gnu_indirect_function`,
/// then `.set` naming the resolver. A static resolver nothing else names
/// survives the optimizer. Calls go through the PLT and the address through
/// the GOT, so what the program sees is the binding the resolver chose.
#[test]
fn codegen_ifunc_attribute_asm() {
    let src = r#"
static int impl(int x) { return x; }
static void *res(void) { return (void *)impl; }
int f(int) __attribute__((ifunc("res")));
static int g(int) __attribute__((ifunc("res")));
int h(int) __attribute__((ifunc("res"), visibility("hidden")));
int call(void) { return f(1) + g(2) + h(3); }
void *addr(void) { return (void *)f; }
"#;
    for triple in [X86_64_LINUX, AARCH64_LINUX] {
        let asm = asm_for_with("ifunc_asm", triple, src, &["-O2"]);
        let lines: Vec<&str> = asm.lines().map(str::trim).collect();
        let at = |want: &str| {
            lines
                .iter()
                .position(|l| *l == want)
                .unwrap_or_else(|| panic!("{triple}: missing {want:?}:\n{asm}"))
        };
        // In gcc's order: binding, visibility, type, value.
        let order = [".globl f", ".type f, @gnu_indirect_function", ".set f, res"];
        assert!(
            order.windows(2).all(|w| at(w[0]) < at(w[1])),
            "{triple}:\n{asm}"
        );
        let order = [
            ".globl h",
            ".hidden h",
            ".type h, @gnu_indirect_function",
            ".set h, res",
        ];
        assert!(
            order.windows(2).all(|w| at(w[0]) < at(w[1])),
            "{triple}:\n{asm}"
        );
        at(".type g, @gnu_indirect_function");
        at(".set g, res");
        assert!(
            !lines.contains(&".globl g"),
            "{triple}: static g exported:\n{asm}"
        );
        // The resolver is kept, and stays local.
        at("res:");
        assert!(!lines.contains(&".globl res"), "{triple}:\n{asm}");
        // Never a body or a `.size` for the indirect function itself.
        assert!(!lines.contains(&"f:"), "{triple}:\n{asm}");
        assert!(!asm.contains(".size f,"), "{triple}:\n{asm}");

        let call = body_of(&asm, "call");
        let addr = body_of(&asm, "addr");
        if triple == X86_64_LINUX {
            for want in ["call f@PLT", "call g@PLT", "call h@PLT"] {
                assert!(call.contains(want), "{triple}: no {want:?}:\n{call}");
            }
            assert!(addr.contains("f@GOTPCREL(%rip)"), "{triple}:\n{addr}");
        } else {
            for want in ["bl f", "bl g", "bl h"] {
                assert!(
                    call.lines().any(|l| l.trim() == want),
                    "{triple}: no {want:?}:\n{call}"
                );
            }
            assert!(addr.contains(":got:f"), "{triple}:\n{addr}");
        }
    }
}
