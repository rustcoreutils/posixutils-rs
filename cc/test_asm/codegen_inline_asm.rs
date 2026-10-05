//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Inline assembly whose assembly text is the claim: compiled in process, for
// an explicit target, so every host checks both backends.
//

use super::asm_probe::{asm_for, asm_for_with, AARCH64_LINUX, X86_64_LINUX};

/// An `asm goto` label reference is spelled the way its definition is.
///
/// A function whose identifier holds an extended character needs its local
/// labels quoted, and `Label::name()` quotes them. The aarch64 operand builder
/// hand-rolled `format!(".L{}_{}", ...)` instead, so the branch the template
/// expanded to named `.Lf\u{e9}_1` while the block that defines it was emitted
/// as `".Lf\u{e9}_1"` -- two spellings of one label, which the assembler reads
/// as an undefined symbol.
///
/// Asserted as agreement rather than as a literal: the test does not care
/// whether the label is quoted, only that both sites agree.
#[test]
fn codegen_asm_goto_label_matches_its_definition() {
    let code = "
int f\u{e9}(int x) {
    __asm__ goto (\"b %l[done]\" : : : : done);
    return 0;
done:
    return 1;
}
";

    for triple in [AARCH64_LINUX, X86_64_LINUX] {
        let asm = asm_for("asm_goto_label_quoting", triple, code);

        // Every local label this function defines, exactly as written.
        let defined: Vec<&str> = asm
            .lines()
            .map(str::trim)
            .filter_map(|l| l.strip_suffix(':'))
            .filter(|l| l.contains(".L") && l.contains("f\u{e9}"))
            .collect();
        assert!(
            !defined.is_empty(),
            "{triple}: no local labels defined:\n{asm}"
        );

        // Every branch target naming one of this function's local labels.
        for line in asm.lines().map(str::trim) {
            let Some(rest) = line
                .strip_prefix("b ")
                .or_else(|| line.strip_prefix("jmp "))
            else {
                continue;
            };
            let target = rest.trim();
            if !target.contains("f\u{e9}") {
                continue;
            }
            assert!(
                defined.contains(&target),
                "{triple}: branch to {target} but the definitions are {defined:?}:\n{asm}"
            );
        }
    }
}

/// One asm statement over 16 locals with 25 operands: `"+m"` chars and
/// longs, `"=m"` and `"m"`, in the gcc torture shape.
///
/// A memory operand naming an object at a constant offset needs no register:
/// it is addressed where it lives, `-N(%rbp)`, as gcc does. c17 routed it
/// through an address pseudo instead, and with more operands than registers
/// that address was spilled -- and x86-64 then substituted the *spill slot*
/// as the operand, so the template read and wrote the saved pointer rather
/// than the object. gcc returns 0; c17 returned 15.
///
/// `tests/codegen/inline_asm.rs` runs the same function, from the same
/// generator, in `codegen_inline_asm_x86_64_pressure_mega`.
fn many_local_memory_operands_x86_64() -> String {
    let plan = ["ch", "rw", "wo", "in"].repeat(4);
    let (mut decls, mut outs, mut ins, mut checks) = (vec![], vec![], vec![], vec![]);
    outs.push(r#"[sum] "=m"(sum)"#.to_string());
    let mut body = String::from(r"movq $0, %[sum]\n\t");
    let mut sum = 0;
    for (i, kind) in plan.iter().enumerate() {
        match *kind {
            "ch" => {
                decls.push(format!("    char m{i} = {i};"));
                outs.push(format!(r#"[m{i}] "+m"(m{i})"#));
                body.push_str(&format!(
                    r"movb %[m{i}], %%al\n\taddb $1, %%al\n\tmovb %%al, %[m{i}]\n\t"
                ));
                checks.push(format!("    if (m{i} != {i} + 1) return {};", i + 1));
            }
            "rw" => {
                decls.push(format!("    long m{i} = {i} * 1000;"));
                outs.push(format!(r#"[m{i}] "+m"(m{i})"#));
                body.push_str(&format!(
                    r"movq %[m{i}], %%rax\n\taddq $1, %%rax\n\tmovq %%rax, %[m{i}]\n\t"
                ));
                checks.push(format!("    if (m{i} != {i} * 1000 + 1) return {};", i + 1));
            }
            "wo" => {
                decls.push(format!("    long m{i} = {i} * 1000;"));
                outs.push(format!(r#"[m{i}] "=m"(m{i})"#));
                body.push_str(&format!(r"movq ${}, %[m{i}]\n\t", i + 7));
                checks.push(format!("    if (m{i} != {}) return {};", i + 7, i + 1));
            }
            _ => {
                decls.push(format!("    long m{i} = {i} * 1000;"));
                ins.push(format!(r#"[m{i}] "m"(m{i})"#));
                body.push_str(&format!(r"movq %[m{i}], %%rax\n\taddq %%rax, %[sum]\n\t"));
                sum += i * 1000;
            }
        }
    }
    format!(
        r#"#define NI __attribute__((noinline))
volatile long sink;
NI void touch(void *p) {{ sink += *(volatile char *)p; }}

NI int many_memory_operands(void)
{{
    volatile char lo[4000];
{decls}
    long sum;
    touch((void *)lo);
    __asm__ volatile(
        "{body}"
        : {outs}
        : {ins}
        : "rax", "memory");
{checks}
    if (sum != {sum}) return 100;
    return 0;
}}

int main(void) {{ return many_memory_operands(); }}
"#,
        decls = decls.join("\n"),
        outs = outs.join(", "),
        ins = ins.join(", "),
        checks = checks.join("\n"),
    )
}

/// Every operand is a frame slot in the template, and no register is loaded
/// with an address for it.
#[test]
fn codegen_inline_asm_local_memory_operands_are_frame_slots_x86_64() {
    let asm = asm_for_with(
        "asm_mem_locals_shape",
        X86_64_LINUX,
        &many_local_memory_operands_x86_64(),
        &["-O2"],
    );
    let start = asm.find("many_memory_operands:").expect("the function");
    let body = &asm[start
        ..asm[start..]
            .find(".cfi_endproc")
            .map_or(asm.len(), |e| start + e)];
    assert!(body.contains("movb %al, -"), "{body}");
    assert!(body.contains("(%rbp), %al\n    addb $1, %al"), "{body}");
    assert!(
        !body.contains("(%r10)") && !body.contains("(%r11)"),
        "{body}"
    );
}
