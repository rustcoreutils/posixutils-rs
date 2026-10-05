//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Assembly cases of tests/codegen/inlining.rs, in process: which calls the
// inliner keeps (`noinline`, `always_inline` and its stack guard, weak
// definitions), which inline spellings emit a symbol, and the TLS model of an
// `extern` thread-local.
//

use crate::test_asm::asm_probe::{asm_for_with, AARCH64_LINUX, X86_64_LINUX};

/// `__attribute__((noinline))` is a directive, not a hint.
///
/// It was recognized and then ignored, so a small function marked `noinline`
/// was inlined anyway at `-O2` and the call disappeared. People reach for it
/// to keep a frame on the stack, to keep a symbol callable, or to work around
/// a miscompile -- none of which survive the size heuristic overruling them.
#[test]
fn codegen_noinline_is_honoured() {
    let src = r#"
static __attribute__((noinline)) int small(int x) { return x * 2; }
/* the same function without the attribute, to show the size alone
   would have had it inlined */
static int small_ok(int x) { return x * 3; }

int caller(int x) { return small(x) + small_ok(x); }
"#;
    for triple in [X86_64_LINUX, AARCH64_LINUX] {
        let asm = asm_for_with("noinline", triple, src, &["-O2"]);
        assert!(
            asm.contains("small:"),
            "{triple}: noinline function was not emitted:\n{asm}"
        );
        let calls = asm
            .lines()
            .filter(|l| {
                let l = l.trim();
                (l.starts_with("call") || l.starts_with("bl ")) && l.contains("small")
            })
            .count();
        assert!(
            calls >= 1,
            "{triple}: noinline function was inlined away (no call survived):\n{asm}"
        );
        assert!(
            !asm.contains("small_ok:") || calls >= 1,
            "{triple}: unexpected shape:\n{asm}"
        );
    }
}

/// The attribute is honoured in either spelling and wherever it sits -- before
/// or after the declaration specifiers, on the definition, or on nothing but
/// an earlier prototype, which is how gcc treats it.
#[test]
fn codegen_noinline_spellings_and_placement() {
    for src in [
        "static __attribute__((noinline)) int f(int x) { return x + 1; }\nint g(int x){return f(x);}",
        "__attribute__((noinline)) static int f(int x) { return x + 1; }\nint g(int x){return f(x);}",
        "static int f(int x) __attribute__((noinline)) { return x + 1; }\nint g(int x){return f(x);}",
        "static __attribute__((__noinline__)) int f(int x) { return x + 1; }\nint g(int x){return f(x);}",
        "static int f(int x) __attribute__((noinline));\nstatic int f(int x) { return x + 1; }\nint g(int x){return f(x);}",
    ] {
        let asm = asm_for_with("noinline_spelling", X86_64_LINUX, src, &["-O2"]);
        assert!(
            asm.lines().any(|l| l.trim().starts_with("call") && l.contains('f')),
            "no call survived for:\n{src}\n{asm}"
        );
    }
}

/// `always_inline` overrides the size heuristics, and applies at `-O0`.
///
/// Both halves are needed to match gcc. A body far over the inline threshold
/// is still spliced in at `-O2`, and the attribute takes effect at `-O0`,
/// where the inliner is otherwise switched off entirely -- code that reaches
/// for the attribute (inline-asm wrappers, intrinsics headers) is relying on
/// the body actually being there.
#[test]
fn codegen_always_inline_overrides_heuristics_and_o0() {
    // Well past any size threshold: a plain `inline` hint would be refused.
    let big: String = (1..120)
        .map(|i| {
            format!(
                "    s += x*{i}; s ^= s << ({i}%7); s -= x/({}); s += x%({});\n",
                i + 1,
                i + 2
            )
        })
        .collect();
    let src = format!(
        "static __attribute__((always_inline)) inline int huge(int x)\n\
         {{\n    int s = 0;\n{big}    return s;\n}}\n\
         int caller(int x) {{ return huge(x); }}\n"
    );

    for opt in ["-O0", "-O2"] {
        let asm = asm_for_with("always_inline_big", X86_64_LINUX, &src, &[opt]);
        assert!(
            !asm.lines().any(|l| l.trim().starts_with("call huge")),
            "{opt}: always_inline function was left as a call:\n{asm}"
        );
    }
}

/// The attribute is honoured in either spelling and from a prototype, and
/// `noinline` beats it when both are present -- which is what gcc does, with a
/// `-Wattributes` warning.
#[test]
fn codegen_always_inline_spellings_and_conflict() {
    let inlined = [
        "static __attribute__((always_inline)) inline int f(int x) { return x+1; }",
        "static __attribute__((__always_inline__)) inline int f(int x) { return x+1; }",
        "static inline int f(int x) __attribute__((always_inline));\n\
         static inline int f(int x) { return x+1; }",
    ];
    for decl in inlined {
        let src = format!("{decl}\nint g(int x){{return f(x);}}\n");
        let asm = asm_for_with("always_inline_spelling", X86_64_LINUX, &src, &["-O0"]);
        assert!(
            !asm.lines().any(|l| l.trim().starts_with("call f")),
            "call survived at -O0 for:\n{decl}\n{asm}"
        );
    }

    // noinline wins: the call must survive even at -O2.
    let src = "static __attribute__((noinline)) __attribute__((always_inline)) inline\n\
               int f(int x) { return x+1; }\nint g(int x){return f(x);}\n";
    let asm = asm_for_with("always_inline_conflict", X86_64_LINUX, src, &["-O2"]);
    assert!(
        asm.lines().any(|l| l.trim().starts_with("call f")),
        "noinline must outrank always_inline:\n{asm}"
    );
}

/// `always_inline` overrides the *desirability* heuristics, not the
/// stack-safety guards.
///
/// The distinction is c17-specific and load-bearing. gcc can honour the
/// attribute unconditionally because its frames are compact; c17 has no
/// register promotion, so every inlined copy adds roughly eight bytes of
/// stack, which is why `should_inline` caps growth into a recursive caller at
/// `RECURSIVE_CALLER_MAX_STACK`. Inlining past that cap does not produce wrong
/// code -- it exhausts the stack at a recursion depth the program's own guard
/// thought was safe.
///
/// Found by the CPython gate: `test_isinstance` segfaulted in
/// `test_infinitely_many_bases`, which recurses until it expects a
/// `RecursionError`, once `always_inline` was allowed to bypass this check.
/// CPython documents the same hazard in `Include/pyport.h`, noting that
/// forcing inlining takes its per-call stack from 6 KB to 15 KB.
#[test]
fn codegen_always_inline_yields_to_the_recursive_stack_guard() {
    // Big enough that caller_size * 8 clears RECURSIVE_CALLER_MAX_STACK --
    // after simplification, so no statement may undo the ones before it: a
    // shift by `i % 7` alone is `s ^= s` every seventh line.
    let body: String = (1..200)
        .map(|i| format!("    s += x*{i}; s ^= s << ({i}%7+1); s -= x/({});\n", i + 1))
        .collect();
    let src = format!(
        "static __attribute__((always_inline)) inline int helper(int x)\n\
         {{\n    int s = 0;\n{body}    return s;\n}}\n\
         int rec(int n) {{ if (n <= 0) return 0; return helper(n) + rec(n-1); }}\n"
    );

    let asm = asm_for_with("always_inline_recursive", X86_64_LINUX, &src, &["-O2"]);
    assert!(
        asm.lines().any(|l| l.trim().starts_with("call helper")),
        "always_inline must not inline into a recursive caller past the \
         stack cap; the frame growth overflows the stack at a depth the \
         program's own recursion guard considers safe:\n{asm}"
    );
}

/// The four inline spellings differ in whether they emit an external symbol.
///
/// Measured against gcc. Note C99 and GNU semantics are *opposite* for
/// `extern inline`, which is exactly what `__gnu_inline__` selects:
///
/// | spelling                          | external definition? |
/// |-----------------------------------|----------------------|
/// | `static inline`                   | no (internal)        |
/// | plain `inline`, no `extern` decl   | no                   |
/// | `extern inline` (C99)             | yes                  |
/// | `extern inline` + `__gnu_inline__` | no                   |
#[test]
fn codegen_inline_spellings_emit_the_right_symbols() {
    let src = r#"
static inline int si(int x) { return x + 1; }

inline int pi(int x) { return x + 2; }

extern int ei(int x);
inline int ei(int x) { return x + 3; }

extern __inline __attribute__((__gnu_inline__)) int gi(int x) { return x + 4; }

/* Reference each one so nothing is dropped merely for being unused. */
int use_all(int x) { return si(x) + pi(x) + ei(x) + gi(x); }
"#;

    for triple in [X86_64_LINUX, AARCH64_LINUX] {
        let asm = asm_for_with("inline_spellings", triple, src, &["-O"]);

        assert!(
            asm.contains("ei:"),
            "{triple}: C99 `extern inline` provides the external definition:\n{asm}"
        );
        assert!(
            !asm.contains(".globl pi"),
            "{triple}: a plain `inline` definition provides no external definition:\n{asm}"
        );
        assert!(
            !asm.contains(".globl gi"),
            "{triple}: `__gnu_inline__` provides no external definition:\n{asm}"
        );
        assert!(
            !asm.contains(".globl si"),
            "{triple}: `static inline` has internal linkage:\n{asm}"
        );
    }
}

/// An `extern` thread-local is in another object, so its offset is never known
/// at link time -- Initial Exec regardless of build mode.
#[test]
fn codegen_extern_thread_local_always_uses_initial_exec() {
    let src = r#"
extern _Thread_local int ev;
int read_ev(int x) { return x + ev; }
"#;
    let asm = asm_for_with("tls_extern", X86_64_LINUX, src, &["-O"]);
    assert!(
        asm.contains("@GOTTPOFF"),
        "x86_64: extern TLS needs Initial Exec even in an executable:\n{asm}"
    );
    // aarch64 spells the same relocations `:gottprel:`/`:gottprel_lo12:`;
    // x86-64's `gottpoff` there is rejected by the assembler.
    let asm = asm_for_with("tls_extern", AARCH64_LINUX, src, &["-O"]);
    assert!(
        asm.contains(":gottprel:") && asm.contains(":gottprel_lo12:") && !asm.contains("gottpoff"),
        "aarch64: extern TLS needs Initial Exec even in an executable:\n{asm}"
    );
}

/// A `weak` definition may be replaced at link time, so its body is not
/// authoritative and must never be spliced into a caller.
///
/// The inliner consulted nothing about weakness, so a small weak function was
/// inlined on the same terms as a strong one and the interposing definition
/// simply never ran. Asserted on assembly because a single translation unit
/// cannot exhibit interposition: the property is that the *call* survives.
#[test]
#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
fn codegen_weak_definition_is_not_inlined() {
    let asm = crate::test_asm::asm_probe::asm_for_at(
        "c17_weak_noinline_",
        r#"
__attribute__((weak)) int impl(void) { return 1; }
/* Static, so interposition cannot apply and it may still be inlined. */
__attribute__((weak)) static int local_impl(void) { return 2; }
int call_weak(void) { return impl(); }
int call_weak_static(void) { return local_impl(); }
"#,
        &["-O2"],
    );
    let weak_caller = asm.split("\ncall_weak:").nth(1).unwrap_or_default();
    assert!(
        weak_caller.contains("call\timpl") || weak_caller.contains("call impl"),
        "an interposable definition must still be called:\n{weak_caller}"
    );
    // The `static` one has internal linkage, so nothing can replace it and the
    // attribute does not bar inlining.
    let static_caller = asm
        .split("\ncall_weak_static:")
        .nth(1)
        .unwrap_or_default()
        .split("\n\t.size")
        .next()
        .unwrap_or_default();
    assert!(
        !static_caller.contains("local_impl"),
        "a weak *static* cannot be interposed, so it may be inlined:\n{static_caller}"
    );
}
