//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Unwind tables: the CFI rules that let `backtrace()`, a debugger, a profiler
// or the C++ runtime walk out of a c17 function. They are part of the default
// output, like gcc's `-fasynchronous-unwind-tables`, not something `-g` adds.
//

use super::asm_probe::{asm_for_with, body_of, AARCH64_DARWIN, AARCH64_LINUX, X86_64_LINUX};
use std::collections::HashSet;

/// Every frame shape the prologue has: callee-saved general and FP registers
/// live across a call, `alloca` and a VLA moving the stack pointer after the
/// prologue, a frame past what one `stp` pre-index can allocate (and past the
/// 4 KiB a single `sub` immediate holds), an over-aligned local, a variadic
/// function, and a mid-function return. Each calls on, so every one of them
/// is a frame the unwinder has to step through. Shared with
/// `tests/codegen/unwind.rs`, which runs it under `backtrace()`.
const FRAMES: &str = include_str!("../tests/codegen/unwind_frames.c");

/// [`FRAMES`] needs a `depth` to call; this one keeps the call a call.
const DEPTH_STUB: &str = "__attribute__((noinline)) int depth(void) { return sink; }\n";

/// What `.cfi_remember_state` saves -- the CFA rule and the saved registers --
/// with the SP and FP positions that held at that point.
type Remembered = ((String, i64), HashSet<String>, Option<i64>, Option<i64>);

/// One function's CFI, followed instruction by instruction.
///
/// Tracks where the stack pointer and frame pointer are relative to the CFA,
/// and checks at every instruction boundary that the CFA rule names the same
/// address. At every call the frame pointer, the return address (implicit on
/// x86-64) and every callee-saved register the function touches must have a
/// rule; at every return the CFA must be the stack pointer itself and no
/// register may still be described as saved, since its slot is now below the
/// stack pointer where a signal frame can overwrite it.
struct CfiWalk<'a> {
    x86: bool,
    func: &'a str,
    body: &'a str,
    /// The CFA rule: register and offset.
    cfa: (String, i64),
    saved: HashSet<String>,
    /// CFA minus SP and CFA minus FP, when known.
    sp: Option<i64>,
    fp: Option<i64>,
    stack: Vec<Remembered>,
    calls: usize,
    rets: usize,
}

fn imm(s: &str) -> i64 {
    let s = s.trim().trim_start_matches(['#', '$']);
    let (n, shifted) = match s.split_once(", lsl #12") {
        Some((n, _)) => (n, true),
        None => (s, false),
    };
    let v: i64 = n.parse().unwrap_or_else(|_| panic!("immediate {s:?}"));
    if shifted {
        v << 12
    } else {
        v
    }
}

impl<'a> CfiWalk<'a> {
    fn new(x86: bool, func: &'a str, body: &'a str) -> Self {
        let (sp, cfa) = if x86 { ("%rsp", 8) } else { ("sp", 0) };
        CfiWalk {
            x86,
            func,
            body,
            cfa: (sp.to_string(), cfa),
            saved: HashSet::new(),
            sp: Some(cfa),
            fp: None,
            stack: Vec::new(),
            calls: 0,
            rets: 0,
        }
    }

    fn fail(&self, line: &str, why: &str) -> ! {
        panic!("{}: at `{line}`: {why}\n{}", self.func, self.body)
    }

    /// The callee-saved registers this function's instructions name.
    fn callee_saved_used(&self) -> HashSet<String> {
        let mut used = HashSet::new();
        for line in self.body.lines().map(str::trim) {
            if line.starts_with('.') || line.ends_with(':') {
                continue;
            }
            for tok in line.split(|c: char| !(c.is_ascii_alphanumeric() || c == '%')) {
                let t = tok.trim_start_matches('%');
                let name = if self.x86 {
                    match t {
                        "rbx" | "ebx" | "bx" | "bl" => Some("%rbx".to_string()),
                        _ => (12..=15)
                            .find(|n| {
                                [
                                    format!("r{n}"),
                                    format!("r{n}d"),
                                    format!("r{n}w"),
                                    format!("r{n}b"),
                                ]
                                .contains(&t.to_string())
                            })
                            .map(|n| format!("%r{n}")),
                    }
                } else {
                    let (class, num) = t.split_at(t.len().min(1));
                    match (class, num.parse::<u32>()) {
                        ("x" | "w", Ok(n)) if (19..=28).contains(&n) => Some(format!("x{n}")),
                        ("d" | "s" | "q" | "v" | "h" | "b", Ok(n)) if (8..=15).contains(&n) => {
                            Some(format!("d{n}"))
                        }
                        _ => None,
                    }
                };
                used.extend(name);
            }
        }
        used
    }

    fn directive(&mut self, line: &str) {
        let (name, args) = line.split_once(' ').unwrap_or((line, ""));
        let args: Vec<&str> = args.split(',').map(str::trim).collect();
        match name {
            ".cfi_def_cfa" => self.cfa = (args[0].to_string(), imm(args[1])),
            ".cfi_def_cfa_offset" => self.cfa.1 = imm(args[0]),
            ".cfi_def_cfa_register" => self.cfa.0 = args[0].to_string(),
            ".cfi_offset" => {
                self.saved.insert(args[0].to_string());
            }
            ".cfi_restore" => {
                self.saved.remove(args[0]);
            }
            ".cfi_remember_state" => {
                self.stack
                    .push((self.cfa.clone(), self.saved.clone(), self.sp, self.fp))
            }
            ".cfi_restore_state" => {
                let Some(s) = self.stack.pop() else {
                    self.fail(line, "restore_state with nothing remembered")
                };
                (self.cfa, self.saved, self.sp, self.fp) = s;
            }
            n if n.starts_with(".cfi_") => self.fail(line, "unexpected CFI directive"),
            _ => {}
        }
    }

    /// The CFA rule names the CFA here.
    fn check_boundary(&self, line: &str) {
        let (sp, fp) = if self.x86 {
            ("%rsp", "%rbp")
        } else {
            ("sp", "x29")
        };
        let known = if self.cfa.0 == sp {
            self.sp
        } else if self.cfa.0 == fp {
            self.fp
        } else {
            self.fail(line, &format!("CFA in {}", self.cfa.0))
        };
        if known != Some(self.cfa.1) {
            self.fail(
                line,
                &format!(
                    "CFA rule is {}+{} but sp is CFA-{:?} and fp is CFA-{:?}",
                    self.cfa.0, self.cfa.1, self.sp, self.fp
                ),
            );
        }
    }

    fn instruction(&mut self, line: &str, needed: &HashSet<String>) {
        self.check_boundary(line);
        let (op, ops) = line.split_once(' ').unwrap_or((line, ""));
        let ops = ops.trim();
        if matches!(op, "bl" | "blr" | "call" | "callq") {
            self.calls += 1;
            let ra: &[&str] = if self.x86 { &["%rbp"] } else { &["x29", "x30"] };
            for r in ra
                .iter()
                .map(|r| r.to_string())
                .chain(needed.iter().cloned())
            {
                if !self.saved.contains(&r) {
                    self.fail(line, &format!("no rule for {r} across a call"));
                }
            }
        }
        if matches!(op, "ret" | "retq") {
            self.rets += 1;
            if self.sp != Some(if self.x86 { 8 } else { 0 }) {
                self.fail(line, "stack not released at return");
            }
            if !self.saved.is_empty() {
                self.fail(line, &format!("still described as saved: {:?}", self.saved));
            }
        }
        if self.x86 {
            self.x86_effect(line, op, ops);
        } else {
            self.a64_effect(line, op, ops);
        }
    }

    fn a64_effect(&mut self, line: &str, op: &str, ops: &str) {
        let add = |v: Option<i64>, d: i64| v.map(|v| v + d);
        match (op, ops) {
            ("stp", o) if o.starts_with("x29, x30, [sp, #-") && o.ends_with("]!") => {
                let n = imm(&o["x29, x30, [sp, #-".len()..o.len() - 2]);
                self.sp = add(self.sp, n);
            }
            ("ldp", o) if o.starts_with("x29, x30, [sp], #") => {
                self.sp = add(self.sp, -imm(&o["x29, x30, [sp], #".len()..]));
                self.fp = None;
            }
            ("sub", o) if o.starts_with("sp, sp, #") => {
                self.sp = add(self.sp, imm(&o["sp, sp, ".len()..]));
            }
            ("add", o) if o.starts_with("sp, sp, #") => {
                self.sp = add(self.sp, -imm(&o["sp, sp, ".len()..]));
            }
            ("mov", "x29, sp") => self.fp = self.sp,
            ("mov", "sp, x29") => self.sp = self.fp,
            _ if ops.contains("[sp") && (ops.ends_with("]!") || ops.contains("], #")) => {
                self.fail(line, "unrecognized stack pointer write-back")
            }
            // A store names its destination last; anything else, first.
            _ if op.starts_with("st") => {}
            (_, o) if o.starts_with("sp,") => self.sp = None,
            (_, o) if o.starts_with("x29,") => self.fp = None,
            _ => {}
        }
    }

    fn x86_effect(&mut self, line: &str, op: &str, ops: &str) {
        let add = |v: Option<i64>, d: i64| v.map(|v| v + d);
        match (op, ops) {
            ("pushq", _) => self.sp = add(self.sp, 8),
            ("popq", o) => {
                self.sp = add(self.sp, -8);
                if o == "%rbp" {
                    self.fp = None;
                }
            }
            ("movq", "%rsp, %rbp") => self.fp = self.sp,
            ("movq", "%rbp, %rsp") => self.sp = self.fp,
            ("subq", o) if o.ends_with(", %rsp") && o.starts_with('$') => {
                self.sp = add(self.sp, imm(&o[..o.len() - ", %rsp".len()]));
            }
            ("addq", o) if o.ends_with(", %rsp") && o.starts_with('$') => {
                self.sp = add(self.sp, -imm(&o[..o.len() - ", %rsp".len()]));
            }
            ("leaq", o) if o.ends_with("(%rbp), %rsp") => {
                let n: i64 = o[..o.len() - "(%rbp), %rsp".len()].parse().expect("lea");
                self.sp = add(self.fp, -n);
            }
            (_, o) if o.ends_with(", %rsp") => self.sp = None,
            (_, o) if o.ends_with(", %rbp") => self.fp = None,
            ("leave", _) => self.fail(line, "leave is not modelled"),
            _ => {}
        }
    }

    fn walk(mut self) {
        let needed = self.callee_saved_used();
        let mut started = false;
        for line in self.body.lines().map(str::trim) {
            if line == ".cfi_startproc" {
                started = true;
                continue;
            }
            if !started || line.is_empty() || line.ends_with(':') {
                continue;
            }
            if line.starts_with('.') {
                self.directive(line);
            } else {
                self.instruction(line, &needed);
            }
        }
        assert!(started, "{}: no .cfi_startproc:\n{}", self.func, self.body);
        assert!(
            self.calls > 0 && self.rets > 0,
            "{}: the walk saw {} calls and {} returns:\n{}",
            self.func,
            self.calls,
            self.rets,
            self.body
        );
    }
}

/// The CFI describes the CFA and every saved register at every instruction
/// boundary of every frame shape, with and without `-g`, for ELF and Mach-O.
#[test]
fn codegen_cfi_describes_every_frame() {
    let src = format!("{FRAMES}{DEPTH_STUB}");
    let funcs = ["pressure", "dyn", "big", "mid", "aligned", "varargs"];
    for triple in [
        AARCH64_LINUX,
        AARCH64_DARWIN,
        X86_64_LINUX,
        "x86_64-apple-darwin",
    ] {
        for opts in [&["-O0"][..], &["-O2"], &["-O2", "-g"]] {
            let asm = asm_for_with("cfi_walk", triple, &src, opts);
            for f in funcs {
                let name = format!("{f} ({triple} {opts:?})");
                CfiWalk::new(triple.starts_with("x86_64"), &name, body_of(&asm, f)).walk();
            }
        }
    }
}

/// `--fno-unwind-tables` drops the rules along with the procedure brackets.
#[test]
fn codegen_cfi_off_means_no_rules() {
    let src = format!("{FRAMES}{DEPTH_STUB}");
    for triple in [AARCH64_LINUX, X86_64_LINUX] {
        let asm = asm_for_with("cfi_off", triple, &src, &["-O2", "--fno-unwind-tables"]);
        assert!(!asm.contains(".cfi_"), "{triple}:\n{asm}");
    }
}
