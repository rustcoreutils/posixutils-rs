//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Where `-fstack-protector`'s canary sits in the frame, on both targets: above
// every array and every other slot the body addresses -- spills, shared
// slots, over-aligned locals -- and below everything the epilogue reloads,
// so a linear overrun upwards reaches it first.
//

use super::asm_probe::{asm_for_with, body_of, AARCH64_LINUX, X86_64_LINUX};

/// Sixteen values live across calls, more than either target has
/// callee-saved registers for, so some are spilled; two arrays in sibling
/// scopes, which may share a slot; an `int` array and a scalar whose
/// address escapes.
const SPILLS_AND_SHARING: &str = r#"
extern void use(void *);
extern long g(long);
long f(int i, int k) {
    long a = g(i), b = g(a + 1), c = g(b + 2), d = g(c + 3), e = g(d + 4);
    long h = g(e + 5), m = g(h + 6), q = g(m + 7), r = g(q + 8), s = g(r + 9);
    long t = g(s + 10), u = g(t + 11), v = g(u + 12), w = g(v + 13);
    long y = g(w + 14), z = g(y + 15);
    long x = i;
    { char s1[24]; s1[k & 15] = 1; use(s1); }
    { char s2[24]; s2[k & 15] = 2; use(s2); }
    int arr[6]; arr[k & 3] = 4; use(arr); use(&x);
    return a + b + c + d + e + h + m + q + r + s + t + u + v + w + y + z + x + arr[0];
}
"#;

/// An over-aligned array, which puts the frame on a second base register.
const OVER_ALIGNED: &str = r#"
extern void use(void *);
int f(int i) {
    int x = i;
    _Alignas(64) char big[64];
    char small[16];
    big[i] = 1; small[i] = 2;
    use(big); use(small); use(&x);
    return big[0] + small[0] + x;
}
"#;

/// A frame address: the base register's name and the displacement.
type Addr = (String, i32);

/// The comment introducer `-fverbose-asm` uses on `triple`.
fn comment(triple: &str) -> &'static str {
    if triple.starts_with("aarch64") {
        "//"
    } else {
        "#"
    }
}

/// The frame address `line` names, if any: x86-64's `N(%reg)` or
/// aarch64's `[reg, #N]` and `add xD, reg, #N`.
fn frame_addr(line: &str) -> Option<Addr> {
    // Without the comment: aarch64's `//`, or x86-64's `#`, which on aarch64
    // starts an immediate.
    let line = line.split(" //").next().unwrap();
    let line = if line.contains('%') {
        line.split(" #").next().unwrap()
    } else {
        line
    };
    if let Some(open) = line.find("(%") {
        let close = open + line[open..].find(')')?;
        let start = line[..open].rfind([' ', ',']).map_or(0, |s| s + 1);
        let disp = line[start..open].parse().unwrap_or(0);
        return Some((line[open + 1..close].to_string(), disp));
    }
    if let Some(open) = line.find('[') {
        let inner = &line[open + 1..line.find(']')?];
        let mut parts = inner.split(", #");
        let base = parts.next()?.to_string();
        let disp = parts.next().map_or(Some(0), |d| d.parse().ok())?;
        return Some((base, disp));
    }
    let ops: Vec<&str> = line.trim().strip_prefix("add ")?.split(", ").collect();
    match ops.as_slice() {
        [_, base, imm] if imm.starts_with('#') => Some((base.to_string(), imm[1..].parse().ok()?)),
        _ => None,
    }
}

/// The canary's frame address in `f`: where the prologue stores the guard.
fn canary(body: &str) -> Addr {
    let lines: Vec<&str> = body.lines().map(str::trim).collect();
    let load = lines
        .iter()
        .position(|l| l.starts_with("movq %fs:40, %r11") || l.starts_with("ldr x16, [x16]"))
        .unwrap_or_else(|| panic!("no guard load:\n{body}"));
    frame_addr(lines[load + 1]).unwrap_or_else(|| panic!("no canary store:\n{body}"))
}

/// The frame address of the local `name`, from `-fverbose-asm`'s comment.
fn local(body: &str, triple: &str, name: &str) -> Addr {
    let tag = format!("{} {name}.", comment(triple));
    body.lines()
        .filter(|l| l.contains(&tag))
        .find_map(frame_addr)
        .unwrap_or_else(|| panic!("{triple}: no address of {name}:\n{body}"))
}

/// Where the unwinder -- and the epilogue -- finds each register `f` saves
/// on aarch64, as a displacement from x29: the prologue's last
/// `.cfi_offset` for it, from a CFA the frame's size above x29.
fn aarch64_saves(body: &str, why: &str) -> std::collections::BTreeMap<String, i32> {
    let prologue = &body[..body.find("\n.Lf_").unwrap_or(body.len())];
    let size: i32 = prologue
        .lines()
        .rev()
        .find_map(|l| l.trim().strip_prefix(".cfi_def_cfa_offset "))
        .and_then(|n| n.parse().ok())
        .unwrap_or_else(|| panic!("{why}: no frame size\n{body}"));
    let mut saved = std::collections::BTreeMap::new();
    for l in prologue.lines() {
        if let Some(rule) = l.trim().strip_prefix(".cfi_offset ") {
            let (reg, off) = rule.split_once(", ").unwrap();
            saved.insert(reg.to_string(), size + off.parse::<i32>().unwrap());
        }
    }
    saved
}

/// Assert the canary of `f` in `asm` lies above each of `arrays` (name and
/// size) and above every other slot the body addresses off the same base,
/// and, on aarch64, below the callee-saved registers and the frame record
/// the epilogue reloads.
fn assert_layout(asm: &str, triple: &str, arrays: &[(&str, i32)], why: &str) {
    let body = body_of(asm, "f");
    let (base, top) = canary(body);
    for &(name, size) in arrays {
        let (b, at) = local(body, triple, name);
        assert_eq!(b, base, "{why}: {name} off another base\n{body}");
        assert!(
            at + size <= top,
            "{why}: {name} at {at} reaches the canary at {top}\n{body}"
        );
    }
    let saved = if triple.starts_with("aarch64") {
        aarch64_saves(body, why)
    } else {
        Default::default()
    };
    for l in body.lines().map(str::trim) {
        // The epilogue's `leaq` that points `%rsp` at the saved registers
        // addresses no object, and neither do the register saves.
        if l.ends_with("%rsp") || l.starts_with("stp x29, x30") {
            continue;
        }
        if let Some((b, at)) = frame_addr(l) {
            let a_save = b == "x29" && saved.values().any(|&s| s == at || s == at + 8);
            if b == base && at != top && !a_save {
                assert!(at < top, "{why}: `{l}` above the canary at {top}\n{body}");
            }
        }
    }
    if triple.starts_with("aarch64") {
        // Every register the epilogue reloads is saved above the canary --
        // x29 and x30 too: the record at `[sp]` is the frame chain's, which
        // the epilogue does not reload. An over-aligned frame's canary is
        // off x19, which the prologue sets to `x29 + K` rounded down: at
        // most K above x29.
        let x29_top = if base == "x29" {
            top
        } else {
            let latch = format!("add {base}, x29, #");
            let k: i32 = body
                .lines()
                .find_map(|l| l.trim().strip_prefix(latch.as_str()))
                .and_then(|k| k.split_whitespace().next()?.parse().ok())
                .unwrap_or_else(|| panic!("{why}: no base latch\n{body}"));
            k + top
        };
        assert!(saved.contains_key("x30"), "{why}: no x30 rule\n{body}");
        for (reg, at) in saved {
            assert!(
                at >= x29_top + 8,
                "{why}: {reg} saved at x29+{at}, below the canary at x29+{x29_top}\n{body}"
            );
        }
        assert!(
            !body.contains("ldp x29, x30, [sp], #"),
            "{why}: the frame record is reloaded from below the locals\n{body}"
        );
    } else if base == "%rbp" {
        // Under the pushes, which are the 8-byte words below `%rbp`.
        let pushes = body
            .lines()
            .filter(|l| l.trim().starts_with("pushq"))
            .count() as i32
            - 1;
        assert!(
            top + 8 <= -8 * pushes,
            "{why}: canary among the saved registers\n{body}"
        );
    }
}

#[test]
fn stack_protector_canary_above_spills_and_shared_slots() {
    for triple in [X86_64_LINUX, AARCH64_LINUX] {
        for opt in ["-O0", "-O2"] {
            let flags = ["-fstack-protector-strong", "-fverbose-asm", opt];
            let asm = asm_for_with("ssp_layout", triple, SPILLS_AND_SHARING, &flags);
            let why = format!("{triple} {opt}");
            assert_layout(&asm, triple, &[("s1", 24), ("s2", 24), ("arr", 24)], &why);
            // Right under the canary, the first `char` array: no spilled
            // argument, no scalar, between it and the canary.
            let body = body_of(&asm, "f");
            let (_, top) = canary(body);
            let (_, s1) = local(body, triple, "s1");
            assert_eq!(s1 + 24, top, "{why}: s1 not right under the canary\n{body}");
        }
    }
}

#[test]
fn stack_protector_canary_above_over_aligned_locals() {
    for triple in [X86_64_LINUX, AARCH64_LINUX] {
        for opt in ["-O0", "-O2"] {
            let flags = ["-fstack-protector-strong", "-fverbose-asm", opt];
            let asm = asm_for_with("ssp_layout_al", triple, OVER_ALIGNED, &flags);
            let why = format!("{triple} {opt}");
            assert_layout(&asm, triple, &[("big", 64), ("small", 16)], &why);
        }
    }
}

/// A function that allocates at run time gets the same layout on aarch64:
/// the area `alloca` carves sits right under the frame record at `[x29]`,
/// so the record the epilogue reloads, and the callee-saved registers, are
/// kept above the canary instead.
#[test]
fn stack_protector_aarch64_saves_above_the_canary_with_alloca() {
    let src = r#"
extern void use(void *);
extern long g(long);
long f(int n) {
    char buf[n];
    char fixed[16];
    long a = g(n), b = g(a), c = g(b);
    use(buf); use(fixed);
    return a + b + c + fixed[0];
}
"#;
    for opt in ["-O0", "-O2"] {
        let flags = ["-fstack-protector", "-fverbose-asm", opt];
        let asm = asm_for_with("ssp_layout_vla", AARCH64_LINUX, src, &flags);
        assert_layout(&asm, AARCH64_LINUX, &[("fixed", 16)], opt);
    }
}
