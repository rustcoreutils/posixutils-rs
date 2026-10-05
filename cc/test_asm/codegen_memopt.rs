//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Assembly cases of tests/codegen/memopt.rs, in process: what the memory
// analyses must keep -- volatile reads and stores, volatile members and
// bit-fields, composite arguments at their own width -- and the plain
// controls they may still remove.
//

use crate::test_asm::asm_probe::{
    asm_for_with, assert_body_contains, assert_body_lacks, body_of, count_in_body, AARCH64_LINUX,
    X86_64_LINUX,
};

/// An aggregate with a `volatile` member is re-read for each copy of it.
///
/// C17 6.7.3p7: an object with volatile-qualified type may change in ways
/// the implementation cannot see, so every access to it happens as written.
/// A qualifier on a *member* makes that member's storage volatile, but the
/// struct holding it is not itself volatile-qualified -- and the struct's own
/// modifiers, plus the access type, were all `forwardable` asked. Once the
/// copy is expanded into loads and stores those accesses are plain integers,
/// so `t = s; u = s;` loaded `s` once and fed both copies from it.
///
/// The unqualified struct beside it is the control: forwarding *is* right
/// there, and clang does it -- 4 loads of the volatile object against 2 of
/// the plain one. The assertion is relative for that reason, rather than
/// pinning an instruction count that codegen may fairly change.
#[test]
fn codegen_volatile_member_is_reread_for_each_aggregate_copy() {
    let src = r#"
struct V { volatile int v; int pad[7]; };
struct P { int v; int pad[7]; };
struct V vs, vt, vu;
struct P ps, pt, pu;
void fv(void) { vt = vs; vu = vs; }
void fp(void) { pt = ps; pu = ps; }
"#;
    for triple in [X86_64_LINUX, AARCH64_LINUX] {
        let asm = asm_for_with("vol_member_copy", triple, src, &["-O2"]);
        // A load is an instruction whose *source* operand is memory.
        let loads = |func: &str| -> usize {
            body_of(&asm, func)
                .lines()
                .map(str::trim)
                .filter(|l| {
                    if triple == X86_64_LINUX {
                        l.starts_with("mov")
                            && l.split_once(char::is_whitespace).is_some_and(|(_, ops)| {
                                ops.split(',').next().is_some_and(|src| src.contains('('))
                            })
                    } else {
                        l.starts_with("ldr ") || l.starts_with("ldp ")
                    }
                })
                .count()
        };
        let (volatile, plain) = (loads("fv"), loads("fp"));
        assert!(
            volatile > plain,
            "{triple}: the volatile member must be re-read for the second \
             copy -- volatile {volatile} loads, plain {plain}:\n{}",
            body_of(&asm, "fv")
        );
    }
}

/// A store into an aggregate with a `volatile` member is not deleted by a
/// later store that covers it.
///
/// The mirror of the load case: `dse::deletable` asked the same three
/// questions `loadfwd::forwardable` did -- the access type, which an expanded
/// aggregate copy makes a plain integer, and the object's own modifiers, which
/// a `struct` holding a volatile member does not carry -- so `s = x; s = y;`
/// let the first copy's stores go, dropping a write to the volatile member
/// that C17 6.7.3p7 says must happen.
///
/// The unqualified struct beside it is the control: deleting the dead store
/// *is* right there, so the fix cannot pass by giving up on every aggregate.
#[test]
fn codegen_volatile_member_keeps_a_store_a_later_one_covers() {
    let src = r#"
struct V { volatile int v; int pad[7]; };
struct P { int v; int pad[7]; };
struct V vs, vx, vy;
struct P ps, px, py;
void fv(void) { vs = vx; vs = vy; }
void fp(void) { ps = px; ps = py; }
"#;
    for triple in [X86_64_LINUX, AARCH64_LINUX] {
        let asm = asm_for_with("vol_member_dse", triple, src, &["-O2"]);
        let stores = |func: &str| -> usize {
            body_of(&asm, func)
                .lines()
                .map(str::trim)
                .filter(|l| {
                    if triple == X86_64_LINUX {
                        // `movq %rdx, 8(%rax)` -- a memory *destination*.
                        l.starts_with("mov")
                            && l.split_once(char::is_whitespace).is_some_and(|(_, ops)| {
                                ops.rsplit(',').next().is_some_and(|d| d.contains('('))
                            })
                    } else {
                        l.starts_with("str ") || l.starts_with("stp ")
                    }
                })
                .count()
        };
        let (volatile, plain) = (stores("fv"), stores("fp"));
        assert!(
            volatile > plain,
            "{triple}: the store to a volatile member survives a later copy \
             (volatile {volatile} stores, plain {plain}):\n{}",
            body_of(&asm, "fv")
        );
    }
}

/// The widest load in `body` that reads through a pointer, in bytes, or 0.
///
/// Frame-relative accesses are skipped: a spill or a stack temporary is the
/// compiler's own storage, and its width says nothing about the object.
fn widest_object_load(body: &str, aarch64: bool) -> (u32, String) {
    let mut widest = 0;
    let mut at = String::new();
    for line in body.lines() {
        let text = line.trim();
        let (mnemonic, operands) = match text.split_once(char::is_whitespace) {
            Some(pair) => pair,
            None => continue,
        };
        let width = if aarch64 {
            if operands.contains("[sp") || operands.contains("[x29") {
                continue;
            }
            match mnemonic {
                "ldrb" | "ldrsb" => 1,
                "ldrh" | "ldrsh" => 2,
                "ldr" | "ldrsw" if operands.starts_with('w') => 4,
                "ldr" if operands.starts_with('x') => 8,
                "ldp" if operands.starts_with('x') => 16,
                "ldp" if operands.starts_with('w') => 8,
                _ => continue,
            }
        } else {
            // The source is the first operand; it must be a memory reference
            // through something other than the frame.
            let src = operands.split(',').next().unwrap_or("").trim();
            if !src.contains("(%r") || src.contains("%rbp)") || src.contains("%rsp)") {
                continue;
            }
            match mnemonic {
                "movb" | "movzbl" | "movsbl" | "movzbq" | "movsbq" => 1,
                "movw" | "movzwl" | "movswl" | "movzwq" | "movswq" => 2,
                "movl" | "movslq" => 4,
                "movq" => 8,
                _ => continue,
            }
        };
        if width > widest {
            widest = width;
            at = text.to_string();
        }
    }
    (widest, at)
}

/// No composite is read wider than it is, on either target.
///
/// The runtime tests above execute, so they only ever cover the host --
/// x86-64 here. This one compiles for both triples and reads the widths out
/// of the assembly, which is what covers the aarch64 lowering on a machine
/// that cannot run it. A ragged size is read as two overlapping halves, so
/// the widest access is the half, never the object rounded up.
#[test]
fn codegen_no_composite_is_read_wider_than_itself() {
    // (bytes in the struct, the widest load its value may be read with)
    let cases: &[(usize, u32)] = &[
        (1, 1),
        (2, 2),
        // The ragged sizes: 3 is two halves of 2, and 5, 6 and 7 are two of 4.
        (3, 2),
        (5, 4),
        (6, 4),
        (7, 4),
        // The controls, each already one natural access.
        (4, 4),
        (8, 8),
    ];
    for &(bytes, want) in cases {
        let src = format!(
            "struct S {{ char a[{bytes}]; }};\n\
             int g(struct S s);\n\
             int f(struct S *p) {{ return g(*p); }}\n"
        );
        for (triple, is_a64) in [(X86_64_LINUX, false), (AARCH64_LINUX, true)] {
            let asm = asm_for_with(&format!("widest_{bytes}"), triple, &src, &["-O1"]);
            let body = body_of(&asm, "f");
            let (got, line) = widest_object_load(body, is_a64);
            assert_eq!(
                got, want,
                "{triple}: a {bytes}-byte struct is read with a {got}-byte \
                 access (`{line}`), not {want} -- anything wider reaches past \
                 the object:\n{body}"
            );
        }
    }
}

/// Reading a `volatile` object is an observable side effect, so the access
/// survives every optimization level -- including a read whose value is
/// discarded, which no data-flow fact keeps alive (C17 5.1.2.3).
///
/// The property is on the access, not on the result, so a discarded read has
/// nothing an exit status can see. The check is on the emitted instruction,
/// against both targets, because the rule is architecture-independent.
#[test]
fn memopt_a_discarded_volatile_read_is_still_performed() {
    // The object names are deliberately unmistakable. A single letter is not a
    // sound needle here: every x86-64 body contains `pushq`/`popq` and every
    // aarch64 body contains `stp`/`sp`, and `.cfi_startproc` is inside the
    // range `body_of` returns -- so searching for "p" passes against a body
    // that was emptied, which is exactly the defect. (No empty body on either
    // target contains a "g", which is why the other cases were sound.)
    let cases = [
        (
            "assign",
            "volatile int volobj;\nvoid probe(void) { int a = volobj; (void)a; }\n",
            "volobj",
        ),
        (
            "discard",
            "volatile int volobj;\nvoid probe(void) { volobj; }\n",
            "volobj",
        ),
        (
            "via_ptr",
            "volatile int *volptr;\nvoid probe(void) { *volptr; }\n",
            "volptr",
        ),
        (
            "cast_void",
            "volatile int volobj;\nvoid probe(void) { (void)volobj; }\n",
            "volobj",
        ),
    ];

    for (tag, src, object) in cases {
        for level in ["-O0", "-O1", "-O2", "-Os"] {
            for triple in [X86_64_LINUX, AARCH64_LINUX] {
                let asm = asm_for_with(&format!("vol_{tag}"), triple, src, &[level]);
                assert_body_contains(
                    &asm,
                    "probe",
                    object,
                    &format!(
                        "a volatile read is observable: `{tag}` at {level} on {triple} \
                         must still access `{object}`"
                    ),
                );

                // Naming the pointer is not the same as dereferencing it, and
                // the qualifier here is on the pointee, so the load through it
                // is the access under test.
                if tag == "via_ptr" {
                    let indirect = if triple == X86_64_LINUX { "(%r" } else { "[x" };
                    assert_body_contains(
                        &asm,
                        "probe",
                        indirect,
                        &format!(
                            "the volatile pointee is read, not just the pointer: \
                             {level} on {triple}"
                        ),
                    );
                }
            }
        }
    }
}

/// The counterpart that keeps the fix above honest: an *ordinary* discarded
/// read is still dead code, and DCE still deletes it.
///
/// Without this, marking every load a root would pass the volatile test.
#[test]
fn memopt_a_discarded_plain_read_is_still_removed() {
    // Object names chosen to occur in no mnemonic, register or label the
    // body can otherwise contain -- `popq` alone contains both `p` and `pq`,
    // and the body a negative assertion searches includes the function's own
    // label and prologue.
    let cases = [
        (
            "assign",
            "int objx;\nvoid probe(void) { int a = objx; (void)a; }\n",
            "objx",
        ),
        ("discard", "int objx;\nvoid probe(void) { objx; }\n", "objx"),
        (
            "via_ptr",
            "int *ptrx;\nvoid probe(void) { *ptrx; }\n",
            "ptrx",
        ),
    ];

    for (tag, src, object) in cases {
        for triple in [X86_64_LINUX, AARCH64_LINUX] {
            let asm = asm_for_with(&format!("plain_{tag}"), triple, src, &["-O2"]);
            assert_body_lacks(
                &asm,
                "probe",
                object,
                &format!(
                    "reading a non-volatile object has no effect: `{tag}` on {triple} \
                     must not access `{object}`"
                ),
            );
        }
    }
}

/// Each read of a `volatile` object is its own observable event, so two of
/// them are two accesses -- neither load-forwarding nor DCE may fold the pair
/// into one.
#[test]
fn memopt_two_volatile_reads_are_both_performed() {
    // A named object only: two reads through one `volatile int *p` show up as
    // a *single* reference to `p` -- reading the pointer itself is not
    // volatile and is rightly done once -- so the count says nothing there.
    // The through-pointer case is pinned at the IR level instead, by
    // `test_volatile_accesses_carry_the_marker` and the `dce` unit tests.
    //
    // The name occurs in no mnemonic, register or label the body can
    // otherwise contain: `popq` alone contains both `p` and `pq`.
    let cases = [(
        "named",
        "volatile int objx;\nint sink(int, int);\n\
         int probe(void) { int a = objx; int b = objx; return sink(a, b); }\n",
        "objx",
    )];

    for (tag, src, object) in cases {
        for level in ["-O1", "-O2", "-Os"] {
            for triple in [X86_64_LINUX, AARCH64_LINUX] {
                let asm = asm_for_with(&format!("vol_two_reads_{tag}"), triple, src, &[level]);
                let n = count_in_body(&asm, "probe", object);
                assert!(
                    n >= 2,
                    "both volatile reads are observable: `{tag}` at {level} on {triple} \
                     kept {n} reference(s) to `{object}`:\n{}",
                    body_of(&asm, "probe")
                );
            }
        }
    }
}

/// An `_Atomic` read is observable for the same reason, and reaches DCE by a
/// different route: `AtomicLoad` is a side-effecting opcode outright, so this
/// cross-checks that the two spellings of "this read must happen" agree.
#[test]
fn memopt_a_discarded_atomic_read_is_still_performed() {
    let cases = [
        ("discard", "_Atomic int g;\nvoid probe(void) { g; }\n", "g"),
        (
            "assign",
            "_Atomic int g;\nvoid probe(void) { int a = g; (void)a; }\n",
            "g",
        ),
    ];

    for (tag, src, object) in cases {
        for level in ["-O0", "-O2"] {
            for triple in [X86_64_LINUX, AARCH64_LINUX] {
                let asm = asm_for_with(&format!("atomic_{tag}"), triple, src, &[level]);
                assert_body_contains(
                    &asm,
                    "probe",
                    object,
                    &format!(
                        "an atomic read is observable: `{tag}` at {level} on {triple} \
                         must still access `{object}`"
                    ),
                );
            }
        }
    }
}

/// A `volatile` store is observable for the same reason, and DSE must not drop
/// the earlier of two writes to one.
///
/// The companion to the read case above: a test that only checked loads would
/// pass against an `has_side_effects` that named `Store` and not `Load`.
#[test]
fn memopt_two_volatile_stores_are_both_performed() {
    let src = "volatile int g;\nvoid probe(void) { g = 1; g = 2; }\n";
    for level in ["-O1", "-O2", "-Os"] {
        for triple in [X86_64_LINUX, AARCH64_LINUX] {
            let asm = asm_for_with("vol_two_stores", triple, src, &[level]);
            let n = count_in_body(&asm, "probe", "g");
            assert!(
                n >= 2,
                "both volatile stores are observable: {level} on {triple} kept {n} \
                 reference(s) to `g`:\n{}",
                body_of(&asm, "probe")
            );
        }
    }
}

/// A member of a `volatile` object is itself volatile, so reading it is an
/// observable event that survives every optimization level.
///
/// C17 6.5.2.3p3/p4: the result of `s.m` has the *so-qualified* version of the
/// member's type — it inherits the qualifiers of the object. c17 took the
/// member's declared type unchanged, so a member of a `volatile` struct read as
/// an ordinary `int` and DCE deleted it from `-O1` up. The reverse direction
/// (`struct T { volatile int a; }`) always worked, because there the member's
/// own type carries the qualifier; that case is the control below.
#[test]
fn memopt_a_member_of_a_volatile_object_is_volatile() {
    // Distinctive names: a single letter matches `pushq`/`stp`/`.cfi_startproc`
    // inside the body range and would pass against an emptied function.
    let src = "\
struct S { int a; int b; };
volatile struct S vqobj;
volatile struct S *vqptr;
void probe_direct(void) { vqobj.a; }
void probe_arrow(void) { vqptr->a; }
void probe_assign(void) { int t = vqobj.a; (void)t; }
void probe_second(void) { vqobj.b; }
";
    for level in ["-O0", "-O1", "-O2", "-Os"] {
        for triple in [X86_64_LINUX, AARCH64_LINUX] {
            let asm = asm_for_with("vol_member", triple, src, &[level]);
            for (func, object) in [
                ("probe_direct", "vqobj"),
                ("probe_arrow", "vqptr"),
                ("probe_assign", "vqobj"),
                ("probe_second", "vqobj"),
            ] {
                assert_body_contains(
                    &asm,
                    func,
                    object,
                    &format!(
                        "a member of a volatile object is volatile (C17 6.5.2.3p3): \
                         {func} at {level} on {triple} must still access `{object}`"
                    ),
                );
            }
        }
    }
}

/// A copy out of a `volatile` aggregate reads it, even when nothing uses the
/// copy (C17 5.1.2.3p6).
///
/// A struct too wide for one register is copied in integer chunks, and the
/// chunks carried no volatile marker, so `struct S t = vstructobj;` with `t`
/// unused lost every read from `-O1` up on both targets. The copy is of a
/// named global so that the object's name in the body is the access itself.
#[test]
fn memopt_a_copy_out_of_a_volatile_aggregate_is_performed() {
    let src = "\
struct S { int a, b, c; };
volatile struct S vstructobj;
struct { volatile struct { int a; }; int b; } vanonobj;
void probe_copy(void) { struct S t = vstructobj; (void)t; }
void probe_anon(void) { vanonobj.a; }
";
    for level in ["-O0", "-O1", "-O2"] {
        for triple in [X86_64_LINUX, AARCH64_LINUX] {
            let asm = asm_for_with("vol_copy", triple, src, &[level]);
            for (func, object) in [("probe_copy", "vstructobj"), ("probe_anon", "vanonobj")] {
                assert_body_contains(
                    &asm,
                    func,
                    object,
                    &format!("{func} at {level} on {triple} must still read `{object}`"),
                );
            }
        }
    }
}

/// The control for the test above: an ordinary aggregate's member read is still
/// deleted, so that test cannot pass by marking every member access volatile.
#[test]
fn memopt_a_member_of_a_plain_object_is_still_removed() {
    let src = "\
struct S { int a; };
struct S pqobj;
void probe(void) { pqobj.a; }
";
    for triple in [X86_64_LINUX, AARCH64_LINUX] {
        let asm = asm_for_with("plain_member", triple, src, &["-O2"]);
        assert_body_lacks(
            &asm,
            "probe",
            "pqobj",
            "reading an ordinary member has no effect and is dead code",
        );
    }
}

/// A `volatile` member is not speculatable, so a conditional must not read the
/// arm it did not take.
///
/// C17 6.5.15p4 evaluates only one of the second and third operands, and
/// 5.1.2.3 makes each volatile read an observable event. `is_pure_expr`'s
/// `Member` arm asked only whether the *base* was pure, so the read was
/// hoisted and both members were loaded unconditionally into a branchless
/// select — at `-O0` too.
#[test]
fn memopt_a_volatile_member_is_not_speculated_by_a_conditional() {
    let src = "\
struct S { volatile unsigned status; unsigned other; };
struct S sqobj;
unsigned probe(int c) { return c ? sqobj.status : sqobj.other; }
";
    for level in ["-O0", "-O2"] {
        for triple in [X86_64_LINUX, AARCH64_LINUX] {
            let asm = asm_for_with("vol_member_select", triple, src, &[level]);
            let select = if triple == X86_64_LINUX {
                "cmov"
            } else {
                "csel"
            };
            assert_body_lacks(
                &asm,
                "probe",
                select,
                &format!(
                    "a volatile member read cannot be speculated, so the arms may not \
                     collapse into a conditional move: {level} on {triple}"
                ),
            );
        }
    }
}

/// The control for the test above: with no volatile member, the branchless
/// select is still allowed, so that test is asserting the qualifier and not
/// merely that c17 stopped emitting conditional moves.
#[test]
fn memopt_a_plain_member_may_still_be_speculated() {
    let src = "\
struct S { unsigned one; unsigned other; };
struct S pqsel;
unsigned probe(int c) { return c ? pqsel.one : pqsel.other; }
";
    for triple in [X86_64_LINUX, AARCH64_LINUX] {
        let asm = asm_for_with("plain_member_select", triple, src, &["-O2"]);
        let select = if triple == X86_64_LINUX {
            "cmov"
        } else {
            "csel"
        };
        assert_body_contains(
            &asm,
            "probe",
            select,
            "two ordinary member reads are pure and may still collapse to a select",
        );
    }
}

/// A `volatile` bit-field read is observable, even though the access is of the
/// carrier and the carrier can never carry the qualifier.
///
/// The bit-field emitters build their load and store at
/// `bitfield_storage_type`, which is the unqualified storage unit, so nothing
/// derived the marker from the access type. `mark_volatile_access` anticipates
/// exactly this ("a bit-field reads a storage unit whose type is the carrier")
/// and preserves a marker the site sets itself — neither emitter set one, and
/// the read was deleted outright from `-O1` up. Both spellings are covered: the
/// field declared `volatile`, and an ordinary field of a `volatile` object.
#[test]
fn memopt_a_volatile_bitfield_read_is_performed() {
    let src = "\
struct B { volatile unsigned f : 3; unsigned g : 5; };
struct B bfqobj;
volatile struct B vbfqobj;
void probe_field(void) { bfqobj.f; }
void probe_object(void) { vbfqobj.g; }
";
    for level in ["-O0", "-O1", "-O2", "-Os"] {
        for triple in [X86_64_LINUX, AARCH64_LINUX] {
            let asm = asm_for_with("vol_bitfield", triple, src, &[level]);
            for (func, object) in [("probe_field", "bfqobj"), ("probe_object", "vbfqobj")] {
                assert_body_contains(
                    &asm,
                    func,
                    object,
                    &format!(
                        "a volatile bit-field read is observable: {func} at {level} \
                         on {triple} must still access `{object}`"
                    ),
                );
            }
        }
    }
}

/// The control: an ordinary bit-field read is still dead code, so the test
/// above cannot pass by marking every bit-field access volatile.
#[test]
fn memopt_a_plain_bitfield_read_is_still_removed() {
    let src = "\
struct B { unsigned f : 3; };
struct B pbfqobj;
void probe(void) { pbfqobj.f; }
";
    for triple in [X86_64_LINUX, AARCH64_LINUX] {
        let asm = asm_for_with("plain_bitfield", triple, src, &["-O2"]);
        assert_body_lacks(
            &asm,
            "probe",
            "pbfqobj",
            "reading an ordinary bit-field has no effect and is dead code",
        );
    }
}
