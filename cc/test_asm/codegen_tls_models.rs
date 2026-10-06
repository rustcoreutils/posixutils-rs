//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Thread-local access models in the assembly: the Mach-O descriptor and
// getter shapes, and the ELF models FreeBSD shares with Linux. The linked and
// run halves are in `tests/codegen/tls_models.rs`.
//

use super::asm_probe::{asm_for_with, body_of};

/// A thread-local reached through a block-scope `extern` inside a function
/// whose parameter has the same name. The parameter's address is taken, so it
/// keeps a stack slot registered under the bare name `x`, and the global's
/// `Sym` carries that name too: telling the two apart by name took the
/// global for the parameter, so its access was never expanded. On aarch64
/// the descriptor call then clobbered `p` and `y` behind the allocator's
/// back, x86-64 fell back to Initial Exec inside a shared object, and at -O2
/// the backend's own check rejected the function.
/// Shared with `tests/codegen/tls_models.rs`, which links and runs it.
const SHADOW_LIB: &str = include_str!("../tests/codegen/tls_shadow_lib.c");

// ---------------------------------------------------------------------------
// Mach-O thread-local variables
// ---------------------------------------------------------------------------
//
// Darwin has no static TLS model. The symbol a program names is a
// three-word descriptor in `__DATA,__thread_vars` -- `{ __tlv_bootstrap, 0,
// image }` -- and every access calls the getter it holds. c17 emitted
// thread-locals as plain data and reached them as plain globals (`@PAGE`,
// `@GOTPAGE`, `(%rip)`), so every thread shared one copy, and an `extern`
// one did not link. Nothing here can assemble or run Mach-O, so the shape is
// pinned against clang's layout below, and
// [`tls_threads_have_their_own_copies`] is the run-time proof on a macOS host.

const DARWIN_AARCH64: &str = "aarch64-apple-darwin";
const DARWIN_X86_64: &str = "x86_64-apple-darwin";

/// Every definition is an image plus a descriptor, with linkage on the
/// descriptor: initialized data, zero-fill, a static, a weak one, an
/// over-aligned one.
#[test]
fn tls_macho_definitions_are_descriptors() {
    let src = r#"
_Thread_local int tv = 5;
_Thread_local long tz;
static _Thread_local int ts = 3;
__attribute__((weak)) _Thread_local int tw = 9;
_Alignas(64) _Thread_local char ta[64];
int use(void) { return tv + (int)tz + ts + tw + ta[0]; }
"#;
    for triple in [DARWIN_AARCH64, DARWIN_X86_64] {
        let asm = asm_for_with("tls_macho_defs", triple, src, &["-O2"]);
        let descriptor = |name: &str| {
            format!(
                ".p2align 3\n_{name}:\n    .quad __tlv_bootstrap\n    .quad 0\n    .quad _{name}$tlv$init\n"
            )
        };
        for name in ["tv", "tz", "ts", "tw", "ta"] {
            assert!(
                asm.contains(&descriptor(name)),
                "{triple}: no TLV descriptor for {name}:\n{asm}"
            );
        }
        assert!(
            asm.contains(
                ".section __DATA,__thread_data,thread_local_regular\n.p2align 2\n_tv$tlv$init:\n    .long 5\n"
            ),
            "{triple}: initialized image:\n{asm}"
        );
        assert!(
            asm.contains(".tbss _tz$tlv$init, 8, 3\n"),
            "{triple}: zero-fill image:\n{asm}"
        );
        assert!(
            asm.contains(".tbss _ta$tlv$init, 64, 6\n"),
            "{triple}: over-aligned image:\n{asm}"
        );
        assert!(
            asm.contains(".section __DATA,__thread_vars,thread_local_variables\n.globl _tv\n"),
            "{triple}: an external descriptor is global:\n{asm}"
        );
        assert!(
            asm.contains(".globl _tw\n.weak_definition _tw\n"),
            "{triple}: a weak descriptor:\n{asm}"
        );
        assert!(
            !asm.contains(".globl _ts\n") && !asm.contains("$tlv$init\n.globl"),
            "{triple}: a static descriptor and every image stay local:\n{asm}"
        );
    }
}

/// Every access form -- each width, `long double`, a struct copied both
/// ways, an array element, address-of, an `extern` and a weak thread-local,
/// and an inline-asm memory operand -- goes through the TLV getter, and no
/// thread-local is ever reached as plain data.
const MACHO_FORMS: &str = r#"
struct big { long a[5]; };
_Thread_local char tc = 1;
_Thread_local short tsh = 2;
_Thread_local long double tld = 3.5L;
_Thread_local float tf;
_Thread_local struct big tb;
_Thread_local int tarr[16];
__attribute__((weak)) _Thread_local int tw = 9;
static __thread int tstat;
extern _Thread_local double tex;
struct big copy_out(void) { return tb; }
void copy_in(struct big *p) { tb = *p; }
int elem(int i) { return tarr[i] + tarr[3]; }
void elem_set(int i, int v) { tarr[i] = v; tarr[7] = v; }
long double getld(void) { return tld; }
void setld(long double v) { tld = v; }
float getf(void) { return tf; }
int narrow(void) { return tc + tsh + tstat + tw; }
double getex(void) { return tex; }
int *arr_addr(void) { return &tarr[5]; }
double keep(double a, double b) { double s = a * b; tc = 1; return s + a; }
long asm_mem(void) { long out; __asm__("" : "=r"(out) : "m"(tarr[2])); return out; }
"#;

#[test]
fn tls_macho_every_access_calls_the_getter() {
    let names = ["tc", "tsh", "tld", "tf", "tb", "tarr", "tw", "tstat", "tex"];
    for triple in [DARWIN_AARCH64, DARWIN_X86_64] {
        for opt in ["-O0", "-O2"] {
            let asm = asm_for_with("tls_macho_forms", triple, MACHO_FORMS, &[opt]);
            for name in names {
                let sequence = if triple == DARWIN_AARCH64 {
                    format!(
                        "    adrp x0, _{name}@TLVPPAGE\n    ldr x0, [x0, _{name}@TLVPPAGEOFF]\n    ldr x16, [x0]\n    blr x16\n"
                    )
                } else {
                    format!("    movq _{name}@TLVP(%rip), %rdi\n    call *(%rdi)\n")
                };
                assert!(
                    asm.contains(&sequence),
                    "{triple} {opt}: {name} is not reached through its descriptor:\n{asm}"
                );
                for plain in [
                    format!("_{name}@PAGE"),
                    format!("_{name}@GOTPAGE"),
                    format!("_{name}(%rip)"),
                    format!("_{name}@GOTPCREL"),
                    format!("_{name}+"),
                ] {
                    assert!(
                        !asm.contains(&plain),
                        "{triple} {opt}: {name} reached as plain data ({plain}):\n{asm}"
                    );
                }
            }
        }
    }
}

/// [`SHADOW_LIB`]'s thread-local, reached past a parameter of the same name,
/// goes through its descriptor in every function under every call-based
/// model. Missed, it was reached as plain data on Darwin -- a store wrote
/// over the descriptor itself -- and by Initial Exec in x86-64 shared code.
#[test]
fn tls_shadowed_by_a_parameter_is_reached_through_its_descriptor() {
    let funcs = ["set_shadowed", "get_shadowed", "addr_shadowed"];
    for (triple, flags, call, plain) in [
        (
            "x86_64-unknown-linux-gnu",
            &["-fPIC"][..],
            "    leaq x@TLSDESC(%rip), %rax\n    call *x@TLSCALL(%rax)\n",
            &["x@GOTTPOFF", "x@TPOFF", "x(%rip)", "x@GOTPCREL"][..],
        ),
        (
            "aarch64-unknown-linux-gnu",
            &["-fPIC"][..],
            "    .tlsdesccall x\n",
            &[":gottprel:x", ":tprel_", ":lo12:x", ":got:x"][..],
        ),
        (
            DARWIN_AARCH64,
            &[][..],
            "    adrp x0, _x@TLVPPAGE\n    ldr x0, [x0, _x@TLVPPAGEOFF]\n    ldr x16, [x0]\n    blr x16\n",
            &["_x@PAGE", "_x@GOTPAGE"][..],
        ),
        (
            DARWIN_X86_64,
            &[][..],
            "    movq _x@TLVP(%rip), %rdi\n    call *(%rdi)\n",
            &["_x(%rip)", "_x@GOTPCREL"][..],
        ),
    ] {
        for opt in ["-O0", "-O2"] {
            let mut opts = vec![opt];
            opts.extend_from_slice(flags);
            let asm = asm_for_with("tls_shadowed", triple, SHADOW_LIB, &opts);
            for func in funcs {
                let body = body_of(&asm, func);
                assert!(
                    body.contains(call),
                    "{triple} {opt}: {func} does not reach x through its descriptor:\n{body}"
                );
                for p in plain {
                    assert!(
                        !body.contains(p),
                        "{triple} {opt}: {func} reaches x as {p}:\n{body}"
                    );
                }
            }
        }
    }
}

/// The x86-64 getter keeps no XMM register, so no floating-point value may
/// be live in one across `call *(%rdi)`: each must be reloaded or rewritten
/// after the call before it is read.
#[test]
fn tls_macho_x86_64_getter_destroys_xmm_registers() {
    for opt in ["-O0", "-O2"] {
        let asm = asm_for_with("tls_macho_fp", DARWIN_X86_64, MACHO_FORMS, &[opt]);
        let body = body_of(&asm, "keep");
        let after = body
            .split("    call *(%rdi)\n")
            .nth(1)
            .unwrap_or_else(|| panic!("{opt}: no getter call in keep:\n{body}"));
        let mut written = std::collections::HashSet::new();
        for line in after.lines() {
            let line = line.trim();
            let Some((mnemonic, operands)) = line.split_once(' ') else {
                continue;
            };
            let dest = operands.rsplit(", ").next().unwrap_or("");
            let pure_move = mnemonic.starts_with("mov");
            for reg in operands.split(", ").filter(|op| op.starts_with("%xmm")) {
                let is_dest_only = pure_move && reg == dest;
                assert!(
                    is_dest_only || written.contains(reg),
                    "{opt}: {reg} read after the getter destroyed it:\n{body}"
                );
            }
            if dest.starts_with("%xmm") {
                written.insert(dest.to_string());
            }
        }
    }
}

/// No incoming argument may be read from a register the TLV getter destroys.
///
/// The getter takes its descriptor in, and returns the address in, x0 on
/// aarch64; on x86-64 it takes %rdi and returns %rax. So after the call an
/// `int` parameter can no longer be in w0, or in %edi/%rdi: reading one there
/// before rewriting it reads the thread-local's address or garbage. This is
/// exactly what the macOS CI runner hit (`store_local(77)` stored an address,
/// a thread worker's `k` became a pointer and faulted).
#[test]
fn tls_macho_arguments_survive_the_getter() {
    let src = r#"
_Thread_local int ti;
_Thread_local char tc;
_Thread_local long tl;
void st(int a, int b, long c) { ti = a; tc = (char)b; tl = c; ti += a; }
"#;
    // `clobbered` is what an int or long argument would be read as; `names`
    // is every spelling of the register, any of which rewrites it.
    for (triple, call, clobbered, names) in [
        (
            DARWIN_AARCH64,
            "    blr x16\n",
            &["w0"][..],
            &["w0", "x0"][..],
        ),
        (
            DARWIN_X86_64,
            "    call *(%rdi)\n",
            &["%edi", "%rdi"][..],
            &["%dil", "%di", "%edi", "%rdi"][..],
        ),
    ] {
        for opt in ["-O0", "-O2"] {
            let asm = asm_for_with("tls_macho_args", triple, src, &[opt]);
            let body = body_of(&asm, "st");
            for after in body.split(call).skip(1) {
                for line in after.lines() {
                    let line = line.trim();
                    let Some((mnemonic, operands)) = line.split_once(' ') else {
                        continue;
                    };
                    let ops: Vec<&str> = operands.split(", ").collect();
                    // x86-64 writes its last operand, and every instruction
                    // but a move also reads it. aarch64 writes its first --
                    // except a store, where every register operand is read.
                    let (dest, srcs): (&str, Vec<&str>) = if triple == DARWIN_X86_64 {
                        let dest = ops[ops.len() - 1];
                        let mut srcs = ops[..ops.len() - 1].to_vec();
                        if !mnemonic.starts_with("mov") {
                            srcs.push(dest);
                        }
                        (dest, srcs)
                    } else if mnemonic.starts_with("st") {
                        ("", ops.clone())
                    } else {
                        (ops[0], ops[1..].to_vec())
                    };
                    let reads_clobbered = srcs.iter().any(|op| clobbered.contains(op));
                    assert!(
                        !reads_clobbered,
                        "{triple} {opt}: an argument read from a register the getter \
                         destroyed ({line}):\n{body}"
                    );
                    if names.contains(&dest) {
                        break;
                    }
                }
            }
        }
    }
}

/// FreeBSD is ELF and uses the same thread-pointer models as Linux. The
/// backends asked for Linux alone before treating a symbol as thread-local,
/// so on FreeBSD every thread-local was plain data -- one copy for all
/// threads.
#[test]
fn tls_freebsd_uses_the_elf_models() {
    let src = r#"
_Thread_local int tv = 5;
extern _Thread_local int te;
int get(void) { return tv + te; }
void set(int x) { tv = x; te = x; }
int *addr(void) { return &tv; }
"#;
    for (triple, local, ie) in [
        (
            "x86_64-unknown-freebsd",
            "%fs:tv@TPOFF",
            "te@GOTTPOFF(%rip)",
        ),
        ("aarch64-unknown-freebsd", ":tprel_hi12:tv", ":gottprel:te"),
    ] {
        for opt in ["-O0", "-O2"] {
            let asm = asm_for_with("tls_freebsd", triple, src, &[opt]);
            assert!(
                asm.contains(local) && asm.contains(ie),
                "{triple} {opt}: expected Local Exec and Initial Exec:\n{asm}"
            );
            assert!(
                !asm.contains("tv(%rip)")
                    && !asm.contains(":lo12:tv")
                    && !asm.contains("te@GOTPCREL"),
                "{triple} {opt}: a thread-local reached as plain data:\n{asm}"
            );
        }
    }
}

/// Code for a FreeBSD shared object never uses Local Exec.
///
/// Local Exec bakes in an offset from the main executable's thread pointer,
/// which is only right inside the executable. FreeBSD keeps the static models
/// for shared code (Linux takes the descriptor model there), and the choice of
/// Initial Exec ignored `-fPIC`, so a thread-local defined in the same file got
/// `%fs:t@TPOFF`, which `ld -shared` refuses outright on x86-64 and aarch64's
/// linker accepts and then resolves against the wrong block.
#[test]
fn tls_freebsd_shared_objects_use_initial_exec() {
    let src = r#"
_Thread_local int t;
extern _Thread_local int e;
int get(void) { return t + e; }
int *addr(void) { return &t; }
"#;
    for (triple, ie, le) in [
        ("x86_64-unknown-freebsd", "t@GOTTPOFF(%rip)", "@TPOFF"),
        ("aarch64-unknown-freebsd", ":gottprel:t", ":tprel_"),
    ] {
        for flags in [
            &["-O2", "-fPIC"][..],
            &["-O0", "-fPIC"],
            &["-O2", "--shared"],
        ] {
            let asm = asm_for_with("tls_freebsd_so", triple, src, flags);
            assert!(
                asm.contains(ie) && !asm.contains(le),
                "{triple} {flags:?}: expected Initial Exec, no Local Exec:\n{asm}"
            );
        }
    }
}

/// Inline-asm operands on thread-locals, of every constraint class, in a
/// FreeBSD shared object: Initial Exec for all of them, the one place the
/// static models give a thread-local defined in this file Initial Exec.
///
/// Four model-blind backend paths printed a thread-local as `%fs:t@TPOFF` or
/// `t(%rip)`. None is reached -- a register operand arrives as a loaded value
/// and a memory operand goes through the model-aware path -- and each now
/// reports an internal error; this is the case that would show one firing.
#[test]
fn tls_freebsd_shared_asm_operands_use_initial_exec() {
    let src = r#"
struct S { int a; long b; };
_Thread_local int t;
_Thread_local struct S s;
extern _Thread_local int e;
int f(void)
{
    int r, q;
    __asm__("movl %1, %0" : "=r"(r) : "r"(t));
    __asm__("movl %1, %0" : "=r"(q) : "m"(s.a));
    __asm__("incl %0" : "+rm"(e));
    __asm__("incl %0" : "+m"(t));
    __asm__("movq $1, %0" : "=g"(s.b));
    __asm__("incl %0" : "=r"(t) : "0"(t));
    __asm__("" : : "X"(t), "X"(e));
    return r + q;
}
"#;
    for flags in [&["-O0", "-fPIC"][..], &["-O2", "--shared"]] {
        let asm = asm_for_with("tls_freebsd_so_asm", "x86_64-unknown-freebsd", src, flags);
        assert!(
            asm.contains("t@GOTTPOFF(%rip)")
                && asm.contains("e@GOTTPOFF(%rip)")
                && asm.contains("s@GOTTPOFF(%rip)")
                && !asm.contains("@TPOFF")
                && !asm.contains("t(%rip)"),
            "{flags:?}: expected Initial Exec only:\n{asm}"
        );
    }
}

// ---------------------------------------------------------------------------
// -ftls-model=
// ---------------------------------------------------------------------------

/// The ELF access a function's thread-local reference took.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Access {
    /// A descriptor call: the dynamic models.
    Dynamic,
    InitialExec,
    LocalExec,
}

/// Which access `body` uses, by the relocations it names.
fn access_in(body: &str) -> Access {
    let dynamic = body.contains("@TLSDESC") || body.contains(":tlsdesc:");
    let ie = body.contains("@GOTTPOFF") || body.contains(":gottprel:");
    let le = body.contains("@TPOFF") || body.contains(":tprel_hi12:");
    match (dynamic, ie, le) {
        (true, false, false) => Access::Dynamic,
        (false, true, false) => Access::InitialExec,
        (false, false, true) => Access::LocalExec,
        _ => panic!("no single thread-local access in:\n{body}"),
    }
}

/// `-ftls-model=` names the least optimized model the compilation may use,
/// and the access is the stronger of that and what the code can prove, as
/// gcc decides it: an executable keeps Local Exec for its own thread-locals
/// whatever is asked, `initial-exec` takes shared code off the descriptor
/// call, and `local-exec` is Local Exec for everything, a thread-local
/// defined elsewhere included. c17 has no Local Dynamic sequence and gives
/// `local-dynamic` the descriptor call, which is a valid Global Dynamic
/// access (gcc's own aarch64 code uses the descriptor for both). The option
/// was ignored with a warning.
///
/// gcc 13, x86-64 and aarch64 alike, for a defined `t` and an `extern` `e`:
///
/// | flags                          | t  | e  |
/// |--------------------------------|----|----|
/// | (none), `global-dynamic`, `local-dynamic`, `initial-exec` | LE | IE |
/// | `local-exec`                   | LE | LE |
/// | `-fPIC`, `-fPIC global-dynamic`, `-fPIC local-dynamic` | GD | GD |
/// | `-fPIC initial-exec`           | IE | IE |
/// | `-fPIC local-exec`             | LE | LE |
#[test]
fn tls_model_option_sets_the_least_optimized_model() {
    use Access::{Dynamic as GD, InitialExec as IE, LocalExec as LE};
    let src = "_Thread_local int t;\nextern _Thread_local int e;\n\
               int rt(void) { return t; }\nint re(void) { return e; }\n";
    let rows: &[(&[&str], Access, Access)] = &[
        (&[], LE, IE),
        (&["-ftls-model=global-dynamic"], LE, IE),
        (&["-ftls-model=local-dynamic"], LE, IE),
        (&["-ftls-model=initial-exec"], LE, IE),
        (&["-ftls-model=local-exec"], LE, LE),
        (&["-fPIC"], GD, GD),
        (&["-fPIC", "-ftls-model=global-dynamic"], GD, GD),
        (&["-fPIC", "-ftls-model=local-dynamic"], GD, GD),
        (&["-fPIC", "-ftls-model=initial-exec"], IE, IE),
        (&["-fPIC", "-ftls-model=local-exec"], LE, LE),
        // The last one given wins.
        (
            &[
                "-fPIC",
                "-ftls-model=local-exec",
                "-ftls-model=initial-exec",
            ],
            IE,
            IE,
        ),
        (&["-shared", "-ftls-model=initial-exec"], IE, IE),
    ];
    for triple in [
        super::asm_probe::X86_64_LINUX,
        super::asm_probe::AARCH64_LINUX,
    ] {
        for opt in ["-O0", "-O2"] {
            for &(flags, t, e) in rows {
                let mut all = vec![opt];
                all.extend_from_slice(flags);
                let asm = asm_for_with("tls_model_opt", triple, src, &all);
                let got = (
                    access_in(body_of(&asm, "rt")),
                    access_in(body_of(&asm, "re")),
                );
                assert_eq!(got, (t, e), "{triple} {all:?}:\n{asm}");
            }
        }
    }
}

/// Mach-O has one thread-local access, the TLV getter call, and the option
/// leaves it alone, as clang does.
#[test]
fn tls_model_option_leaves_macho_alone() {
    let src = "_Thread_local int t;\nint rt(void) { return t; }\n";
    let asm = asm_for_with(
        "tls_model_macho",
        super::asm_probe::AARCH64_DARWIN,
        src,
        &["-O2", "-ftls-model=local-exec"],
    );
    let body = body_of(&asm, "_rt");
    assert!(body.contains("@TLVPPAGE"), "{body}");
}
