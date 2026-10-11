//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// The `-f` options that change what a unit means or how its functions
// begin and end, which a kernel build passes: `-fshort-wchar`,
// `-ffreestanding`, `-fstrict-flex-arrays`, `-ftrivial-auto-var-init=`,
// `-fpatchable-function-entry=`, run on the host and on aarch64 under
// qemu. Every expectation is gcc 13's.
//

use crate::common::{compile_and_run, compile_and_run_aarch64_with, preprocess_text};

/// Run `src` built with `flags` at -O0 and -O2, on the host and, when the
/// cross toolchain is present, on aarch64; each run must exit 0.
fn run_with(name: &str, src: &str, flags: &[&str]) {
    for opt in ["-O0", "-O2"] {
        let mut args: Vec<String> = flags.iter().map(|f| f.to_string()).collect();
        args.push(opt.to_string());
        assert_eq!(
            compile_and_run(name, src, &args),
            0,
            "{name} {flags:?} {opt}"
        );
        let mut cross: Vec<&str> = flags.to_vec();
        cross.push(opt);
        if let Some(code) = compile_and_run_aarch64_with(name, src, &cross, &[]) {
            assert_eq!(code, 0, "{name} {flags:?} on aarch64 at {opt}");
        }
    }
}

/// `-fshort-wchar` makes `wchar_t` gcc's `short unsigned int`: a wide
/// string literal is an array of 16-bit units, UTF-16 encoded, that
/// initializes a `u16` array as the kernel's EFI code does; a wide
/// character constant has that type; and `__WCHAR_TYPE__` and its kin say
/// so.
#[test]
fn code_options_short_wchar() {
    let src = r#"
typedef unsigned short efi_char16_t;
typedef __WCHAR_TYPE__ wchar_t;
_Static_assert(sizeof(wchar_t) == 2 && __SIZEOF_WCHAR_T__ == 2, "16-bit wchar_t");
_Static_assert(__WCHAR_MAX__ == 0xffff && __WCHAR_MIN__ == 0 && __WCHAR_WIDTH__ == 16, "limits");
_Static_assert(_Generic(L'x', unsigned short: 1, default: 0), "L'x' is wchar_t");
_Static_assert((wchar_t)-1 > 0, "unsigned");
static const efi_char16_t name[] = L"SecureBoot";
const wchar_t *wide = L"\U0001F600z";
#if L'\xffff' < 0
#error "wchar_t is unsigned"
#endif
int main(void) {
    efi_char16_t s[] = L"Sev";
    if (sizeof(s) != 8 || s[1] != 'e' || s[3] != 0) return 1;
    if (sizeof(name) != 22 || name[6] != 'B') return 2;
    if (wide[0] != 0xd83d || wide[1] != 0xde00 || wide[2] != 'z' || wide[3] != 0) return 3;
    if (sizeof(L'x') != 2) return 4;
    return 0;
}
"#;
    run_with("short_wchar", src, &["-fshort-wchar"]);
    // The last of the pair wins.
    let back = "int main(void) { return sizeof(L'x') == sizeof(int) ? 0 : 1; }\n";
    run_with("short_wchar", back, &["-fshort-wchar", "-fno-short-wchar"]);
}

/// `-ffreestanding` gives `__STDC_HOSTED__` 0, and `-fhosted` after it
/// gives it back. (Its `-fno-builtin` is `test_asm/codegen_kernel.rs`'s.)
#[test]
fn code_options_hosted_macro() {
    for (flags, hosted) in [
        (&["-ffreestanding"][..], "0"),
        (&["-ffreestanding", "-fhosted"], "1"),
        (&[], "1"),
    ] {
        let r = preprocess_text("hosted", "__STDC_HOSTED__\n", flags);
        assert!(r.success, "{flags:?}: {}", r.stderr);
        assert_eq!(r.stdout.trim().lines().last(), Some(hosted), "{flags:?}");
    }
}

/// `-fstrict-flex-arrays=N` decides which trailing arrays of an object
/// reached through a pointer `__builtin_object_size` takes for flexible:
/// any at 0, the default; `[1]`, `[0]` and `[]` at 1; `[0]` and `[]` at 2;
/// only `[]` at 3, which the bare option means. gcc 13 at -O2 prints each
/// row.
#[test]
fn code_options_strict_flex_arrays() {
    let src = |want: &str| {
        format!(
            r#"
struct A {{ int n; char t[4]; }};
struct B {{ int n; char t[1]; }};
struct C {{ int n; char t[0]; }};
struct D {{ int n; char t[]; }};
struct E {{ char t[4]; int n; }};
static int sizes[5];
__attribute__((noinline)) void probe(struct A *a, struct B *b, struct C *c, struct D *d, struct E *e) {{
    sizes[0] = (int)__builtin_object_size(a->t, 1);
    sizes[1] = (int)__builtin_object_size(b->t, 1);
    sizes[2] = (int)__builtin_object_size(c->t, 1);
    sizes[3] = (int)__builtin_object_size(d->t, 1);
    sizes[4] = (int)__builtin_object_size(e->t, 1);
}}
int main(void) {{
    static char buf[64];
    probe((void *)buf, (void *)buf, (void *)buf, (void *)buf, (void *)buf);
    static const int want[5] = {{ {want} }};
    for (int i = 0; i < 5; i++) if (sizes[i] != want[i]) return 1 + i;
    return 0;
}}
"#
        )
    };
    for (flags, want) in [
        (&[][..], "-1, -1, -1, -1, 4"),
        (&["-fstrict-flex-arrays=0"], "-1, -1, -1, -1, 4"),
        (&["-fstrict-flex-arrays=1"], "4, -1, -1, -1, 4"),
        (&["-fstrict-flex-arrays=2"], "4, 1, -1, -1, 4"),
        (&["-fstrict-flex-arrays=3"], "4, 1, 0, -1, 4"),
        (&["-fstrict-flex-arrays"], "4, 1, 0, -1, 4"),
        (
            &["-fstrict-flex-arrays", "-fno-strict-flex-arrays"],
            "-1, -1, -1, -1, 4",
        ),
    ] {
        let mut args: Vec<String> = flags.iter().map(|f| f.to_string()).collect();
        args.push("-O2".to_string());
        assert_eq!(
            compile_and_run("flex_arrays", &src(want), &args),
            0,
            "{flags:?}"
        );
    }
}

/// `-ftrivial-auto-var-init=zero` and `=pattern` start every automatic
/// variable declared without an initializer -- scalars, arrays, structures
/// and variable length arrays -- as all zero bytes, or as 0xFE bytes, over
/// a stack a previous call has dirtied; an `uninitialized` one is left
/// alone, and one with an initializer gets it.
#[test]
fn code_options_trivial_auto_var_init() {
    let src = |byte: &str| {
        format!(
            r#"
__attribute__((noinline)) void dirty(void) {{
    volatile char junk[512];
    for (int i = 0; i < 512; i++) junk[i] = (char)(i * 7 + 3);
}}
struct S {{ int a; char c; long d; void *p; }};
static int all(const void *v, unsigned long n) {{
    const unsigned char *b = v;
    for (unsigned long i = 0; i < n; i++) if (b[i] != {byte}) return 0;
    return 1;
}}
__attribute__((noinline)) int check(int n) {{
    int x; long y; char buf[13]; struct S s; int *pp; int arr[n];
    __attribute__((uninitialized)) int u;
    int given = 5;
    (void)u;
    return all(&x, sizeof x) && all(&y, sizeof y) && all(buf, sizeof buf)
        && all(&s, sizeof s) && all(&pp, sizeof pp) && all(arr, sizeof arr)
        && given == 5;
}}
int main(void) {{ dirty(); return check(10) ? 0 : 1; }}
"#
        )
    };
    run_with(
        "auto_var_init",
        &src("0"),
        &["-ftrivial-auto-var-init=zero"],
    );
    run_with(
        "auto_var_init",
        &src("0xfe"),
        &["-ftrivial-auto-var-init=pattern"],
    );
}
