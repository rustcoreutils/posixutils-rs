//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Objects placed in named sections, linked and run: the linker's
// `__start_`/`__stop_` symbols find exactly what the program put there. The
// assembly-level checks are in `cc/test_asm/codegen_named_sections.rs`.
//
// ELF only: Mach-O section names are `SEGMENT,section` pairs.
//

#![cfg(target_os = "linux")]

use crate::common::{compile_and_run, compile_and_run_everywhere};

/// A section table built the way Linux builds its trace-event and dynamic-
/// debug tables: pointers declared `static struct T __used __section(...)
/// *p` (the attribute after the tag), and block-scope statics with
/// `section`. Before, both landed in `.data`, so the table was empty.
#[test]
fn codegen_named_section_tables_hold_what_was_put_there() {
    let src = r#"
struct ev { int id; };
static struct ev ev1 = { 1 }, ev2 = { 2 }, ev3 = { 4 };
static struct ev __attribute__((__used__)) __attribute__((__section__("evtab"))) *e1 = &ev1;
static struct ev __attribute__((used, section("evtab"))) *e2 = &ev2;
extern struct ev *__start_evtab[], *__stop_evtab[];

static int bump(void)
{
    static struct ev __attribute__((used, section("evtab"))) *e3 = &ev3;
    static int once __attribute__((section("onceflags")));
    return once++;
}
extern int __start_onceflags[], __stop_onceflags[];

int main(void)
{
    int sum = 0;
    for (struct ev **p = __start_evtab; p < __stop_evtab; p++)
        sum += (*p)->id;
    if (sum != 7)
        return 1;
    if (__stop_onceflags - __start_onceflags != 1)
        return 2;
    if (bump() != 0 || bump() != 1 || __start_onceflags[0] != 2)
        return 3;
    return 0;
}
"#;
    compile_and_run_everywhere("named_section_tables", src);
}

/// A `const` table holding addresses and a `const` string share one named
/// section, as the kernel's `__initconst` objects do. They were given two
/// flag sets and the assembler refused the second; in every code model the
/// unit must build and the objects read back.
#[test]
fn codegen_named_section_mixes_relocated_and_plain_constants() {
    let src = r#"
int f(void) { return 7; }
struct d { int (*cb)(void); const char *s; };
static const struct d tab[] __attribute__((__section__("initro"), used)) = { { f, "x" } };
static const char str[] __attribute__((__section__("initro"), used)) = "hello";
int main(void)
{
    return !(tab[0].cb() == 7 && tab[0].s[0] == 'x' && str[4] == 'o');
}
"#;
    compile_and_run_everywhere("named_section_mixed_const", src);
    for pic in ["-fno-pic", "-fPIC"] {
        let opts = vec![pic.to_string()];
        assert_eq!(
            compile_and_run("named_section_mixed_const", src, &opts),
            0,
            "{pic}"
        );
    }
}
