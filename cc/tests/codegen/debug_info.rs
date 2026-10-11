//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// `-g` output that needs more than one source file or a DWARF reader: the
// line table's file list, and the type DIEs as readelf decodes them.
//

use crate::common::{run_c17, work_dir};

/// The numbered `.file` directives of an assembly listing but entry 0,
/// which names a directory too: (number, path).
fn file_table(asm: &str) -> Vec<(u32, String)> {
    asm.lines()
        .filter_map(|l| {
            let rest = l.trim().strip_prefix(".file ")?;
            let (n, path) = rest.split_once(' ')?;
            let n: u32 = n.parse().ok()?;
            (n != 0).then(|| (n, path.trim_matches('"').to_string()))
        })
        .collect()
}

/// The line table lists each source file once, and no synthetic stream.
///
/// c17 numbered its `.file` table by preprocessor stream, and a stream is
/// one *inclusion*: a header included twice got two numbers, and the
/// streams that are no file at all -- the command-line `-D`s, the
/// predefined macros -- got an entry named ".". gprofng's `archive -a src`
/// then tried to copy "<comp_dir>/." and gave up ("Cannot find file"), so
/// the archived experiment had no source. gcc lists each file once, and code
/// a `-D` macro produced belongs to the line that used it.
///
/// On ELF, entry 0 names the compilation directory and the primary source,
/// as gcc's does: the assembler then writes a version 5 line table, where
/// an older one leaves slot 0 to the reader, and gprofng fills it with the
/// directory and reports that "file" missing as well.
#[test]
fn codegen_debug_file_table_lists_each_file_once() {
    let dir = work_dir("debug_files");
    let p = |name: &str| dir.path().join(name).to_string_lossy().into_owned();
    std::fs::write(p("h.h"), "extern int hv;\n").unwrap();
    std::fs::write(
        p("g.h"),
        "static int from_header(int a)\n{\n    return a + 1;\n}\n",
    )
    .unwrap();
    std::fs::write(
        p("t.c"),
        "#include \"h.h\"\n\
         #include \"g.h\"\n\
         #include \"h.h\"\n\
         int main(void)\n\
         {\n\
         \x20   return from_header(PLUS) + hv;\n\
         }\n",
    )
    .unwrap();

    for target in [
        "--target=x86_64-unknown-linux-gnu",
        "--target=aarch64-unknown-linux-gnu",
        "--target=aarch64-apple-darwin",
    ] {
        let out = p("t.s");
        let r = run_c17(&[target, "-g", "-O0", "-DPLUS=3", "-S", "-o", &out, &p("t.c")]);
        assert!(r.success, "{target}: {}", r.stderr);
        let asm = std::fs::read_to_string(&out).unwrap();
        let files = file_table(&asm);

        let entry0 = asm
            .lines()
            .map(str::trim)
            .find(|l| l.starts_with(".file 0 "));
        if target.ends_with("darwin") {
            assert_eq!(entry0, None, "{target}: Mach-O keeps the older table");
        } else {
            let cwd = std::env::current_dir().unwrap();
            let expected = format!(".file 0 \"{}\" \"{}\"", cwd.display(), p("t.c"));
            assert_eq!(entry0, Some(expected.as_str()), "{target}:\n{asm}");
        }

        for (n, path) in &files {
            assert!(
                !path.is_empty() && path != ".",
                "{target}: .file {n} names no file:\n{files:?}"
            );
        }
        let mut paths: Vec<&str> = files.iter().map(|(_, p)| p.as_str()).collect();
        paths.sort();
        let count = paths.len();
        paths.dedup();
        assert_eq!(
            paths.len(),
            count,
            "{target}: a file is listed twice:\n{files:?}"
        );
        for name in ["t.c", "h.h", "g.h"] {
            assert!(
                files.iter().any(|(_, p)| p.ends_with(name)),
                "{target}: no .file for {name}:\n{files:?}"
            );
        }

        // Every `.loc` names a listed file, and the header's function body
        // is attributed to the header.
        let number_of = |name: &str| {
            files
                .iter()
                .find(|(_, p)| p.ends_with(name))
                .map(|(n, _)| *n)
                .unwrap()
        };
        let locs: Vec<(u32, u32)> = asm
            .lines()
            .filter_map(|l| {
                let mut f = l.trim().strip_prefix(".loc ")?.split_whitespace();
                Some((f.next()?.parse().ok()?, f.next()?.parse().ok()?))
            })
            .collect();
        for (file, line) in &locs {
            assert!(
                files.iter().any(|(n, _)| n == file),
                "{target}: .loc {file} {line} names no listed file:\n{asm}"
            );
        }
        assert!(
            locs.contains(&(number_of("g.h"), 3)),
            "{target}: from_header's `return` is g.h line 3:\n{asm}"
        );
        assert!(
            locs.contains(&(number_of("t.c"), 6)),
            "{target}: main's `return` -- PLUS included -- is t.c line 6:\n{asm}"
        );
    }
}

/// One DIE as readelf prints it: its tag, and its name and type if it has
/// them.
#[cfg(target_os = "linux")]
#[derive(Debug, Default, Clone)]
struct Die {
    tag: String,
    name: Option<String>,
    typ: Option<u64>,
}

/// The DIEs of an object, by offset, from `readelf --debug-dump=info`.
#[cfg(target_os = "linux")]
fn read_dies(obj: &str) -> std::collections::BTreeMap<u64, Die> {
    let out = std::process::Command::new("readelf")
        .args(["--debug-dump=info", obj])
        .output()
        .expect("readelf");
    let text = String::from_utf8_lossy(&out.stdout);
    let mut dies = std::collections::BTreeMap::new();
    let mut current: Option<u64> = None;
    for line in text.lines() {
        let line = line.trim();
        // `<1><45>: Abbrev Number: 2 (DW_TAG_const_type)`
        if let Some(tag_at) = line.find("(DW_TAG_") {
            let off = line
                .split('>')
                .nth(1)
                .and_then(|s| s.strip_prefix('<'))
                .and_then(|s| u64::from_str_radix(s, 16).ok())
                .expect("DIE offset");
            let tag = line[tag_at + 1..].trim_end_matches(')').to_string();
            dies.insert(
                off,
                Die {
                    tag,
                    ..Die::default()
                },
            );
            current = Some(off);
            continue;
        }
        let Some(die) = current.and_then(|o| dies.get_mut(&o)) else {
            continue;
        };
        let value = |l: &str| l.split_once(": ").map(|(_, v)| v.trim().to_string());
        if line.contains("DW_AT_name") {
            // An indirect string reads `(indirect string, offset: 0x93): long int`.
            die.name = value(line).map(|v| match v.rsplit_once("): ") {
                Some((_, n)) => n.to_string(),
                None => v,
            });
        } else if line.contains("DW_AT_type") {
            die.typ = value(line).and_then(|v| {
                u64::from_str_radix(v.trim_start_matches("<0x").trim_end_matches('>'), 16).ok()
            });
        }
    }
    dies
}

/// The chain of tags from a named variable's type down to a DIE with no
/// type, then that DIE's name: `const_type pointer_type base_type:int`.
#[cfg(target_os = "linux")]
fn type_chain(dies: &std::collections::BTreeMap<u64, Die>, var: &str) -> String {
    let die = dies
        .values()
        .find(|d| {
            matches!(
                d.tag.as_str(),
                "DW_TAG_variable" | "DW_TAG_formal_parameter"
            ) && d.name.as_deref() == Some(var)
        })
        .unwrap_or_else(|| panic!("no DIE for {var}:\n{dies:#?}"));
    let mut chain = Vec::new();
    let mut next = die.typ;
    while let Some(off) = next {
        let t = &dies[&off];
        let tag = t.tag.trim_start_matches("DW_TAG_");
        chain.push(match &t.name {
            Some(n) => format!("{tag}:{n}"),
            None => tag.to_string(),
        });
        next = t.typ;
        assert!(chain.len() < 16, "{var}: type chain loops: {chain:?}");
    }
    chain.join(" ")
}

/// A qualified type is a modifier DIE over the unqualified one, as gcc
/// emits it -- `DW_TAG_const_type`, `DW_TAG_volatile_type`,
/// `DW_TAG_restrict_type`, `DW_TAG_atomic_type` -- not a base type named
/// "const int": a debugger looks a base type up by name and encoding, and
/// a pointer's own qualifier was dropped altogether. gcc nests them const
/// innermost, then volatile, restrict, atomic.
#[cfg(target_os = "linux")]
#[test]
fn codegen_debug_qualifiers_are_modifier_dies() {
    let dir = work_dir("debug_quals");
    let p = |name: &str| dir.path().join(name).to_string_lossy().into_owned();
    std::fs::write(
        p("q.c"),
        "struct S { int a; };\n\
         volatile long v;\n\
         const int c = 1;\n\
         int plain;\n\
         int *const cp = 0;\n\
         const int *pc;\n\
         int *const restrict cpr;\n\
         _Atomic int ai;\n\
         const volatile int cvi;\n\
         const _Atomic int cai;\n\
         const struct S cs;\n\
         const void *cvp;\n\
         const signed int csi;\n\
         const short csh;\n\
         volatile unsigned long long vull;\n\
         int f(void)\n\
         {\n\
         \x20   volatile int lv = 1;\n\
         \x20   return lv + c + v;\n\
         }\n",
    )
    .unwrap();
    let obj = p("q.o");
    let r = run_c17(&["-g", "-O0", "-c", "-o", &obj, &p("q.c")]);
    assert!(r.success, "{}", r.stderr);
    let dies = read_dies(&obj);

    for d in dies.values() {
        if let Some(n) = &d.name {
            assert!(
                !["const", "volatile", "restrict", "_Atomic"]
                    .iter()
                    .any(|q| n.split_whitespace().any(|w| w == *q)),
                "a qualifier in a type name: {d:?}"
            );
        }
    }
    let ints = dies
        .values()
        .filter(|d| d.tag == "DW_TAG_base_type" && d.name.as_deref() == Some("int"))
        .count();
    assert_eq!(ints, 1, "one base-type DIE for int:\n{dies:#?}");

    for (var, chain) in [
        ("v", "volatile_type base_type:long"),
        ("c", "const_type base_type:int"),
        ("plain", "base_type:int"),
        ("cp", "const_type pointer_type base_type:int"),
        ("pc", "pointer_type const_type base_type:int"),
        ("cpr", "restrict_type const_type pointer_type base_type:int"),
        ("ai", "atomic_type base_type:int"),
        ("cvi", "volatile_type const_type base_type:int"),
        ("cai", "atomic_type const_type base_type:int"),
        ("cs", "const_type structure_type"),
        ("cvp", "pointer_type const_type"),
        ("lv", "volatile_type base_type:int"),
        // However the scalar is spelled, the qualifier wraps the plain one.
        ("csi", "const_type base_type:int"),
        ("csh", "const_type base_type:short"),
        ("vull", "volatile_type base_type:unsigned long long"),
    ] {
        assert_eq!(type_chain(&dies, var), chain, "{var}");
    }

    // Each qualified type is described once, however many objects use it.
    let consts_over_int = dies
        .values()
        .filter(|d| {
            d.tag == "DW_TAG_const_type"
                && d.typ
                    .is_some_and(|t| dies[&t].name.as_deref() == Some("int"))
        })
        .count();
    assert_eq!(consts_over_int, 1, "one const int DIE:\n{dies:#?}");
}
