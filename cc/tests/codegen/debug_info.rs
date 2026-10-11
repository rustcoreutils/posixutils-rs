//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// `-g` output that needs more than one source file: the line table's
// file list.
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
