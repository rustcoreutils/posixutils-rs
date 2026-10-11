//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// The include search chain as gcc builds it from the command line: a
// directory named twice is searched once, and a `-I` naming a system
// directory gives way to it. `#include_next` walks that chain, so a
// duplicate left in it sends a header back into itself.
//

use crate::common::run_c17;
use std::path::Path;

/// A scratch directory with `d1/si.h` (guarded, forwarding with
/// `#include_next`), `d2/si.h` (defining `D2`), `d3/si.h` (an `#error`),
/// and `d1/w.h` and `d2/w.h` defining `WHICH` as 1 and 2.
fn scratch() -> plib::tmp::TempDir {
    let dir = plib::tmp::Builder::new()
        .prefix("c17_search_chain_")
        .tempdir()
        .unwrap();
    let write = |name: &str, text: &str| {
        let p = dir.path().join(name);
        std::fs::create_dir_all(p.parent().unwrap()).unwrap();
        std::fs::write(p, text).unwrap();
    };
    write(
        "d1/si.h",
        "#ifndef D1\n#define D1\n#include_next <si.h>\n#endif\n",
    );
    write("d2/si.h", "#define D2 2\n");
    write("d3/si.h", "#error d3 reached\n");
    write("d1/w.h", "#define WHICH 1\n");
    write("d2/w.h", "#define WHICH 2\n");
    write("si.c", "#include <si.h>\nint x = D2;\n");
    write("q.c", "#include \"si.h\"\nint x = D2;\n");
    write("w.c", "#include <w.h>\nint x = WHICH;\n");
    dir
}

/// `-E -P` of `file` in `dir` with `opts`, with each `{}` in an option
/// replaced by `dir`.
fn preprocess(dir: &Path, opts: &[&str], file: &str) -> String {
    let d = dir.to_string_lossy();
    let mut args: Vec<String> = vec!["-E".into(), "-P".into()];
    args.extend(opts.iter().map(|o| o.replace("{}", &d)));
    args.push(dir.join(file).to_string_lossy().into_owned());
    let argv: Vec<&str> = args.iter().map(String::as_str).collect();
    let r = run_c17(&argv);
    assert!(r.success, "{opts:?}: {}", r.stderr);
    r.stdout.trim().to_string()
}

/// glibc passes `-I../include` twice for some modules, so its
/// `include/stackinfo.h` reached itself through `#include_next` and the
/// guard hid the real header: `_STACK_GROWS_DOWN` undefined. gcc drops the
/// second `-I`, however the directory is spelled.
#[test]
fn search_chain_duplicate_dash_i_is_searched_once() {
    let dir = scratch();
    let p = dir.path();
    for opts in [
        &["-I{}/d1", "-I{}/d1", "-I{}/d2", "-I{}/d3"][..],
        &["-I{}/d1", "-I{}/d1/", "-I{}/./d1", "-I{}/d2", "-I{}/d3"],
    ] {
        assert_eq!(preprocess(p, opts, "si.c"), "int x = 2;", "{opts:?}");
    }
}

/// A `-I` naming a directory that is also a system directory is dropped,
/// so the system order wins (gcc's "as it is a non-system directory that
/// duplicates a system directory").
#[test]
fn search_chain_dash_i_gives_way_to_a_system_directory() {
    let dir = scratch();
    let p = dir.path();
    for opts in [
        &["-I{}/d1", "-I{}/d2", "-isystem", "{}/d1"][..],
        &["-I{}/d1", "-I{}/d2", "-idirafter", "{}/d1"],
    ] {
        assert_eq!(preprocess(p, opts, "w.c"), "int x = 2;", "{opts:?}");
    }
    // A -isystem directory is searched ahead of every other system one,
    // but behind -I.
    assert_eq!(
        preprocess(p, &["-isystem", "{}/d2", "-I{}/d1", "-I{}/d2"], "w.c"),
        "int x = 1;"
    );
}

/// The `-iquote` directories are deduplicated too, and the last of them is
/// dropped when it is the first `-I` directory.
#[test]
fn search_chain_iquote_duplicates_are_dropped() {
    let dir = scratch();
    let p = dir.path();
    for opts in [
        &["-iquote", "{}/d1", "-iquote", "{}/d1", "-I{}/d2"][..],
        &["-iquote", "{}/d1", "-I{}/d1", "-I{}/d2"],
    ] {
        assert_eq!(preprocess(p, opts, "q.c"), "int x = 2;", "{opts:?}");
    }
}

/// The host gcc's private headers (`<unwind.h>`) are found last, after
/// every directory the command line and the target name, so a project's own
/// `unwind.h` wins; and they go with the bundled headers under `-nostdinc`.
#[cfg(target_os = "linux")]
#[test]
fn search_chain_host_compiler_headers_come_last() {
    let host = run_c17(&["-print-file-name=include"]).stdout;
    if !Path::new(host.trim()).join("unwind.h").is_file() {
        return; // the host driver is not gcc
    }
    let dir = scratch();
    std::fs::create_dir_all(dir.path().join("own")).unwrap();
    std::fs::write(dir.path().join("own/unwind.h"), "#define OWN 1\n").unwrap();
    std::fs::write(
        dir.path().join("u.c"),
        "#include <unwind.h>\n#ifdef OWN\nint own;\n#else\nint gcc_s = _URC_NO_REASON;\n#endif\n",
    )
    .unwrap();
    let out = preprocess(dir.path(), &[], "u.c");
    assert!(out.ends_with("int gcc_s = _URC_NO_REASON;"), "{out}");
    for opts in [&["-I{}/own"][..], &["-idirafter", "{}/own"]] {
        let out = preprocess(dir.path(), opts, "u.c");
        assert!(out.ends_with("int own;"), "{opts:?}: {out}");
    }
    let u = dir.path().join("u.c");
    let r = run_c17(&["-E", "-nostdinc", u.to_str().unwrap()]);
    assert!(
        !r.success && r.stderr.contains("'unwind.h': file not found"),
        "{}",
        r.stderr
    );
}
