//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! strip on linked ELF files and the options debhelper's dh_strip passes.
//!
//! dh_strip runs, per file type:
//!   shared libraries: `--remove-section=.comment --remove-section=.note --strip-unneeded`
//!   executables:      `--remove-section=.comment --remove-section=.note`
//!   static libraries: `--strip-debug --remove-section=.comment --remove-section=.note
//!                      --enable-deterministic-archives -R .gnu.lto_* -R .gnu.debuglto_*
//!                      -N __gnu_lto_slim -N __gnu_lto_v1`
//!
//! ELF-specific: cc produces Mach-O on macOS, which strip does not support.

#![cfg(target_os = "linux")]

use crate::c_compiler;
use object::read::elf::ElfFile64;
use object::{Endianness, Object, ObjectComdat, ObjectSection, ObjectSymbol, SymbolKind};
use plib::tmp::TempDir;
use std::fs;
use std::path::Path;
use std::process::{Command, Output};

const DH_SHARED: [&str; 3] = [
    "--remove-section=.comment",
    "--remove-section=.note",
    "--strip-unneeded",
];
const DH_EXEC: [&str; 2] = ["--remove-section=.comment", "--remove-section=.note"];
const DH_STATIC: [&str; 12] = [
    "--strip-debug",
    "--remove-section=.comment",
    "--remove-section=.note",
    "--enable-deterministic-archives",
    "-R",
    ".gnu.lto_*",
    "-R",
    ".gnu.debuglto_*",
    "-N",
    "__gnu_lto_slim",
    "-N",
    "__gnu_lto_v1",
];

const LIB_C: &str = "int lib_f(int x){return x+1;}\n\
int lib_g(int x){return lib_f(x)*2;}\n\
static int hid(int x){return x*3;}\n\
int lib_h(int x){return hid(x);}\n";

const MAIN_C: &str = "#include <stdio.h>\n\
int lib_g(int);\n\
int main(void){printf(\"%d\\n\", lib_g(4));return 0;}\n";

fn cc(dir: &Path, args: &[&str]) {
    let out = c_compiler()
        .current_dir(dir)
        .args(args)
        .output()
        .expect("failed to run cc");
    assert!(
        out.status.success(),
        "cc {:?} failed: {}",
        args,
        String::from_utf8_lossy(&out.stderr)
    );
}

fn strip(args: &[&str], file: &Path) -> Output {
    Command::new(env!("CARGO_BIN_EXE_strip"))
        .env("LC_ALL", "C")
        .args(args)
        .arg(file)
        .output()
        .expect("failed to run strip")
}

fn strip_ok(args: &[&str], file: &Path) {
    let out = strip(args, file);
    assert!(
        out.status.success(),
        "strip {:?} failed: {}",
        args,
        String::from_utf8_lossy(&out.stderr)
    );
}

fn run_prints(prog: &Path, expected: &str) {
    let out = Command::new(prog).output().expect("run stripped program");
    assert!(
        out.status.success(),
        "{} failed after strip: {}",
        prog.display(),
        String::from_utf8_lossy(&out.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&out.stdout), expected);
}

fn section_names(bytes: &[u8]) -> Vec<String> {
    let elf = ElfFile64::<Endianness>::parse(bytes).expect("stripped file must be ELF");
    elf.sections()
        .map(|s| s.name().unwrap_or("").to_string())
        .collect()
}

fn symbol_names(bytes: &[u8]) -> Vec<String> {
    let elf = ElfFile64::<Endianness>::parse(bytes).unwrap();
    elf.symbols()
        .map(|s| s.name().unwrap_or("").to_string())
        .collect()
}

fn has_file_symbol(bytes: &[u8]) -> bool {
    let elf = ElfFile64::<Endianness>::parse(bytes).unwrap();
    let found = elf.symbols().any(|s| s.kind() == SymbolKind::File);
    found
}

/// Write `lib.c` and `main.c` in `dir`.
fn write_sources(dir: &Path) {
    fs::write(dir.join("lib.c"), LIB_C).unwrap();
    fs::write(dir.join("main.c"), MAIN_C).unwrap();
}

/// Build `lib.c` as a shared library and `main.c` linked against it.
fn build_shared(dir: &Path) {
    write_sources(dir);
    cc(
        dir,
        &[
            "-g",
            "-fPIC",
            "-shared",
            "-Wl,-soname,libx.so.1",
            "-o",
            "libx.so.1",
            "lib.c",
        ],
    );
    let rpath = format!("-Wl,-rpath,{}", dir.display());
    cc(dir, &["-g", "-o", "main", "main.c", "libx.so.1", &rpath]);
}

#[test]
fn test_strip_pie_executable_still_runs() {
    // Deleting every REL/RELA section took .rela.dyn out of a PIE, and the
    // dynamic loader then aborted on the first relocation it read.
    let dir = TempDir::new().unwrap();
    write_sources(dir.path());
    let exe = dir.path().join("pie");
    cc(
        dir.path(),
        &["-g", "-fPIE", "-pie", "-o", "pie", "main.c", "lib.c"],
    );
    strip_ok(&[], &exe);
    run_prints(&exe, "10\n");
    let names = section_names(&fs::read(&exe).unwrap());
    assert!(names.iter().any(|n| n == ".rela.dyn"), "{names:?}");
    assert!(!names.iter().any(|n| n == ".symtab" || n == ".strtab"));
    assert!(!names.iter().any(|n| n.starts_with(".debug")));
}

#[test]
fn test_strip_dh_strip_executable() {
    let dir = TempDir::new().unwrap();
    build_shared(dir.path());
    let exe = dir.path().join("main");
    strip_ok(&DH_EXEC, &exe);
    run_prints(&exe, "10\n");
    let names = section_names(&fs::read(&exe).unwrap());
    for gone in [".comment", ".symtab", ".strtab", ".debug_info"] {
        assert!(!names.iter().any(|n| n == gone), "{gone} kept: {names:?}");
    }
    // `.note` is an exact section name: the notes the loader and debuggers
    // read (`.note.gnu.build-id`, the ABI tag) stay.
    assert!(names.iter().any(|n| n == ".note.gnu.build-id"), "{names:?}");
}

/// The file bytes of every PT_LOAD segment, what the loader maps, except
/// the ELF header, whose section header fields strip rewrites.
fn loadable_image(bytes: &[u8]) -> Vec<u8> {
    use object::read::elf::{FileHeader, ProgramHeader};
    let header = object::elf::FileHeader64::<Endianness>::parse(bytes).unwrap();
    let endian = header.endian().unwrap();
    let mut image = Vec::new();
    for ph in header.program_headers(endian, bytes).unwrap() {
        if ph.p_type(endian) == object::elf::PT_LOAD {
            let start = image.len();
            image.extend_from_slice(ph.data(endian, bytes).unwrap());
            if ph.p_offset(endian) == 0 {
                let ehsize = usize::from(header.e_ehsize(endian));
                image[start..start + ehsize].fill(0);
            }
        }
    }
    image
}

#[test]
fn test_strip_leaves_loadable_image_untouched() {
    // GNU strip removes only what the loader never maps. Rewriting through
    // object's builder regenerated .gnu.hash and shrank .dynamic.
    let dir = TempDir::new().unwrap();
    build_shared(dir.path());
    for (file, args) in [("libx.so.1", &DH_SHARED[..]), ("main", &DH_EXEC[..])] {
        let path = dir.path().join(file);
        let before = loadable_image(&fs::read(&path).unwrap());
        strip_ok(args, &path);
        let after = loadable_image(&fs::read(&path).unwrap());
        assert!(before == after, "{file}: loadable image changed");
    }
    run_prints(&dir.path().join("main"), "10\n");
}

#[test]
fn test_strip_dh_strip_shared_library() {
    let dir = TempDir::new().unwrap();
    build_shared(dir.path());
    let lib = dir.path().join("libx.so.1");
    strip_ok(&DH_SHARED, &lib);
    run_prints(&dir.path().join("main"), "10\n");
    let bytes = fs::read(&lib).unwrap();
    let names = section_names(&bytes);
    for gone in [".comment", ".symtab", ".strtab", ".debug_info"] {
        assert!(!names.iter().any(|n| n == gone), "{gone} kept: {names:?}");
    }
    let elf = ElfFile64::<Endianness>::parse(&*bytes).unwrap();
    let dynsyms: Vec<_> = elf
        .dynamic_symbols()
        .map(|s| s.name().unwrap().to_string())
        .collect();
    assert!(dynsyms.iter().any(|n| n == "lib_g"), "{dynsyms:?}");

    // A program linked against the stripped library still links and runs.
    let rpath = format!("-Wl,-rpath,{}", dir.path().display());
    cc(dir.path(), &["-o", "main2", "main.c", "libx.so.1", &rpath]);
    run_prints(&dir.path().join("main2"), "10\n");
}

/// One System V archive member header with the given metadata.
fn ar_member(name: &str, mtime: u64, uid: u32, mode: u32, data: &[u8]) -> Vec<u8> {
    let mut v = Vec::new();
    for (field, width) in [
        (format!("{name}/"), 16),
        (mtime.to_string(), 12),
        (uid.to_string(), 6),
        (uid.to_string(), 6),
        (format!("{mode:o}"), 8),
        (data.len().to_string(), 10),
    ] {
        let mut f = field.into_bytes();
        f.resize(width, b' ');
        v.extend(f);
    }
    v.extend_from_slice(b"`\n");
    v.extend_from_slice(data);
    if data.len() % 2 == 1 {
        v.push(b'\n');
    }
    v
}

#[test]
fn test_strip_dh_strip_static_library() {
    let dir = TempDir::new().unwrap();
    fs::write(
        dir.path().join("lib.c"),
        format!(
            "{LIB_C}int __gnu_lto_slim = 1;\n\
             __attribute__((section(\".gnu.lto_x\"), used)) static const char lto[] = \"lto\";\n\
             __attribute__((section(\".note\"), used)) static const char note[] = \"note\";\n"
        ),
    )
    .unwrap();
    fs::write(dir.path().join("main.c"), MAIN_C).unwrap();
    cc(dir.path(), &["-g", "-c", "-o", "lib.o", "lib.c"]);
    let obj = fs::read(dir.path().join("lib.o")).unwrap();
    for want in [".comment", ".note", ".gnu.lto_x", ".debug_info"] {
        assert!(section_names(&obj).iter().any(|n| n == want), "no {want}");
    }
    assert!(has_file_symbol(&obj));

    // Non-deterministic member metadata, so the test sees -D zero it.
    let mut arc = b"!<arch>\n".to_vec();
    arc.extend(ar_member("lib.o", 1_700_000_000, 1000, 0o100755, &obj));
    let lib = dir.path().join("libx.a");
    fs::write(&lib, &arc).unwrap();

    strip_ok(&DH_STATIC, &lib);

    let bytes = fs::read(&lib).unwrap();
    let archive = object::read::archive::ArchiveFile::parse(&*bytes).unwrap();
    let member = archive.members().next().unwrap().unwrap();
    assert_eq!(member.name(), b"lib.o");
    assert_eq!(member.date(), Some(0));
    assert_eq!(member.uid(), Some(0));
    assert_eq!(member.gid(), Some(0));
    assert_eq!(member.mode(), Some(0o644));
    let data = member.data(&*bytes).unwrap();
    let names = section_names(data);
    for gone in [
        ".comment",
        ".note",
        ".gnu.lto_x",
        ".debug_info",
        ".rela.debug_info",
    ] {
        assert!(!names.iter().any(|n| n == gone), "{gone} kept: {names:?}");
    }
    assert!(names.iter().any(|n| n == ".note.GNU-stack"), "{names:?}");
    let syms = symbol_names(data);
    assert!(!syms.iter().any(|n| n == "__gnu_lto_slim"), "{syms:?}");
    assert!(syms.iter().any(|n| n == "lib_h"), "{syms:?}");
    // --strip-debug drops STT_FILE symbols, as GNU strip does.
    assert!(!has_file_symbol(data));

    cc(dir.path(), &["-o", "main", "main.c", "libx.a"]);
    run_prints(&dir.path().join("main"), "10\n");
}

#[test]
fn test_strip_symbol_named_in_relocation_is_kept() {
    // `-N` must not delete a symbol a kept relocation still names: that
    // would silently drop the relocation and corrupt the object.
    let dir = TempDir::new().unwrap();
    write_sources(dir.path());
    cc(dir.path(), &["-c", "-fPIC", "-o", "lib.o", "lib.c"]);
    let obj = dir.path().join("lib.o");
    let out = strip(&["-N", "lib_f"], &obj);
    assert!(out.status.success());
    assert!(
        String::from_utf8_lossy(&out.stderr).contains("named in a relocation"),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    let syms = symbol_names(&fs::read(&obj).unwrap());
    assert!(syms.iter().any(|n| n == "lib_f"), "{syms:?}");
    cc(dir.path(), &["-o", "main", "main.c", "lib.o"]);
    run_prints(&dir.path().join("main"), "10\n");
}

/// The names in an archive's symbol index.
fn archive_index(bytes: &[u8]) -> Vec<String> {
    let archive = object::read::archive::ArchiveFile::parse(bytes).unwrap();
    archive
        .symbols()
        .unwrap()
        .expect("archive must have a symbol index")
        .map(|s| String::from_utf8_lossy(s.unwrap().name()).into_owned())
        .collect()
}

#[test]
fn test_strip_archive_index_lists_only_global_symbols() {
    // The rewritten index listed static functions too, so a link could pull
    // a member in for a name it does not export.
    let dir = TempDir::new().unwrap();
    write_sources(dir.path());
    cc(dir.path(), &["-O0", "-c", "-o", "lib.o", "lib.c"]);
    let lib = dir.path().join("libx.a");
    let mut arc = b"!<arch>\n".to_vec();
    arc.extend(ar_member(
        "lib.o",
        0,
        0,
        0o644,
        &fs::read(dir.path().join("lib.o")).unwrap(),
    ));
    fs::write(&lib, &arc).unwrap();
    strip_ok(&DH_STATIC, &lib);
    let mut index = archive_index(&fs::read(&lib).unwrap());
    index.sort();
    assert_eq!(index, ["lib_f", "lib_g", "lib_h"]);
}

#[test]
fn test_strip_unneeded_relocatable_keeps_globals() {
    let dir = TempDir::new().unwrap();
    write_sources(dir.path());
    // -O0 keeps `hid` a real local function symbol.
    cc(dir.path(), &["-g", "-O0", "-c", "-o", "lib.o", "lib.c"]);
    let obj = dir.path().join("lib.o");
    assert!(symbol_names(&fs::read(&obj).unwrap())
        .iter()
        .any(|n| n == "hid"));
    strip_ok(&["--strip-unneeded"], &obj);
    let bytes = fs::read(&obj).unwrap();
    let syms = symbol_names(&bytes);
    for keep in ["lib_f", "lib_g", "lib_h"] {
        assert!(syms.iter().any(|n| n == keep), "{keep} lost: {syms:?}");
    }
    assert!(!has_file_symbol(&bytes));
    assert!(!section_names(&bytes)
        .iter()
        .any(|n| n.starts_with(".debug")));
    cc(dir.path(), &["-o", "main", "main.c", "lib.o"]);
    run_prints(&dir.path().join("main"), "10\n");
}

#[test]
fn test_strip_debug_executable_keeps_symtab() {
    let dir = TempDir::new().unwrap();
    build_shared(dir.path());
    let exe = dir.path().join("main");
    strip_ok(&["--strip-debug"], &exe);
    run_prints(&exe, "10\n");
    let bytes = fs::read(&exe).unwrap();
    assert!(!section_names(&bytes)
        .iter()
        .any(|n| n.starts_with(".debug")));
    assert!(symbol_names(&bytes).iter().any(|n| n == "main"));
    assert!(!has_file_symbol(&bytes));
}

/// A translation unit with a COMDAT group defining `grp_val`, as every C++
/// inline function or template instance gets.
fn comdat_source(func: &str) -> String {
    format!(
        "extern int grp_val;\nint {func}(void){{return grp_val;}}\n\
         __asm__(\".section .data.grp_val,\\\"awG\\\",%progbits,grp_val,comdat\\n\
         .globl grp_val\\n.type grp_val,%object\\n.size grp_val,4\\n\
         grp_val:\\n.long 7\\n.text\\n\");\n"
    )
}

#[test]
fn test_strip_keeps_comdat_groups() {
    // object's builder rejects SHT_GROUP sections, so strip failed on
    // every C++ object and static library. Each object here also carries
    // -g3 macro groups, which lose every member and must go, and losing
    // debug sections and symbols renumbers what the kept group names.
    let dir = TempDir::new().unwrap();
    fs::write(dir.path().join("a.c"), comdat_source("fa")).unwrap();
    fs::write(dir.path().join("b.c"), comdat_source("fb")).unwrap();
    fs::write(
        dir.path().join("main.c"),
        "#include <stdio.h>\nint fa(void);int fb(void);\n\
         int main(void){printf(\"%d\\n\", fa()+fb());return 0;}\n",
    )
    .unwrap();
    for (src, obj) in [("a.c", "a.o"), ("b.c", "b.o")] {
        cc(dir.path(), &["-g3", "-c", "-o", obj, src]);
        strip_ok(&[], &dir.path().join(obj));
        let bytes = fs::read(dir.path().join(obj)).unwrap();
        let elf = ElfFile64::<Endianness>::parse(&*bytes).unwrap();
        let groups: Vec<_> = elf
            .comdats()
            .map(|c| c.name().unwrap().to_string())
            .collect();
        assert_eq!(groups, ["grp_val"], "{obj}");
    }
    // Two copies of the group link as one only if each still names its
    // signature symbol and member section.
    cc(dir.path(), &["-o", "main", "main.c", "a.o", "b.o"]);
    run_prints(&dir.path().join("main"), "14\n");
}

#[test]
fn test_strip_rejects_strip_debug_with_strip_unneeded() {
    let dir = TempDir::new().unwrap();
    let f = dir.path().join("x.o");
    fs::write(&f, b"").unwrap();
    let out = strip(&["--strip-debug", "--strip-unneeded"], &f);
    assert!(!out.status.success());
}
