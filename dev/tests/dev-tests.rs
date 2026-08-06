//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

mod lex;
mod yacc;

use object::{Object, ObjectSection, ObjectSymbol};
use plib::testing::{run_test, run_test_with_checker, TestPlan};
use std::fs;

fn ar_compare_test(
    args: &[&str],
    stdout: &str,
    stderr: &str,
    correct_contents: &[u8],
    previous_contents: &[u8],
    result_file: &str,
) {
    run_test(TestPlan {
        cmd: "ar".to_string(),
        args: args.iter().map(|s| s.to_string()).collect(),
        stdin_data: "".to_string(),
        expected_out: stdout.to_string(),
        expected_err: stderr.to_string(),
        expected_exit_code: 0,
    });
    let result = fs::read(result_file).expect("could not open result file");
    if previous_contents.is_empty() {
        fs::remove_file(result_file).expect("could not remove result file");
    } else {
        fs::write(result_file, previous_contents).expect("could not reset result file");
    }
    assert_eq!(result, correct_contents);
}

/// Tests the ar command with the given arguments and expected output.
/// It approximates equality by disregarding member metadata like date, uid, gid, and mode.
/// It's used for tests where the exact metadata is not important, for example for replacing members.
fn ar_compare_approx_test(
    args: &[&str],
    stdout: &str,
    stderr: &str,
    correct_contents: &[u8],
    previous_contents: &[u8],
    result_file: &str,
) {
    run_test(TestPlan {
        cmd: "ar".to_string(),
        args: args.iter().map(|s| s.to_string()).collect(),
        stdin_data: "".to_string(),
        expected_out: stdout.to_string(),
        expected_err: stderr.to_string(),
        expected_exit_code: 0,
    });

    let result = fs::read(result_file).expect("could not open result file");
    if previous_contents.is_empty() {
        fs::remove_file(result_file).expect("could not remove result file");
    } else {
        fs::write(result_file, previous_contents).expect("could not reset result file");
    }
    let archive = object::read::archive::ArchiveFile::parse(&*result).unwrap();
    let correct_archive = object::read::archive::ArchiveFile::parse(correct_contents).unwrap();
    for (entry, correct_entry) in archive.members().zip(correct_archive.members()) {
        let entry = entry.unwrap();
        let correct_entry = correct_entry.unwrap();

        assert_eq!(entry.name(), correct_entry.name());
        assert_eq!(entry.data(&*result), correct_entry.data(correct_contents));
    }
}

fn ar_print_test(args: &[&str], stdout_bytes: &[u8]) {
    run_test_with_checker(
        TestPlan {
            cmd: "ar".to_string(),
            args: args.iter().map(|s| s.to_string()).collect(),
            stdin_data: "".to_string(),
            expected_out: "".to_string(),
            expected_err: "".to_string(),
            expected_exit_code: 0,
        },
        |_, output| {
            assert!(output.status.success());
            assert_eq!(output.stdout, stdout_bytes);
        },
    );
}

fn strip_file(path: &str, previous_contents: &[u8]) -> Vec<u8> {
    run_test(TestPlan {
        cmd: "strip".to_string(),
        args: vec![path.to_string()],
        stdin_data: "".to_string(),
        expected_out: "".to_string(),
        expected_err: "".to_string(),
        expected_exit_code: 0,
    });
    let result = fs::read(path).expect("could not open result file");
    fs::write(path, previous_contents).expect("could not reset result file");
    result
}

fn strings_test(args: &[&str], stdout: &str) {
    strings_test_env(args, &[], stdout);
}

/// Like `strings_test` but injects per-invocation env vars into the spawned
/// `strings` subprocess. Used to pin `LC_CTYPE` for tests where the expected
/// output depends on `iswprint(3)`'s behavior (which differs between glibc
/// and Darwin for Latin-1 supplement codepoints). Goes through
/// `run_test_base_with_env` to avoid the racy `std::env::set_var` pattern.
fn strings_test_env(args: &[&str], env_vars: &[(&str, &str)], stdout: &str) {
    let args_v: Vec<String> = args.iter().map(|s| s.to_string()).collect();
    let output = plib::testing::run_test_base_with_env("strings", &args_v, b"", env_vars);
    assert_eq!(String::from_utf8_lossy(&output.stdout), stdout);
    assert_eq!(String::from_utf8_lossy(&output.stderr), "");
    assert_eq!(output.status.code(), Some(0));
    assert!(output.status.success());
}

#[test]
fn test_ar_delete_one() {
    ar_compare_test(
        &["-d", "tests/ar/delete_one.a", "lib3.o"],
        "",
        "",
        include_bytes!("ar/delete_one.correct.a"),
        include_bytes!("ar/delete_one.a"),
        "tests/ar/delete_one.a",
    );
}

#[test]
fn test_ar_delete_multiple() {
    ar_compare_test(
        &[
            "-d",
            "tests/ar/delete_multiple.a",
            "lib4.o",
            "lib2.o",
            "lib6.o",
        ],
        "",
        "",
        include_bytes!("ar/delete_multiple.correct.a"),
        include_bytes!("ar/delete_multiple.a"),
        "tests/ar/delete_multiple.a",
    );
}

#[test]
fn test_ar_delete_verbose() {
    ar_compare_test(
        &["-d", "-v", "tests/ar/delete_verbose.a", "lib1.o"],
        "d - lib1.o\n",
        "",
        include_bytes!("ar/delete_verbose.correct.a"),
        include_bytes!("ar/delete_verbose.a"),
        "tests/ar/delete_verbose.a",
    );
}

#[test]
fn test_ar_move_to_end() {
    ar_compare_test(
        &["-m", "tests/ar/move_to_end.a", "lib4.o", "lib2.o"],
        "",
        "",
        include_bytes!("ar/move_to_end.correct.a"),
        include_bytes!("ar/move_to_end.a"),
        "tests/ar/move_to_end.a",
    );
}

#[test]
fn test_ar_move_after() {
    ar_compare_test(
        &[
            "-m",
            "-a",
            "lib3.o",
            "tests/ar/move_after.a",
            "lib2.o",
            "lib4.o",
        ],
        "",
        "",
        include_bytes!("ar/move_after.correct.a"),
        include_bytes!("ar/move_after.a"),
        "tests/ar/move_after.a",
    );
}

#[test]
fn test_ar_move_before() {
    ar_compare_test(
        &[
            "-m",
            "-b",
            "lib3.o",
            "tests/ar/move_before.a",
            "lib2.o",
            "lib4.o",
        ],
        "",
        "",
        include_bytes!("ar/move_before.correct.a"),
        include_bytes!("ar/move_before.a"),
        "tests/ar/move_before.a",
    );
}

// -p
#[test]
fn test_ar_print_all() {
    ar_print_test(
        &["-p", "tests/ar/print.a"],
        include_bytes!("ar/print_all.correct"),
    );
}

#[test]
fn test_ar_print_some() {
    ar_print_test(
        &["-p", "tests/ar/print.a", "lib1.o", "lib5.o", "lib2.o"],
        include_bytes!("ar/print_some.correct"),
    );
}

#[test]
fn test_ar_print_verbose() {
    ar_print_test(
        &["-p", "-v", "tests/ar/print.a"],
        include_bytes!("ar/print_verbose.correct"),
    );
}

// -q
#[test]
fn test_ar_quick_append() {
    ar_compare_approx_test(
        &[
            "-q",
            "tests/ar/quick_append.a",
            "tests/ar/lib2.o",
            "tests/ar/lib6.o",
        ],
        "",
        "",
        include_bytes!("ar/quick_append.correct.a"),
        include_bytes!("ar/quick_append.a"),
        "tests/ar/quick_append.a",
    );
}

#[test]
fn test_ar_quick_append_create_archive() {
    ar_compare_approx_test(
        &[
            "-q",
            "tests/ar/quick_append_create.a",
            "tests/ar/lib1.o",
            "tests/ar/lib4.o",
        ],
        "",
        "ar: creating tests/ar/quick_append_create.a\n",
        include_bytes!("ar/quick_append_create.correct.a"),
        &[],
        "tests/ar/quick_append_create.a",
    )
}

#[test]
fn test_ar_quick_append_create_archive_dont_report_creation() {
    ar_compare_approx_test(
        &[
            "-q",
            "-c",
            "tests/ar/quick_append_create2.a",
            "tests/ar/lib1.o",
            "tests/ar/lib4.o",
        ],
        "",
        "",
        include_bytes!("ar/quick_append_create.correct.a"),
        &[],
        "tests/ar/quick_append_create2.a",
    )
}

#[test]
fn test_ar_replace() {
    ar_compare_approx_test(
        &[
            "-r",
            "tests/ar/replace.a",
            "tests/ar/lib7.o",
            "tests/ar/lib8.o",
        ],
        "",
        "",
        include_bytes!("ar/replace.correct.a"),
        include_bytes!("ar/replace.a"),
        "tests/ar/replace.a",
    );
}

#[test]
fn test_ar_replace_verbose() {
    ar_compare_approx_test(
        &[
            "-r",
            "-v",
            "tests/ar/replace_verbose.a",
            "tests/ar/lib7.o",
            "tests/ar/lib8.o",
        ],
        "r - tests/ar/lib7.o\nr - tests/ar/lib8.o\n",
        "",
        include_bytes!("ar/replace.correct.a"),
        include_bytes!("ar/replace_verbose.a"),
        "tests/ar/replace_verbose.a",
    );
}

#[test]
fn test_ar_replace_missing() {
    ar_compare_approx_test(
        &[
            "-r",
            "tests/ar/replace_missing.a",
            "tests/ar/lib2.o",
            "tests/ar/lib6.o",
        ],
        "",
        "",
        include_bytes!("ar/replace_missing.correct.a"),
        include_bytes!("ar/replace_missing.a"),
        "tests/ar/replace_missing.a",
    );
}

#[test]
fn test_ar_replace_missing_insert_before() {
    ar_compare_approx_test(
        &[
            "-r",
            "-b",
            "lib3.o",
            "tests/ar/replace_missing_insert_before.a",
            "tests/ar/lib2.o",
            "tests/ar/lib6.o",
        ],
        "",
        "",
        include_bytes!("ar/replace_missing_insert_before.correct.a"),
        include_bytes!("ar/replace_missing_insert_before.a"),
        "tests/ar/replace_missing_insert_before.a",
    );
}

#[test]
fn test_ar_replace_missing_insert_after() {
    ar_compare_approx_test(
        &[
            "-r",
            "-a",
            "lib3.o",
            "tests/ar/replace_missing_insert_after.a",
            "tests/ar/lib2.o",
            "tests/ar/lib6.o",
        ],
        "",
        "",
        include_bytes!("ar/replace_missing_insert_after.correct.a"),
        include_bytes!("ar/replace_missing_insert_after.a"),
        "tests/ar/replace_missing_insert_after.a",
    );
}

#[test]
fn test_ar_replace_missing_verbose() {
    ar_compare_approx_test(
        &[
            "-r",
            "-v",
            "tests/ar/replace_missing_verbose.a",
            "tests/ar/lib2.o",
            "tests/ar/lib6.o",
        ],
        "a - tests/ar/lib2.o\na - tests/ar/lib6.o\n",
        "",
        include_bytes!("ar/replace_missing.correct.a"),
        include_bytes!("ar/replace_missing_verbose.a"),
        "tests/ar/replace_missing_verbose.a",
    );
}

#[test]
fn test_ar_replace_create_archive() {
    ar_compare_approx_test(
        &[
            "-r",
            "tests/ar/replace_create1.a",
            "tests/ar/lib1.o",
            "tests/ar/lib4.o",
        ],
        "",
        "ar: creating tests/ar/replace_create1.a\n",
        include_bytes!("ar/replace_create.correct.a"),
        &[],
        "tests/ar/replace_create1.a",
    );
}

#[test]
fn test_ar_replace_create_archive_dont_report_creation() {
    ar_compare_approx_test(
        &[
            "-r",
            "-c",
            "tests/ar/replace_create2.a",
            "tests/ar/lib1.o",
            "tests/ar/lib4.o",
        ],
        "",
        "",
        include_bytes!("ar/replace_create.correct.a"),
        &[],
        "tests/ar/replace_create2.a",
    );
}

#[test]
fn test_ar_list() {
    run_test(TestPlan {
        cmd: "ar".to_string(),
        args: vec!["-t".to_string(), "tests/ar/list.a".to_string()],
        stdin_data: "".to_string(),
        expected_out: "lib1.o\nlib4.o\nlib5.o\n".to_string(),
        expected_err: "".to_string(),
        expected_exit_code: 0,
    });
}

#[test]
fn test_ar_list_some() {
    run_test(TestPlan {
        cmd: "ar".to_string(),
        args: vec![
            "-t".to_string(),
            "tests/ar/list.a".to_string(),
            "lib4.o".to_string(),
        ],
        stdin_data: "".to_string(),
        expected_out: "lib4.o\n".to_string(),
        expected_err: "".to_string(),
        expected_exit_code: 0,
    });
}

#[test]
fn test_strip_stripped_elf_is_valid_elf() {
    let stripped = strip_file(
        "tests/strip/stripped_elf_is_valid_elf.o",
        include_bytes!("strip/stripped_elf_is_valid_elf.o"),
    );
    object::read::elf::ElfFile64::<object::Endianness>::parse(&*stripped).expect("invalid ELF");
}

#[test]
fn test_strip_stripped_archive_contains_valid_elf_members() {
    let stripped = strip_file(
        "tests/strip/stripped_archive_contains_valid_elf_members.a",
        include_bytes!("strip/stripped_archive_contains_valid_elf_members.a"),
    );
    let archive = object::read::archive::ArchiveFile::parse(&*stripped).unwrap();
    for member in archive.members() {
        let member = member.unwrap();
        object::read::elf::ElfFile64::<object::Endianness>::parse(
            member.data(&*stripped).expect("could not read member data"),
        )
        .expect("invalid ELF");
    }
}

#[test]
fn test_strip_keeps_symbols_in_relocatable() {
    // #ST4: a relocatable object (.o) must remain linkable, so stripping keeps
    // its symbol table (only debug info is removed). Previously this test
    // pinned the over-aggressive removal of all non-section symbols.
    let stripped = strip_file(
        "tests/strip/remove_all_non_section_symbols.o",
        include_bytes!("strip/remove_all_non_section_symbols.o"),
    );
    let elf = object::read::elf::ElfFile64::<object::Endianness>::parse(&*stripped).unwrap();
    let non_section = elf
        .symbols()
        .filter(|s| s.kind() != object::SymbolKind::Section)
        .count();
    assert!(
        non_section > 0,
        "a stripped .o must retain its (non-section) symbols to stay linkable"
    );
}

#[test]
fn test_strip_keeps_relocations_in_relocatable() {
    // #ST4: relocations must be preserved on a relocatable object, else it
    // becomes unlinkable. Previously this test pinned their removal.
    let stripped = strip_file(
        "tests/strip/remove_all_relocations.o",
        include_bytes!("strip/remove_all_relocations.o"),
    );
    let elf = object::read::elf::ElfFile64::<object::Endianness>::parse(&*stripped).unwrap();
    let total_relocs: usize = elf.sections().map(|s| s.relocations().count()).sum();
    assert!(
        total_relocs > 0,
        "a stripped .o must retain its relocations to stay linkable"
    );
}

// ELF-specific: cc produces Mach-O on macOS, which strip (intentionally) does
// not support, so this test only runs on Linux.
#[cfg(target_os = "linux")]
#[test]
fn test_strip_executable_removes_symtab() {
    // The aggressive path still applies to executables (ET_EXEC): the symbol
    // table is removed. Build a non-PIE executable so there is no .rela.dyn to
    // confuse the check, strip it, and confirm .symtab is gone.
    let dir = tempfile::TempDir::new().unwrap();
    let src = dir.path().join("a.c");
    let exe = dir.path().join("a.out");
    fs::write(&src, "int main(void){return 0;}\n").unwrap();
    let built = std::process::Command::new("cc")
        .args([
            "-no-pie",
            "-o",
            exe.to_str().unwrap(),
            src.to_str().unwrap(),
        ])
        .status()
        .expect("failed to run cc");
    assert!(built.success(), "cc must build the executable");

    let out = std::process::Command::new(env!("CARGO_BIN_EXE_strip"))
        .arg(&exe)
        .output()
        .expect("failed to run strip");
    assert!(
        out.status.success(),
        "strip failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );

    let bytes = fs::read(&exe).unwrap();
    let elf = object::read::elf::ElfFile64::<object::Endianness>::parse(&*bytes)
        .expect("stripped executable must still be a valid ELF");
    assert!(
        elf.section_by_name(".symtab").is_none(),
        "an executable's .symtab must be stripped"
    );
}

// ELF/GNU-ar specific: on macOS cc emits Mach-O and `ar` writes the BSD format,
// so this exercises behavior that only applies on Linux.
#[cfg(target_os = "linux")]
#[test]
fn test_strip_preserves_non_elf_archive_members() {
    // #ST1: a non-ELF archive member must survive stripping unmodified rather
    // than being silently dropped.
    let dir = tempfile::TempDir::new().unwrap();
    let cpath = dir.path().join("o.c");
    let opath = dir.path().join("o.o");
    fs::write(&cpath, "int f(void){return 1;}\n").unwrap();
    assert!(std::process::Command::new("cc")
        .args(["-c", "-o", opath.to_str().unwrap(), cpath.to_str().unwrap()])
        .status()
        .expect("cc")
        .success());
    let txt = dir.path().join("readme.txt");
    fs::write(&txt, "hello not an object\n").unwrap();
    let arc = dir.path().join("mixed.a");
    assert!(std::process::Command::new("ar")
        .args([
            "rc",
            arc.to_str().unwrap(),
            opath.to_str().unwrap(),
            txt.to_str().unwrap()
        ])
        .status()
        .expect("ar")
        .success());

    let out = std::process::Command::new(env!("CARGO_BIN_EXE_strip"))
        .arg(&arc)
        .output()
        .expect("failed to run strip");
    assert!(
        out.status.success(),
        "strip failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );

    let bytes = fs::read(&arc).unwrap();
    let archive = object::read::archive::ArchiveFile::parse(&*bytes).unwrap();
    let mut names = Vec::new();
    let mut text_data = None;
    for m in archive.members() {
        let m = m.unwrap();
        let name = String::from_utf8_lossy(m.name()).to_string();
        if name == "readme.txt" {
            text_data = Some(m.data(&*bytes).unwrap().to_vec());
        }
        names.push(name);
    }
    assert!(
        names.iter().any(|n| n == "o.o"),
        "ELF member must remain, got {:?}",
        names
    );
    assert_eq!(
        text_data.as_deref(),
        Some(b"hello not an object\n".as_slice()),
        "non-ELF member must be preserved unmodified, got members {:?}",
        names
    );
}

#[cfg(target_os = "linux")]
#[test]
fn test_strip_preserves_long_archive_member_names() {
    // #ST10: a member name longer than the 16-byte header field must be
    // written to a System V "//" string table, not truncated. The `ar` crate
    // resolves long names on read, so truncating on write silently renamed
    // the member and produced an archive the linker could not match.
    const LONG: &str = "a_very_long_member_name.o";
    assert!(LONG.len() > 16, "fixture name must exceed the header field");

    let dir = tempfile::TempDir::new().unwrap();
    let cpath = dir.path().join("s.c");
    let opath = dir.path().join(LONG);
    fs::write(&cpath, "int a_symbol_here(void){return 1;}\n").unwrap();
    assert!(std::process::Command::new("cc")
        .args(["-c", "-o", opath.to_str().unwrap(), cpath.to_str().unwrap()])
        .status()
        .expect("cc")
        .success());

    let arc = dir.path().join("lib.a");
    assert!(std::process::Command::new(env!("CARGO_BIN_EXE_ar"))
        .args(["-r", "-c", arc.to_str().unwrap(), opath.to_str().unwrap()])
        .status()
        .expect("ar")
        .success());

    let out = std::process::Command::new(env!("CARGO_BIN_EXE_strip"))
        .arg(&arc)
        .output()
        .expect("failed to run strip");
    assert!(
        out.status.success(),
        "strip failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );

    let bytes = fs::read(&arc).unwrap();
    let archive = object::read::archive::ArchiveFile::parse(&*bytes)
        .expect("stripped archive must still parse");
    let names: Vec<String> = archive
        .members()
        .map(|m| String::from_utf8_lossy(m.unwrap().name()).to_string())
        .collect();
    assert!(
        names.iter().any(|n| n == LONG),
        "long member name must survive stripping intact, got {:?}",
        names
    );
}

#[test]
fn test_strip_rejects_bsd_variant_archive() {
    // #ST11: `!<arch>\n` is shared by the System V and BSD layouts. Our writer
    // only speaks System V, so a BSD archive (the macOS default) must be
    // refused rather than silently rewritten into a malformed hybrid.
    let dir = tempfile::TempDir::new().unwrap();
    let arc = dir.path().join("bsd.a");

    // Minimal BSD archive: the `#1/<len>` header form stores the member name
    // inline, immediately after the header, and counts it in the size field.
    let name = b"a_long_bsd_member_name.o";
    let mut padded_name = name.to_vec();
    while padded_name.len() % 8 != 0 {
        padded_name.push(0);
    }
    let payload = b"\x7fELF fake object payload";

    let mut header = Vec::new();
    header.extend_from_slice(format!("#1/{}", padded_name.len()).as_bytes());
    header.resize(16, b' ');
    let field = |v: String, n: usize| {
        let mut f = v.into_bytes();
        f.resize(n, b' ');
        f
    };
    header.extend(field("0".into(), 12)); // mtime
    header.extend(field("0".into(), 6)); // uid
    header.extend(field("0".into(), 6)); // gid
    header.extend(field("100644".into(), 8)); // mode
    header.extend(field((padded_name.len() + payload.len()).to_string(), 10));
    header.extend_from_slice(b"`\n");
    assert_eq!(header.len(), 60, "archive member header must be 60 bytes");

    let mut bytes = b"!<arch>\n".to_vec();
    bytes.extend_from_slice(&header);
    bytes.extend_from_slice(&padded_name);
    bytes.extend_from_slice(payload);
    fs::write(&arc, &bytes).unwrap();

    let out = std::process::Command::new(env!("CARGO_BIN_EXE_strip"))
        .arg(&arc)
        .output()
        .expect("failed to run strip");
    assert!(
        !out.status.success(),
        "strip must reject a BSD-variant archive rather than corrupt it"
    );
    let err = String::from_utf8_lossy(&out.stderr);
    assert!(
        err.contains("BSD"),
        "diagnostic must name the unsupported variant: {}",
        err
    );
    // The input must be left untouched by a refused run.
    assert_eq!(
        fs::read(&arc).unwrap(),
        bytes,
        "a refused archive must not be modified"
    );
}

#[test]
fn test_strip_removes_all_debug_sections() {
    const DEBUG_SECTIONS: [&[u8]; 7] = [
        b".debug",
        b".gnu.debuglto_.debug_",
        b".gnu.linkonce.wi.",
        b".zdebug",
        b".line",
        b".stab",
        b".gdb_index",
    ];
    let stripped = strip_file(
        "tests/strip/remove_all_debug_sections.o",
        include_bytes!("strip/remove_all_debug_sections.o"),
    );
    let elf = object::read::elf::ElfFile64::<object::Endianness>::parse(&*stripped).unwrap();
    for section in elf.sections() {
        for debug_section in &DEBUG_SECTIONS {
            assert!(!section.name_bytes().unwrap().starts_with(debug_section));
        }
    }
}

#[test]
fn test_strip_rejects_unsupported_format() {
    // #ST3: a non-ELF, non-archive file is rejected with a clear non-zero exit.
    let dir = tempfile::TempDir::new().unwrap();
    let f = dir.path().join("plain.txt");
    fs::write(&f, "not an object file\n").unwrap();
    let out = std::process::Command::new(env!("CARGO_BIN_EXE_strip"))
        .arg(&f)
        .output()
        .expect("failed to run strip");
    assert!(
        !out.status.success(),
        "strip must reject an unsupported format"
    );
    assert!(
        String::from_utf8_lossy(&out.stderr).contains("unsupported file format"),
        "diagnostic should name the limitation: {}",
        String::from_utf8_lossy(&out.stderr)
    );
}

#[test]
fn test_strip_requires_operand() {
    // #ST7: at least one file operand is required.
    let out = std::process::Command::new(env!("CARGO_BIN_EXE_strip"))
        .output()
        .expect("failed to run strip");
    assert!(!out.status.success(), "strip with no operand must fail");
}

#[test]
fn test_strings_empty_file() {
    strings_test(&["tests/strings/empty.txt"], "");
}

#[test]
fn test_strings_print_one() {
    strings_test(
        &["tests/strings/one.txt"],
        include_str!("strings/one.correct.txt"),
    );
}

#[test]
fn test_strings_print_multiple() {
    strings_test(
        &["tests/strings/multiple.bin"],
        include_str!("strings/multiple.correct.txt"),
    );
}

#[test]
fn test_strings_utf8_file() {
    // POSIX-style locale name (the bare "UTF-8" used historically isn't a
    // valid locale on glibc and silently falls back to "C"; C.UTF-8 is
    // universally available since glibc 2.35 / FreeBSD 11).
    strings_test_env(
        &["tests/strings/utf8.bin"],
        &[("LC_CTYPE", "C.UTF-8")],
        include_str!("strings/utf8.correct.txt"),
    );
}

#[test]
fn test_strings_offsets_are_file_relative_for_objects() {
    // #S10: POSIX 115906 defines -t's offset as "from the start of the file".
    // Scanning an object file section-by-section previously restarted the
    // count at 0 for every section, so offsets were section-relative.
    // Self-validating: each reported offset must actually locate that string
    // in the file, which pins the property without hardcoding numbers.
    let path = "tests/strings/object.o";
    let bytes = fs::read(path).expect("object fixture");
    let args = vec!["-t".to_string(), "d".to_string(), path.to_string()];
    let out = plib::testing::run_test_base_with_env(
        "strings",
        &args,
        b"",
        &[("LC_CTYPE", "C"), ("LC_ALL", "C"), ("LANG", "C")],
    );
    assert!(out.status.success());
    let stdout = String::from_utf8_lossy(&out.stdout);
    let mut checked = 0;
    for line in stdout.lines() {
        let (off, text) = line
            .split_once(' ')
            .expect("`-t d` emits \"<offset> <string>\"");
        let off: usize = off.parse().expect("offset must be decimal");
        assert!(
            off + text.len() <= bytes.len(),
            "offset {} for {:?} runs past the {}-byte file",
            off,
            text,
            bytes.len()
        );
        assert_eq!(
            &bytes[off..off + text.len()],
            text.as_bytes(),
            "offset {} does not locate {:?} in the file (section-relative?)",
            off,
            text
        );
        checked += 1;
    }
    assert!(checked > 1, "fixture should yield several strings");
}

#[test]
fn test_strings_minimum_length_counts_characters_not_bytes() {
    // #S9: POSIX 115860 counts printable *characters*. Three multi-byte
    // characters span nine bytes, so a byte-based comparison wrongly cleared
    // a `-n 4` threshold. GNU strings prints nothing here.
    let dir = tempfile::TempDir::new().unwrap();
    let f = dir.path().join("mb.bin");
    // Three EURO SIGNs (U+20AC): 3 characters, 9 bytes.
    fs::write(&f, "\u{20ac}\u{20ac}\u{20ac}\n").unwrap();

    let args = vec!["-n".to_string(), "4".to_string(), f.display().to_string()];
    let out = plib::testing::run_test_base_with_env(
        "strings",
        &args,
        b"",
        &[("LC_CTYPE", "C.UTF-8"), ("LC_ALL", "C.UTF-8")],
    );
    assert!(out.status.success());
    assert_eq!(
        String::from_utf8_lossy(&out.stdout),
        "",
        "a 3-character run must not satisfy -n 4 even though it spans 9 bytes"
    );

    // ...and four of them must still print, proving the threshold still works.
    fs::write(&f, "\u{20ac}\u{20ac}\u{20ac}\u{20ac}\n").unwrap();
    let out = plib::testing::run_test_base_with_env(
        "strings",
        &args,
        b"",
        &[("LC_CTYPE", "C.UTF-8"), ("LC_ALL", "C.UTF-8")],
    );
    assert_eq!(
        String::from_utf8_lossy(&out.stdout),
        "\u{20ac}\u{20ac}\u{20ac}\u{20ac}\n"
    );
}

#[test]
fn test_strings_newline_terminates_a_string() {
    // #S2: POSIX 115860 -- a printable string is terminated by <newline> or
    // NUL. The audit flagged that no fixture contained an embedded newline,
    // so nothing pinned this behavior.
    let dir = tempfile::TempDir::new().unwrap();
    let f = dir.path().join("nl.bin");
    fs::write(&f, b"abcd\nefgh\nijkl\n").unwrap();

    let args = vec![f.display().to_string()];
    let out = plib::testing::run_test_base_with_env(
        "strings",
        &args,
        b"",
        &[("LC_CTYPE", "C"), ("LC_ALL", "C")],
    );
    assert!(out.status.success());
    assert_eq!(
        String::from_utf8_lossy(&out.stdout),
        "abcd\nefgh\nijkl\n",
        "each newline must end the current string rather than be absorbed into it"
    );
}

#[test]
fn test_strings_object_file() {
    // Pin LC_CTYPE so iswprint behavior is identical on glibc and Darwin.
    // The object file contains valid-UTF-8 Latin-1 supplement byte runs
    // (e.g. `~À`, `nÀÃ`, `üÿÿÿ`) that macOS's default locale considers
    // printable but the C locale does not. Pinning to "C" keeps only the
    // ASCII strings, matching object.correct.txt across platforms.
    strings_test_env(
        &["tests/strings/object.o"],
        &[("LC_CTYPE", "C"), ("LC_ALL", "C"), ("LANG", "C")],
        include_str!("strings/object.correct.txt"),
    );
}

#[test]
fn test_strings_print_shorter_than_default_length() {
    strings_test(
        &["tests/strings/short.bin", "-n", "2"],
        include_str!("strings/short.correct.txt"),
    );
}

#[test]
fn test_strings_print_longer_than_default_length() {
    strings_test(
        &["tests/strings/long.bin", "-n", "7"],
        include_str!("strings/long.correct.txt"),
    );
}

#[test]
fn test_strings_print_with_decimal_offset() {
    strings_test(
        &["-t", "d", "tests/strings/with_offset.bin"],
        include_str!("strings/with_decimal_offset.correct.txt"),
    );
}

#[test]
fn test_strings_print_with_hex_offset() {
    strings_test(
        &["-t", "x", "tests/strings/with_offset.bin"],
        include_str!("strings/with_hex_offset.correct.txt"),
    );
}

#[test]
fn test_strings_print_with_octal_offset() {
    strings_test(
        &["-t", "o", "tests/strings/with_offset.bin"],
        include_str!("strings/with_octal_offset.correct.txt"),
    );
}

#[test]
fn test_strings_reads_stdin_when_no_operand() {
    // POSIX: with no file operand, strings reads standard input.
    let input = b"\x00\x00hello\x00\x00world\x00";
    let output = plib::testing::run_test_base("strings", &[], input);
    assert_eq!(String::from_utf8_lossy(&output.stdout), "hello\nworld\n");
    assert!(output.status.success());
    assert_eq!(String::from_utf8_lossy(&output.stderr), "");
}

#[test]
fn test_strings_rejects_n_zero() {
    // POSIX OPTIONS: -n requires a positive integer.
    let args = vec![
        "-n".to_string(),
        "0".to_string(),
        "tests/strings/one.txt".to_string(),
    ];
    let output = plib::testing::run_test_base("strings", &args, b"");
    assert!(!output.status.success(), "-n 0 must be rejected");
}

#[test]
fn test_strings_continues_after_missing_file() {
    // A missing file is reported on stderr but the run continues to the next
    // operand and exits non-zero.
    let args = vec![
        "tests/strings/does_not_exist_xyz".to_string(),
        "tests/strings/one.txt".to_string(),
    ];
    let output = plib::testing::run_test_base("strings", &args, b"");
    assert!(
        String::from_utf8_lossy(&output.stdout).contains("string"),
        "the valid file's strings must still be printed"
    );
    assert!(
        !output.status.success(),
        "a failed file must yield non-zero exit"
    );
    assert!(String::from_utf8_lossy(&output.stderr).contains("strings:"));
}

#[test]
fn test_ar_tv_date_uses_mtime_not_age() {
    // #A1: the archive date field is the member's mtime (Unix epoch), so
    // `ar -tv` shows the real year, not 1970 (the old file-age bug).
    use std::io::Write;
    use std::time::{Duration, SystemTime};
    let dir = tempfile::TempDir::new().unwrap();
    let f = dir.path().join("member.txt");
    {
        let mut file = fs::File::create(&f).unwrap();
        file.write_all(b"hello world payload").unwrap();
        // 1_600_000_000 = 2020-09-13 UTC.
        file.set_modified(SystemTime::UNIX_EPOCH + Duration::from_secs(1_600_000_000))
            .unwrap();
    }
    let arc = dir.path().join("a.a");
    let create = std::process::Command::new(env!("CARGO_BIN_EXE_ar"))
        .args(["-r", "-c", arc.to_str().unwrap(), f.to_str().unwrap()])
        .output()
        .expect("run ar -r");
    assert!(
        create.status.success(),
        "ar -r failed: {}",
        String::from_utf8_lossy(&create.stderr)
    );

    let tv = std::process::Command::new(env!("CARGO_BIN_EXE_ar"))
        .args(["-t", "-v", arc.to_str().unwrap()])
        .env("TZ", "UTC")
        .output()
        .expect("run ar -t -v");
    assert!(
        tv.status.success(),
        "ar -t -v failed: {}",
        String::from_utf8_lossy(&tv.stderr)
    );
    let out = String::from_utf8_lossy(&tv.stdout);
    assert!(
        out.contains("2020"),
        "ar -tv date must reflect the member mtime (2020): {}",
        out
    );
    assert!(
        !out.contains("1970"),
        "ar -tv date must not show the 1970 file-age bug: {}",
        out
    );
}

#[test]
fn test_ar_delete_matches_basename() {
    // #A2: a file operand is matched to a member by its last pathname
    // component, so `ar -d arc some/dir/foo.o` deletes the member `foo.o`.
    let dir = tempfile::TempDir::new().unwrap();
    let f = dir.path().join("foo.o");
    fs::write(&f, b"member contents").unwrap();
    let arc = dir.path().join("a.a");
    assert!(std::process::Command::new(env!("CARGO_BIN_EXE_ar"))
        .args(["-r", "-c", arc.to_str().unwrap(), f.to_str().unwrap()])
        .output()
        .expect("run ar -r")
        .status
        .success());

    let del = std::process::Command::new(env!("CARGO_BIN_EXE_ar"))
        .args(["-d", arc.to_str().unwrap(), "some/dir/foo.o"])
        .output()
        .expect("run ar -d");
    assert!(
        del.status.success(),
        "ar -d failed: {}",
        String::from_utf8_lossy(&del.stderr)
    );

    let bytes = fs::read(&arc).unwrap();
    let archive = object::read::archive::ArchiveFile::parse(&*bytes).unwrap();
    assert_eq!(
        archive.members().count(),
        0,
        "the basename-matched member should have been deleted"
    );
}

#[test]
fn test_ar_bundled_mode_flags() {
    // #A3: grouped mode+modifier flags like -rc/-tv/-dv must work (XBD 12.2).
    let dir = tempfile::TempDir::new().unwrap();
    let f = dir.path().join("m.o");
    fs::write(&f, b"some object bytes").unwrap();
    let arc = dir.path().join("b.a");

    let create = std::process::Command::new(env!("CARGO_BIN_EXE_ar"))
        .args(["-rc", arc.to_str().unwrap(), f.to_str().unwrap()])
        .output()
        .expect("run ar -rc");
    assert!(
        create.status.success(),
        "ar -rc (bundled) must work: {}",
        String::from_utf8_lossy(&create.stderr)
    );

    let tv = std::process::Command::new(env!("CARGO_BIN_EXE_ar"))
        .args(["-tv", arc.to_str().unwrap()])
        .output()
        .expect("run ar -tv");
    assert!(
        tv.status.success(),
        "ar -tv (bundled) must work: {}",
        String::from_utf8_lossy(&tv.stderr)
    );
    assert!(String::from_utf8_lossy(&tv.stdout).contains("m.o"));

    let dv = std::process::Command::new(env!("CARGO_BIN_EXE_ar"))
        .args(["-dv", arc.to_str().unwrap(), "m.o"])
        .output()
        .expect("run ar -dv");
    assert!(
        dv.status.success(),
        "ar -dv (bundled) must work: {}",
        String::from_utf8_lossy(&dv.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&dv.stdout), "d - m.o\n");
}

/// Build an archive from `(name, contents)` pairs and return (dir, archive path).
fn ar_make_archive(members: &[(&str, &[u8])]) -> (tempfile::TempDir, std::path::PathBuf) {
    let dir = tempfile::TempDir::new().unwrap();
    let mut argv = vec!["-r".to_string(), "-c".to_string()];
    let arc = dir.path().join("a.a");
    argv.push(arc.to_str().unwrap().to_string());
    for (name, data) in members {
        let p = dir.path().join(name);
        fs::write(&p, data).unwrap();
        argv.push(p.to_str().unwrap().to_string());
    }
    let out = std::process::Command::new(env!("CARGO_BIN_EXE_ar"))
        .args(&argv)
        .output()
        .expect("run ar -rc");
    assert!(
        out.status.success(),
        "ar -rc failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    (dir, arc)
}

#[test]
fn test_ar_odd_length_member_roundtrip() {
    // #A13: the header `size` field is the payload size; the 2-byte alignment
    // pad is not part of the member. Counting the pad made `ar -x` write an
    // extra trailing newline for every odd-length member.
    let payload: &[u8] = b"odd";
    assert_eq!(payload.len() % 2, 1, "fixture must be odd-length");
    let (dir, arc) = ar_make_archive(&[("odd.txt", payload), ("even.txt", b"even")]);

    let xdir = dir.path().join("x");
    fs::create_dir(&xdir).unwrap();
    let x = std::process::Command::new(env!("CARGO_BIN_EXE_ar"))
        .args(["-x", arc.to_str().unwrap()])
        .current_dir(&xdir)
        .output()
        .expect("run ar -x");
    assert!(
        x.status.success(),
        "ar -x failed: {}",
        String::from_utf8_lossy(&x.stderr)
    );

    let extracted = fs::read(xdir.join("odd.txt")).unwrap();
    assert_eq!(
        extracted,
        payload,
        "odd-length member must extract byte-identical (got {} bytes, want {})",
        extracted.len(),
        payload.len()
    );
    assert_eq!(fs::read(xdir.join("even.txt")).unwrap(), b"even");
}

#[test]
fn test_ar_odd_length_member_size_field_excludes_pad() {
    // #A13, at the byte level: the ASCII size field must read the payload
    // length, with the pad newline following the payload uncounted.
    let (_dir, arc) = ar_make_archive(&[("odd.txt", b"odd")]);
    let raw = fs::read(&arc).unwrap();
    let text = String::from_utf8_lossy(&raw);
    let hdr = text
        .find("odd.txt/")
        .expect("member header must be present in the archive");
    // Header layout after the 16-byte name: 12 date + 6 uid + 6 gid + 8 mode,
    // then the 10-byte size field.
    let size_at = hdr + 16 + 12 + 6 + 6 + 8;
    let size_field = text[size_at..size_at + 10].trim();
    assert_eq!(
        size_field, "3",
        "size field must be the unpadded payload length, got {:?}",
        size_field
    );
}

#[test]
fn test_ar_repeated_rewrite_is_stable() {
    // #A13: mis-counting the pad byte made each rewrite re-read the pad as
    // payload, growing every odd-length member by one newline per pass.
    let (dir, arc) = ar_make_archive(&[("odd.txt", b"odd")]);
    let extra = dir.path().join("extra.txt");
    fs::write(&extra, b"even").unwrap();

    // Sample the size *before* the first rewrite: the pad byte is absorbed on
    // pass one and the archive is stable thereafter, so a post-rewrite-only
    // sample would miss the growth entirely.
    let mut sizes = vec![fs::metadata(&arc).unwrap().len()];
    for _ in 0..3 {
        let out = std::process::Command::new(env!("CARGO_BIN_EXE_ar"))
            .args(["-r", arc.to_str().unwrap(), extra.to_str().unwrap()])
            .output()
            .expect("run ar -r");
        assert!(
            out.status.success(),
            "ar -r failed: {}",
            String::from_utf8_lossy(&out.stderr)
        );
        sizes.push(fs::metadata(&arc).unwrap().len());
    }
    // extra.txt is added once on the first pass, so allow that one delta and
    // require every later pass to be a no-op in size terms.
    assert!(
        sizes[1..].iter().all(|s| *s == sizes[1]),
        "archive size must not grow across repeated rewrites: {:?}",
        sizes
    );

    // The odd-length member must still be exactly its original payload.
    let xdir = dir.path().join("x");
    fs::create_dir(&xdir).unwrap();
    let x = std::process::Command::new(env!("CARGO_BIN_EXE_ar"))
        .args(["-x", arc.to_str().unwrap(), "odd.txt"])
        .current_dir(&xdir)
        .output()
        .expect("run ar -x");
    assert!(
        x.status.success(),
        "ar -x failed: {}",
        String::from_utf8_lossy(&x.stderr)
    );
    assert_eq!(
        fs::read(xdir.join("odd.txt")).unwrap(),
        b"odd",
        "odd-length member must survive repeated archive rewrites unchanged"
    );
}

#[test]
fn test_ar_list_missing_operand_names_the_operand() {
    // #A14: `ar -t arc nosuch.o` must name the missing operand, not the
    // archive (which exists and was read successfully).
    let (_dir, arc) = ar_make_archive(&[("present.o", b"data")]);
    let out = std::process::Command::new(env!("CARGO_BIN_EXE_ar"))
        .args(["-t", arc.to_str().unwrap(), "nosuch.o"])
        .output()
        .expect("run ar -t");
    assert!(!out.status.success(), "missing operand must fail");
    let err = String::from_utf8_lossy(&out.stderr);
    assert!(
        err.contains("nosuch.o"),
        "diagnostic must name the missing operand: {}",
        err
    );
}

#[test]
fn test_ar_move_verbose() {
    // #A5: ar -m -v reports each moved member as "m - <name>".
    let dir = tempfile::TempDir::new().unwrap();
    let a = dir.path().join("a.o");
    let b = dir.path().join("b.o");
    fs::write(&a, b"aaaa").unwrap();
    fs::write(&b, b"bbbb").unwrap();
    let arc = dir.path().join("mv.a");
    assert!(std::process::Command::new(env!("CARGO_BIN_EXE_ar"))
        .args([
            "-rc",
            arc.to_str().unwrap(),
            a.to_str().unwrap(),
            b.to_str().unwrap()
        ])
        .output()
        .expect("ar -rc")
        .status
        .success());

    let mv = std::process::Command::new(env!("CARGO_BIN_EXE_ar"))
        .args(["-m", "-v", arc.to_str().unwrap(), "a.o"])
        .output()
        .expect("ar -m -v");
    assert!(
        mv.status.success(),
        "ar -m -v failed: {}",
        String::from_utf8_lossy(&mv.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&mv.stdout), "m - a.o\n");
}

#[test]
fn test_ar_extract_long_name_truncation() {
    // #A4: extracting a member whose name exceeds NAME_MAX errors by default
    // and is truncated with -T. Build the long-name archive with the ar crate
    // (a real file with a 300-byte name can't exist on disk).
    let dir = tempfile::TempDir::new().unwrap();
    let longname = format!("{}.o", "x".repeat(300));
    let data = b"member-data";
    let arc = dir.path().join("long.a");
    {
        let f = fs::File::create(&arc).unwrap();
        let mut builder = ar::GnuBuilder::new(f, vec![longname.clone().into_bytes()]);
        let header = ar::Header::new(longname.clone().into_bytes(), data.len() as u64);
        builder.append(&header, &data[..]).unwrap();
    }

    // Without -T: error, no file created.
    let no_t = std::process::Command::new(env!("CARGO_BIN_EXE_ar"))
        .args(["-x", arc.to_str().unwrap()])
        .current_dir(dir.path())
        .output()
        .expect("ar -x");
    assert!(
        !no_t.status.success(),
        "extract of an over-long name must fail without -T"
    );
    assert!(
        String::from_utf8_lossy(&no_t.stderr).contains("too long"),
        "diagnostic should mention the name is too long: {}",
        String::from_utf8_lossy(&no_t.stderr)
    );

    // With -T: name is truncated to NAME_MAX and the file is created.
    let with_t = std::process::Command::new(env!("CARGO_BIN_EXE_ar"))
        .args(["-x", "-T", arc.to_str().unwrap()])
        .current_dir(dir.path())
        .output()
        .expect("ar -x -T");
    assert!(
        with_t.status.success(),
        "extract with -T must succeed: {}",
        String::from_utf8_lossy(&with_t.stderr)
    );
    // The first 255 bytes of the name are all 'x'.
    let truncated = dir.path().join("x".repeat(255));
    assert!(
        truncated.exists(),
        "the truncated-name file should have been created"
    );
}

#[test]
fn test_ar_long_member_name() {
    // #A6: a member name longer than 15 bytes is stored via a "//" long-name
    // table (POSIX: valid filenames must not be rejected), and round-trips
    // through list/parse/extract.
    let dir = tempfile::TempDir::new().unwrap();
    let longname = "this_is_a_long_member_name.o"; // 28 bytes > 15
    let f = dir.path().join(longname);
    fs::write(&f, b"payload-bytes-here").unwrap();
    let arc = dir.path().join("l.a");
    let create = std::process::Command::new(env!("CARGO_BIN_EXE_ar"))
        .args(["-rc", arc.to_str().unwrap(), f.to_str().unwrap()])
        .output()
        .expect("ar -rc");
    assert!(
        create.status.success(),
        "ar -rc failed for a long name: {}",
        String::from_utf8_lossy(&create.stderr)
    );

    // ar -t lists the full name.
    let t = std::process::Command::new(env!("CARGO_BIN_EXE_ar"))
        .args(["-t", arc.to_str().unwrap()])
        .output()
        .expect("ar -t");
    assert!(t.status.success());
    assert_eq!(
        String::from_utf8_lossy(&t.stdout),
        format!("{}\n", longname)
    );

    // The archive parses and the member name/data are preserved.
    let bytes = fs::read(&arc).unwrap();
    let archive = object::read::archive::ArchiveFile::parse(&*bytes).unwrap();
    let m = archive.members().next().unwrap().unwrap();
    assert_eq!(m.name(), longname.as_bytes());
    assert_eq!(m.data(&*bytes).unwrap(), b"payload-bytes-here");

    // Extract recreates the file under its full name.
    let exdir = dir.path().join("ex");
    fs::create_dir(&exdir).unwrap();
    let x = std::process::Command::new(env!("CARGO_BIN_EXE_ar"))
        .args(["-x", arc.to_str().unwrap()])
        .current_dir(&exdir)
        .output()
        .expect("ar -x");
    assert!(
        x.status.success(),
        "ar -x failed: {}",
        String::from_utf8_lossy(&x.stderr)
    );
    assert_eq!(
        fs::read(exdir.join(longname)).unwrap(),
        b"payload-bytes-here"
    );
}

#[test]
fn test_ar_print_verbose_uses_operand_prefix() {
    // #A8: with file operands, the -pv prefix is the operand, not the member.
    let dir = tempfile::TempDir::new().unwrap();
    let f = dir.path().join("m.o");
    fs::write(&f, b"PAYLOAD").unwrap();
    let arc = dir.path().join("p.a");
    assert!(std::process::Command::new(env!("CARGO_BIN_EXE_ar"))
        .args(["-rc", arc.to_str().unwrap(), f.to_str().unwrap()])
        .output()
        .expect("ar -rc")
        .status
        .success());

    // Operand with a directory prefix; matches member m.o by basename.
    let pv = std::process::Command::new(env!("CARGO_BIN_EXE_ar"))
        .args(["-p", "-v", arc.to_str().unwrap(), "sub/m.o"])
        .output()
        .expect("ar -p -v");
    assert!(pv.status.success());
    let out = String::from_utf8_lossy(&pv.stdout);
    assert!(
        out.contains("<sub/m.o>"),
        "the -pv prefix must be the operand, got: {}",
        out
    );
    assert!(out.contains("PAYLOAD"));
}

#[test]
fn test_ar_tv_shows_setuid_bit() {
    // #A9: the -tv mode column renders setuid/setgid/sticky like ls.
    use std::os::unix::fs::PermissionsExt;
    let dir = tempfile::TempDir::new().unwrap();
    let f = dir.path().join("s.o");
    fs::write(&f, b"x").unwrap();
    fs::set_permissions(&f, fs::Permissions::from_mode(0o4755)).unwrap();
    let arc = dir.path().join("s.a");
    assert!(std::process::Command::new(env!("CARGO_BIN_EXE_ar"))
        .args(["-rc", arc.to_str().unwrap(), f.to_str().unwrap()])
        .output()
        .expect("ar -rc")
        .status
        .success());

    let tv = std::process::Command::new(env!("CARGO_BIN_EXE_ar"))
        .args(["-tv", arc.to_str().unwrap()])
        .output()
        .expect("ar -tv");
    assert!(tv.status.success());
    let out = String::from_utf8_lossy(&tv.stdout);
    assert!(
        out.contains("rwsr-xr-x"),
        "setuid bit must render as 's': {}",
        out
    );
}

#[test]
fn test_ar_replace_no_files_errors() {
    // #A11: -r with no file operands is a clear error, not a silent no-op or a
    // misleading "missing archive operand".
    let dir = tempfile::TempDir::new().unwrap();
    let arc = dir.path().join("r.a");
    let out = std::process::Command::new(env!("CARGO_BIN_EXE_ar"))
        .args(["-r", arc.to_str().unwrap()])
        .output()
        .expect("ar -r");
    assert!(
        !out.status.success(),
        "ar -r with no file operands must error"
    );
    assert!(
        String::from_utf8_lossy(&out.stderr).contains("no file operands"),
        "diagnostic should mention missing file operands: {}",
        String::from_utf8_lossy(&out.stderr)
    );
}

// ---------------------------------------------------------------------------
// nm tests. Fixtures are compiled at test time with cc (no committed binaries).
// ---------------------------------------------------------------------------

fn nm_compile_obj(dir: &std::path::Path, name: &str, src: &str) -> std::path::PathBuf {
    let c = dir.join(format!("{}.c", name));
    let o = dir.join(format!("{}.o", name));
    fs::write(&c, src).unwrap();
    let ok = std::process::Command::new("cc")
        .args(["-c", "-o", o.to_str().unwrap(), c.to_str().unwrap()])
        .status()
        .expect("run cc")
        .success();
    assert!(ok, "cc must compile the nm fixture");
    o
}

const NM_SRC: &str = r#"
int alpha_global = 1;
int zeta_global = 2;
int mid_func(void) { return 0; }
extern int undef_sym(void);
int use_undef(void) { return undef_sym(); }
"#;

fn nm_run(args: &[&str]) -> std::process::Output {
    std::process::Command::new(env!("CARGO_BIN_EXE_nm"))
        .args(args)
        .output()
        .expect("run nm")
}

/// The type letter of an nm default-format line ("value type name").
fn nm_line_type(line: &str) -> &str {
    let f: Vec<&str> = line.split_whitespace().collect();
    f[f.len() - 2]
}

fn nm_line_name(line: &str) -> &str {
    line.split_whitespace().last().unwrap()
}

#[test]
fn test_nm_default_sorted_by_name() {
    // #N5: default output is sorted by symbol name.
    let dir = tempfile::TempDir::new().unwrap();
    let obj = nm_compile_obj(dir.path(), "t", NM_SRC);
    let out = nm_run(&[obj.to_str().unwrap()]);
    assert!(out.status.success());
    let stdout = String::from_utf8_lossy(&out.stdout);
    let names: Vec<&str> = stdout.lines().map(nm_line_name).collect();
    let mut sorted = names.clone();
    sorted.sort_unstable();
    assert_eq!(names, sorted, "default nm output must be sorted by name");
    // Mach-O prefixes user symbols with '_', so match on a suffix.
    assert!(
        names.iter().any(|n| n.ends_with("alpha_global"))
            && names.iter().any(|n| n.ends_with("undef_sym")),
        "expected symbols not found: {:?}",
        names
    );
}

#[test]
fn test_nm_portable_format() {
    // #N3/#N6/#N13: -P emits "name type value size".
    let dir = tempfile::TempDir::new().unwrap();
    let obj = nm_compile_obj(dir.path(), "t", NM_SRC);
    let out = nm_run(&["-P", obj.to_str().unwrap()]);
    assert!(out.status.success());
    let stdout = String::from_utf8_lossy(&out.stdout);
    // Match the symbol-name field by suffix (Mach-O prepends '_').
    let line = stdout
        .lines()
        .find(|l| {
            l.split_whitespace()
                .next()
                .is_some_and(|n| n.ends_with("mid_func"))
        })
        .expect("mid_func line");
    let fields: Vec<&str> = line.split_whitespace().collect();
    assert!(fields[0].ends_with("mid_func"));
    assert_eq!(fields[1], "T", "a defined global function is type T");
    assert_eq!(fields.len(), 4, "defined -P line has 4 fields: {}", line);
}

#[test]
fn test_nm_value_sort() {
    // #N9: -v sorts by value (ascending).
    let dir = tempfile::TempDir::new().unwrap();
    let obj = nm_compile_obj(dir.path(), "t", NM_SRC);
    let out = nm_run(&["-v", "-P", "-t", "d", obj.to_str().unwrap()]);
    assert!(out.status.success());
    let stdout = String::from_utf8_lossy(&out.stdout);
    let values: Vec<u64> = stdout
        .lines()
        .filter_map(|l| {
            let f: Vec<&str> = l.split_whitespace().collect();
            if f.len() == 4 {
                f[2].parse().ok()
            } else {
                None
            }
        })
        .collect();
    let mut sorted = values.clone();
    sorted.sort_unstable();
    assert_eq!(values, sorted, "-v output must be sorted by value");
}

#[test]
fn test_nm_global_filter() {
    // #N8: -g keeps only global symbols (uppercase type letters).
    let dir = tempfile::TempDir::new().unwrap();
    let obj = nm_compile_obj(dir.path(), "t", NM_SRC);
    let out = nm_run(&["-g", obj.to_str().unwrap()]);
    assert!(out.status.success());
    let stdout = String::from_utf8_lossy(&out.stdout);
    for line in stdout.lines() {
        let t = nm_line_type(line);
        assert!(
            t.chars().all(|c| c.is_ascii_uppercase()),
            "-g must keep only globals: {}",
            line
        );
    }
    assert!(stdout.contains("mid_func"));
}

#[test]
fn test_nm_undefined_filter() {
    // #N8: -u keeps only undefined symbols.
    let dir = tempfile::TempDir::new().unwrap();
    let obj = nm_compile_obj(dir.path(), "t", NM_SRC);
    let out = nm_run(&["-u", obj.to_str().unwrap()]);
    assert!(out.status.success());
    let stdout = String::from_utf8_lossy(&out.stdout);
    for line in stdout.lines() {
        assert_eq!(
            nm_line_type(line),
            "U",
            "-u must keep only undefined: {}",
            line
        );
    }
    assert!(stdout.contains("undef_sym"));
}

#[test]
fn test_nm_print_name_prefix() {
    // #N4: -A prefixes every line with the object pathname.
    let dir = tempfile::TempDir::new().unwrap();
    let obj = nm_compile_obj(dir.path(), "t", NM_SRC);
    let out = nm_run(&["-A", obj.to_str().unwrap()]);
    assert!(out.status.success());
    let stdout = String::from_utf8_lossy(&out.stdout);
    let prefix = format!("{}: ", obj.display());
    for line in stdout.lines() {
        assert!(line.starts_with(&prefix), "-A prefix missing: {}", line);
    }
}

#[test]
fn test_nm_multiple_files_headers() {
    // #N11: multiple operands produce a "file:" header per file.
    let dir = tempfile::TempDir::new().unwrap();
    let a = nm_compile_obj(dir.path(), "first", NM_SRC);
    let b = nm_compile_obj(dir.path(), "second", "int only_here = 9;\n");
    let out = nm_run(&[a.to_str().unwrap(), b.to_str().unwrap()]);
    assert!(out.status.success());
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(stdout.contains(&format!("{}:", a.display())));
    assert!(stdout.contains(&format!("{}:", b.display())));
}

#[test]
fn test_nm_archive_input() {
    // #N2: nm reads .a archives and emits a "[member]:" stanza per member.
    let dir = tempfile::TempDir::new().unwrap();
    let obj = nm_compile_obj(dir.path(), "t", NM_SRC);
    let arc = dir.path().join("lib.a");
    assert!(std::process::Command::new(env!("CARGO_BIN_EXE_ar"))
        .args(["-rc", arc.to_str().unwrap(), obj.to_str().unwrap()])
        .output()
        .expect("ar -rc")
        .status
        .success());
    let out = nm_run(&[arc.to_str().unwrap()]);
    assert!(
        out.status.success(),
        "nm on archive failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(
        stdout.contains("[t.o]:"),
        "per-member header missing: {}",
        stdout
    );
    assert!(stdout.contains("mid_func"));
}

#[test]
fn test_nm_octal_radix() {
    // #N6/#N7: -t o formats numeric columns in octal (no digits 8/9).
    let dir = tempfile::TempDir::new().unwrap();
    let obj = nm_compile_obj(dir.path(), "t", NM_SRC);
    let out = nm_run(&["-P", "-t", "o", obj.to_str().unwrap()]);
    assert!(out.status.success());
    let stdout = String::from_utf8_lossy(&out.stdout);
    for line in stdout.lines() {
        let f: Vec<&str> = line.split_whitespace().collect();
        if f.len() == 4 {
            for col in [f[2], f[3]] {
                assert!(
                    !col.contains('8') && !col.contains('9'),
                    "octal column has a non-octal digit: {}",
                    line
                );
            }
        }
    }
}

#[test]
fn test_nm_requires_operand() {
    // #N1: file operand is required.
    let out = std::process::Command::new(env!("CARGO_BIN_EXE_nm"))
        .output()
        .expect("run nm");
    assert!(!out.status.success(), "nm with no operand must fail");
}
