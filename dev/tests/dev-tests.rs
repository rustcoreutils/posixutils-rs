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
    let output = plib::testing::run_test_base("strings", &vec![], input);
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
