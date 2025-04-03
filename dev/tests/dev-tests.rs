use std::fs;

use object::{Object, ObjectSection, ObjectSymbol};
use plib::testing::{run_test, run_test_with_checker, TestPlan};

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
    run_test(TestPlan {
        cmd: "strings".to_string(),
        args: args.iter().map(|s| s.to_string()).collect(),
        stdin_data: "".to_string(),
        expected_out: stdout.to_string(),
        expected_err: "".to_string(),
        expected_exit_code: 0,
    });
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
fn test_strip_remove_all_non_section_symbols() {
    let stripped = strip_file(
        "tests/strip/remove_all_non_section_symbols.o",
        include_bytes!("strip/remove_all_non_section_symbols.o"),
    );
    let elf = object::read::elf::ElfFile64::<object::Endianness>::parse(&*stripped).unwrap();
    for symbol in elf.symbols() {
        assert_eq!(symbol.kind(), object::SymbolKind::Section);
    }
}

#[test]
fn test_strip_remove_all_relocations() {
    let stripped = strip_file(
        "tests/strip/remove_all_relocations.o",
        include_bytes!("strip/remove_all_relocations.o"),
    );
    let elf = object::read::elf::ElfFile64::<object::Endianness>::parse(&*stripped).unwrap();
    for section in elf.sections() {
        assert_eq!(section.relocations().count(), 0);
    }
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
    std::env::set_var("LC_CTYPE", "UTF-8");
    strings_test(
        &["tests/strings/utf8.bin"],
        include_str!("strings/utf8.correct.txt"),
    );
}

#[test]
fn test_strings_object_file() {
    strings_test(
        &["tests/strings/object.o"],
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
