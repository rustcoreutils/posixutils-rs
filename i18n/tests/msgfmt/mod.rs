//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test, run_test_with_checker, TestPlan};
use plib::tmp::TempDir;
use std::fs::{self, File};
use std::io::Write;
use std::path::PathBuf;
use std::process::Output;

/// Create a temporary .po file for testing
fn create_temp_po_file(content: &str) -> (TempDir, PathBuf) {
    let temp_dir = TempDir::new().unwrap();
    let po_path = temp_dir.path().join("test.po");
    let mut file = File::create(&po_path).unwrap();
    write!(file, "{}", content).unwrap();
    (temp_dir, po_path)
}

/// Test msgfmt with simple .po file
#[test]
fn test_msgfmt_simple() {
    let po_content = r#"
msgid ""
msgstr ""
"Content-Type: text/plain; charset=UTF-8\n"

msgid "Hello"
msgstr "Hola"
"#;

    let (temp_dir, po_path) = create_temp_po_file(po_content);
    let mo_path = temp_dir.path().join("test.mo");

    run_test(TestPlan {
        cmd: String::from("msgfmt"),
        args: vec![
            String::from("-o"),
            mo_path.to_str().unwrap().to_string(),
            po_path.to_str().unwrap().to_string(),
        ],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    // Verify the .mo file was created
    assert!(mo_path.exists(), "Output .mo file should exist");

    // Check it has valid .mo magic number
    let data = fs::read(&mo_path).unwrap();
    assert!(data.len() >= 4, "MO file should have at least 4 bytes");
    // Magic number should be 0x950412de (little-endian)
    assert_eq!(data[0], 0xde);
    assert_eq!(data[1], 0x12);
    assert_eq!(data[2], 0x04);
    assert_eq!(data[3], 0x95);
}

/// Test msgfmt with plural forms
#[test]
fn test_msgfmt_plural() {
    let po_content = r#"
msgid ""
msgstr ""
"Content-Type: text/plain; charset=UTF-8\n"
"Plural-Forms: nplurals=2; plural=(n != 1);\n"

msgid "One file"
msgid_plural "%d files"
msgstr[0] "Un archivo"
msgstr[1] "%d archivos"
"#;

    let (temp_dir, po_path) = create_temp_po_file(po_content);
    let mo_path = temp_dir.path().join("plural.mo");

    run_test(TestPlan {
        cmd: String::from("msgfmt"),
        args: vec![
            String::from("-o"),
            mo_path.to_str().unwrap().to_string(),
            po_path.to_str().unwrap().to_string(),
        ],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    assert!(mo_path.exists(), "Output .mo file should exist");
}

/// Test msgfmt skips fuzzy entries by default
#[test]
fn test_msgfmt_skip_fuzzy() {
    let po_content = r#"
msgid ""
msgstr ""
"Content-Type: text/plain; charset=UTF-8\n"

#, fuzzy
msgid "Fuzzy message"
msgstr "Mensaje difuso"

msgid "Normal message"
msgstr "Mensaje normal"
"#;

    let (temp_dir, po_path) = create_temp_po_file(po_content);
    let mo_path = temp_dir.path().join("fuzzy.mo");

    run_test(TestPlan {
        cmd: String::from("msgfmt"),
        args: vec![
            String::from("-o"),
            mo_path.to_str().unwrap().to_string(),
            po_path.to_str().unwrap().to_string(),
        ],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    assert!(mo_path.exists());
}

/// Test msgfmt with -f flag includes fuzzy entries
#[test]
fn test_msgfmt_include_fuzzy() {
    let po_content = r#"
msgid ""
msgstr ""
"Content-Type: text/plain; charset=UTF-8\n"

#, fuzzy
msgid "Fuzzy message"
msgstr "Mensaje difuso"
"#;

    let (temp_dir, po_path) = create_temp_po_file(po_content);
    let mo_path = temp_dir.path().join("with_fuzzy.mo");

    run_test(TestPlan {
        cmd: String::from("msgfmt"),
        args: vec![
            String::from("-f"),
            String::from("-o"),
            mo_path.to_str().unwrap().to_string(),
            po_path.to_str().unwrap().to_string(),
        ],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    assert!(mo_path.exists());
}

/// MF-1: a `domain` directive must not be silently dropped. With `-o`, all
/// domains are merged into the single output file (the directives are ignored),
/// so a message from a non-default domain section is still present.
#[test]
fn test_msgfmt_multidomain_merged_with_o() {
    let po_content = r#"
msgid ""
msgstr ""
"Content-Type: text/plain; charset=UTF-8\n"

msgid "Hello"
msgstr "Hola"

domain "other"

msgid "Goodbye"
msgstr "Adios"
"#;

    let (temp_dir, po_path) = create_temp_po_file(po_content);
    let mo_path = temp_dir.path().join("merged.mo");

    run_test(TestPlan {
        cmd: String::from("msgfmt"),
        args: vec![
            String::from("-o"),
            mo_path.to_str().unwrap().to_string(),
            po_path.to_str().unwrap().to_string(),
        ],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    let data = fs::read(&mo_path).unwrap();
    // Both the default-domain and "other"-domain translations are present.
    let needle_hola = b"Hola";
    let needle_adios = b"Adios";
    assert!(
        data.windows(needle_hola.len()).any(|w| w == needle_hola),
        "merged .mo should contain the default-domain translation"
    );
    assert!(
        data.windows(needle_adios.len()).any(|w| w == needle_adios),
        "merged .mo should contain the 'other'-domain translation (domain not dropped)"
    );
}

/// MF-2/MF-9: with `-c -v`, a c-format argument-type mismatch is an abnormality
/// and yields a non-zero exit status.
#[test]
fn test_msgfmt_cformat_mismatch_fails() {
    let po_content = r#"
msgid ""
msgstr ""
"Content-Type: text/plain; charset=UTF-8\n"

#, c-format
msgid "Hello %s"
msgstr "Hola %d"
"#;
    let (temp_dir, po_path) = create_temp_po_file(po_content);
    let mo_path = temp_dir.path().join("bad.mo");
    run_test_with_checker(
        TestPlan {
            cmd: String::from("msgfmt"),
            args: vec![
                String::from("-c"),
                String::from("-v"),
                String::from("-o"),
                mo_path.to_str().unwrap().to_string(),
                po_path.to_str().unwrap().to_string(),
            ],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 1,
        },
        |_plan, output: &Output| {
            assert_eq!(output.status.code(), Some(1));
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(stderr.contains("format specifications"), "{stderr:?}");
        },
    );
}

/// MF-4: `-c` without `-v` runs no abnormality checks, so the same input that
/// fails under `-c -v` compiles successfully (exit 0).
#[test]
fn test_msgfmt_check_without_verbose_is_noop() {
    let po_content = r#"
msgid ""
msgstr ""
"Content-Type: text/plain; charset=UTF-8\n"

#, c-format
msgid "Hello %s"
msgstr "Hola %d"
"#;
    let (temp_dir, po_path) = create_temp_po_file(po_content);
    let mo_path = temp_dir.path().join("ok.mo");
    run_test(TestPlan {
        cmd: String::from("msgfmt"),
        args: vec![
            String::from("-c"),
            String::from("-o"),
            mo_path.to_str().unwrap().to_string(),
            po_path.to_str().unwrap().to_string(),
        ],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

/// MF-12: with no input file operand, msgfmt prints a usage diagnostic and
/// exits non-zero.
#[test]
fn test_msgfmt_no_input_file() {
    run_test(TestPlan {
        cmd: String::from("msgfmt"),
        args: vec![],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::from("msgfmt: no input file given\n"),
        expected_exit_code: 1,
    });
}

/// Test msgfmt with empty .po file
#[test]
fn test_msgfmt_empty() {
    let po_content = r#"
msgid ""
msgstr ""
"Content-Type: text/plain; charset=UTF-8\n"
"#;

    let (temp_dir, po_path) = create_temp_po_file(po_content);
    let mo_path = temp_dir.path().join("empty.mo");

    run_test(TestPlan {
        cmd: String::from("msgfmt"),
        args: vec![
            String::from("-o"),
            mo_path.to_str().unwrap().to_string(),
            po_path.to_str().unwrap().to_string(),
        ],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    assert!(mo_path.exists());
}

// ============================================================================
// LC_CTYPE-driven byte interpretation (cross-cutting theme 4)
// ============================================================================

/// A `.po` file is text in whatever codeset `LC_CTYPE` describes. The parser
/// read lines into a `String`, so a valid Latin-1 (or EUC-JP, or Shift-JIS)
/// catalog was rejected outright, and the bytes must now reach the `.mo`
/// unchanged.
#[test]
fn test_msgfmt_preserves_non_utf8_message_bytes() {
    let temp_dir = TempDir::new().unwrap();
    let po_path = temp_dir.path().join("latin1.po");
    let mo_path = temp_dir.path().join("latin1.mo");

    // "caf<E9>" in Latin-1; 0xE9 is not valid UTF-8 on its own.
    let mut po = Vec::new();
    po.extend_from_slice(b"msgid \"cafe\"\nmsgstr \"caf\xe9\"\n");
    fs::write(&po_path, &po).unwrap();

    let status = std::process::Command::new(plib::testing::get_binary_path("msgfmt"))
        .arg("-o")
        .arg(&mo_path)
        .arg(&po_path)
        .status()
        .unwrap();
    assert!(status.success(), "msgfmt rejected a Latin-1 .po file");

    let mo = fs::read(&mo_path).unwrap();
    assert!(
        mo.windows(4).any(|w| w == b"caf\xe9"),
        "the message bytes must reach the .mo verbatim, not re-encoded"
    );
    assert!(
        !mo.windows(2).any(|w| w == b"\xc3\xa9"),
        "0xE9 was UTF-8 encoded into two bytes"
    );
}

/// `\xNN` and `\ooo` name a *byte*. Pushing them through `char` UTF-8 encoded
/// every value above 0x7F into two bytes on the way into the `.mo`.
#[test]
fn test_msgfmt_high_escapes_are_single_bytes() {
    let temp_dir = TempDir::new().unwrap();
    let po_path = temp_dir.path().join("esc.po");
    let mo_path = temp_dir.path().join("esc.mo");
    // \xe9 and \351 are both 0xE9.
    fs::write(&po_path, "msgid \"a\"\nmsgstr \"\\xe9\\351\"\n").unwrap();

    let status = std::process::Command::new(plib::testing::get_binary_path("msgfmt"))
        .arg("-o")
        .arg(&mo_path)
        .arg(&po_path)
        .status()
        .unwrap();
    assert!(status.success());

    let mo = fs::read(&mo_path).unwrap();
    assert!(
        mo.windows(2).any(|w| w == b"\xe9\xe9"),
        r"\xe9 and \351 must each expand to the single byte 0xE9"
    );
}

/// Run msgfmt with `args` in `dir`; return (stderr, exit code).
fn msgfmt_status(dir: &std::path::Path, args: &[&str]) -> (String, i32) {
    let out = std::process::Command::new(plib::testing::get_binary_path("msgfmt"))
        .args(args)
        .current_dir(dir)
        .output()
        .expect("msgfmt");
    (
        String::from_utf8_lossy(&out.stderr).into_owned(),
        out.status.code().unwrap_or(-1),
    )
}

/// po4a checks every PO file with
/// `msgfmt --check-format --check-domain -o /dev/null FILE`.
#[test]
fn test_msgfmt_po4a_check_accepts_a_good_file() {
    let (dir, po_path) = create_temp_po_file(
        r#"
msgid ""
msgstr ""
"Content-Type: text/plain; charset=UTF-8\n"

#, c-format
msgid "Hello %s, %d files"
msgstr "Hola %s, %i archivos"

msgid "No format %d here"
msgstr "Sin formato"
"#,
    );
    let po = po_path.to_str().unwrap();
    let args = ["--check-format", "--check-domain", "-o", "/dev/null", po];
    assert_eq!(msgfmt_status(dir.path(), &args), (String::new(), 0));
}

/// --check-format alone, without -c -v, checks c-format directives.
#[test]
fn test_msgfmt_check_format_rejects_a_mismatch() {
    let (dir, po_path) = create_temp_po_file(
        r#"
msgid ""
msgstr ""
"Content-Type: text/plain; charset=UTF-8\n"

#, c-format
msgid "Hello %s"
msgstr "Hola %d"
"#,
    );
    let po = po_path.to_str().unwrap();
    let (err, code) = msgfmt_status(dir.path(), &["--check-format", "-o", "/dev/null", po]);
    assert_eq!(code, 1, "{err}");
    assert!(err.contains("format specifications"), "{err:?}");
}

/// --check-domain: a `domain` directive conflicts with -o, which ignores it.
/// Without -o the directive names the output file, and nothing conflicts.
#[test]
fn test_msgfmt_check_domain_rejects_a_directive_under_o() {
    let (dir, po_path) = create_temp_po_file(
        r#"
domain "other"

msgid "Hello"
msgstr "Hola"
"#,
    );
    let po = po_path.to_str().unwrap();
    let (err, code) = msgfmt_status(dir.path(), &["--check-domain", "-o", "/dev/null", po]);
    assert_eq!(code, 1, "{err}");
    assert!(err.contains("'domain other' directive ignored"), "{err:?}");

    assert_eq!(
        msgfmt_status(dir.path(), &["--check-domain", po]),
        (String::new(), 0)
    );
    assert!(dir.path().join("other").exists());
}

// XBD 12.2, Guideline 7: an option-argument may begin with '-'. Each option
// below used to have the word after it refused as an unknown option.
#[test]
fn option_argument_may_begin_with_hyphen() {
    for opt in ["-D", "-o"] {
        plib::testing::assert_hyphen_option_argument("msgfmt", &[opt, "-zq", "--help"]);
    }
}

/// gettext's configure keeps a msgfmt only if
/// `msgfmt --statistics /dev/null` succeeds; an empty input writes no catalog.
#[test]
fn test_msgfmt_statistics_on_empty_input() {
    let dir = TempDir::new().unwrap();
    assert_eq!(
        msgfmt_status(dir.path(), &["--statistics", "/dev/null"]),
        ("0 translated messages.\n".to_string(), 0)
    );
    assert_eq!(fs::read_dir(dir.path()).unwrap().count(), 0);
}

/// --statistics counts as GNU msgfmt does, singular for a count of one.
#[test]
fn test_msgfmt_statistics_counts() {
    let (dir, po_path) = create_temp_po_file(
        r#"
msgid ""
msgstr ""
"Content-Type: text/plain; charset=UTF-8\n"

msgid "a"
msgstr "A"

#, fuzzy
msgid "b"
msgstr "B"

msgid "c"
msgstr ""

msgid "d"
msgstr "D"
"#,
    );
    let po = po_path.to_str().unwrap();
    assert_eq!(
        msgfmt_status(dir.path(), &["--statistics", "-o", "/dev/null", po]),
        (
            "2 translated messages, 1 fuzzy translation, 1 untranslated message.\n".to_string(),
            0
        )
    );
}

/// `--verbose` is GNU's long spelling of -v. gettext's stock po/Makefile.in.in builds each
/// catalog with `msgfmt -c --statistics --verbose -o xx.gmo xx.po`; the counts are written once.
#[test]
fn test_msgfmt_long_verbose() {
    let (dir, po_path) = create_temp_po_file(
        r#"
msgid ""
msgstr ""
"Content-Type: text/plain; charset=UTF-8\n"

msgid "a"
msgstr "A"

msgid "c"
msgstr ""
"#,
    );
    let po = po_path.to_str().unwrap();
    let counts = "1 translated message, 1 untranslated message.\n";
    for args in [
        ["--verbose", "-o", "/dev/null", po, "", ""],
        ["-c", "--statistics", "--verbose", "-o", "xx.gmo", po],
    ] {
        let args: Vec<&str> = args.into_iter().filter(|a| !a.is_empty()).collect();
        let (err, status) = msgfmt_status(dir.path(), &args);
        assert_eq!(status, 0, "{err}");
        assert_eq!(err.matches(counts).count(), 1, "{err}");
        assert!(err.ends_with(counts), "{err}");
        // Everything else written is what -v writes.
        let short: Vec<&str> = args
            .iter()
            .map(|a| if *a == "--verbose" { "-v" } else { a })
            .collect();
        assert_eq!(msgfmt_status(dir.path(), &short), (err, 0));
    }
    assert!(dir.path().join("xx.gmo").exists());
}

/// With -c -v, POSIX compares only the number of conversion specifications
/// and the argument types of corresponding ones: a flag such as `'` (or
/// GNU's `I`) changes neither, and a `%n$` conversion is matched by its
/// argument number, not by where it stands in the text. Each failure names
/// the file, the line of the msgstr and the msgid. Verdicts are GNU
/// msgfmt's.
#[test]
fn test_msgfmt_check_compares_arguments_not_spelling() {
    for (msgid, msgstr, refusal) in [
        ("%d", "%'d", None),
        ("%d %d", "%Id %d", None),
        ("%d of %s", "%2$s ... %1$d", None),
        ("%1$d of %2$s", "%2$s ... %1$d", None),
        ("%*d", "%*d", None),
        ("%5.2f%%", "%f %%", None),
        (
            "a %d b %s",
            "x %s y %d",
            Some("for argument 1 are not the same"),
        ),
        (
            "%d of %s",
            "%2$d ... %1$s",
            Some("for argument 1 are not the same"),
        ),
        (
            "%d of %s",
            "%2$s ... %d",
            Some("both through absolute argument numbers"),
        ),
        ("%d", "%d %d", Some("number of format specifications")),
        (
            "%d of %s",
            "%2$s",
            Some("refers to argument number 2 but ignores argument number 1"),
        ),
        ("%ld", "%d", Some("for argument 1 are not the same")),
        ("%*d", "%d", Some("number of format specifications")),
    ] {
        let (dir, po_path) = create_temp_po_file(&format!(
            "msgid \"\"\nmsgstr \"Content-Type: text/plain; charset=UTF-8\\n\"\n\n\
             #, c-format\nmsgid \"{msgid}\"\nmsgstr \"{msgstr}\"\n"
        ));
        let po = po_path.to_str().unwrap();
        let (err, code) = msgfmt_status(dir.path(), &["-c", "-v", "-o", "/dev/null", po]);
        match refusal {
            None => assert_eq!(code, 0, "{msgid:?} / {msgstr:?}: {err}"),
            Some(reason) => {
                assert_eq!(code, 1, "{msgid:?} / {msgstr:?}: {err}");
                assert!(err.contains(&format!("{po}:6: error: ")), "{err:?}");
                assert!(err.contains(reason), "{msgid:?} / {msgstr:?}: {err:?}");
                assert!(err.contains(&format!("msgid \"{msgid}\"")), "{err:?}");
            }
        }
    }
}

/// GNU msgfmt's plural check (NONPOSIX.md): with -c -v every plural form is
/// checked against msgid_plural, and a form that stands for fewer than five
/// of n = 0..=1000 (the singular of most languages) may leave out trailing
/// arguments, as "one file" for "%d files". Verdicts are GNU msgfmt's.
#[test]
fn test_msgfmt_check_plural_forms_as_gnu() {
    const EN: &str = "nplurals=2; plural=(n != 1);";
    const CS: &str = "nplurals=3; plural=(n==1) ? 0 : (n>=2 && n<=4) ? 1 : 2;";
    const JA: &str = "nplurals=1; plural=0;";
    const FIVE: &str = "nplurals=3; plural=(n==1) ? 0 : (n>=2 && n<=6) ? 1 : 2;";
    const FOUR: &str = "nplurals=3; plural=(n==1) ? 0 : (n>=2 && n<=5) ? 1 : 2;";
    let count = |form: &str| {
        Some(format!(
            "number of format specifications in 'msgid_plural' and 'msgstr[{form}]'"
        ))
    };
    let argument_1 = |form: &str| {
        Some(format!(
            "in 'msgid_plural' and 'msgstr[{form}]' for argument 1 are not the same"
        ))
    };
    for (forms, msgid, plural, msgstr, refusal) in [
        (
            EN,
            "%d file",
            "%d files",
            &["%d soubor", "%d souborů"][..],
            None,
        ),
        (
            EN,
            "one file",
            "%d files",
            &["jeden soubor", "%d souborů"][..],
            None,
        ),
        (
            EN,
            "%d file",
            "%d files",
            &["jeden soubor", "%d souborů"][..],
            None,
        ),
        (
            EN,
            "%d file %s",
            "%d files %s",
            &["%d soubor", "%d souborů %s"][..],
            None,
        ),
        (
            EN,
            "%s: %d file",
            "%s: %d files",
            &["%s: jeden", "%s: %d souborů"][..],
            None,
        ),
        (EN, "file", "files %d", &["soubor", "%d souborů"][..], None),
        (EN, "%s file", "%d files", &["%d", "%d"][..], None),
        (
            CS,
            "%d file",
            "%d files",
            &["jeden", "%d soubory", "%d souborů"][..],
            None,
        ),
        (JA, "%d file", "%d files", &["%d"][..], None),
        (FOUR, "%d file", "%d files", &["%d", "x", "%d"][..], None),
        (
            EN,
            "%d file",
            "%d files",
            &["%d soubor", "souborů"][..],
            count("1"),
        ),
        (
            EN,
            "%d file",
            "%d files",
            &["%s soubor", "%d souborů"][..],
            argument_1("0"),
        ),
        (
            EN,
            "%d file %s",
            "%d files %s",
            &["soubor %s", "%d souborů %s"][..],
            argument_1("0"),
        ),
        (
            EN,
            "%d file",
            "%d files",
            &["%d soubor %d", "%d souborů"][..],
            count("0"),
        ),
        (JA, "%d file", "%d files", &["ファイル"][..], count("0")),
        (
            FIVE,
            "%d file",
            "%d files",
            &["%d", "x", "%d"][..],
            count("1"),
        ),
    ] {
        let mut po = format!(
            "msgid \"\"\nmsgstr \"\"\n\"Content-Type: text/plain; charset=UTF-8\\n\"\n\
             \"Plural-Forms: {forms}\\n\"\n\n\
             #, c-format\nmsgid \"{msgid}\"\nmsgid_plural \"{plural}\"\n"
        );
        for (i, s) in msgstr.iter().enumerate() {
            po.push_str(&format!("msgstr[{i}] \"{s}\"\n"));
        }
        let (dir, po_path) = create_temp_po_file(&po);
        let po = po_path.to_str().unwrap();
        let (err, code) = msgfmt_status(dir.path(), &["-c", "-v", "-o", "/dev/null", po]);
        match refusal {
            None => assert_eq!(code, 0, "{forms} {plural:?} / {msgstr:?}: {err}"),
            Some(reason) => {
                assert_eq!(code, 1, "{forms} {plural:?} / {msgstr:?}: {err}");
                assert!(err.contains(&format!("{po}:9: error: ")), "{err:?}");
                assert!(err.contains(&reason), "{plural:?} / {msgstr:?}: {err:?}");
            }
        }
    }
}
