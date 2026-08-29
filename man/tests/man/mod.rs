//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

#[cfg(test)]
mod tests {
    use std::process::Command;

    // -------------------------------------------------------------------------
    // -k / --apropos
    // -------------------------------------------------------------------------
    #[test]
    fn apropos_no_keywords() {
        let output = Command::new(env!("CARGO_BIN_EXE_man"))
            .args(["-k", "-C", "man.test.conf"])
            .output()
            .expect("Failed to run man -k");

        assert!(!output.status.success());
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            stderr.contains("no names specified"),
            "Expected 'no names specified' error, got:\n{stderr}"
        );
    }

    #[test]
    fn apropos_matches_names_and_descriptions() {
        // -k parsed every page as mdoc, so on a man(7) system nothing ever
        // matched; and .Nm/.Nd are children of the .Sh NAME block, which the
        // extractor never descended into, so mdoc pages produced nothing
        // either. POSIX: the result is equivalent to `grep -Ei` over a summary
        // database, so both names and descriptions are searched.
        let (code, out, _) = man(&["-M", "test_files", "-k", "expand", "-C", "man.test.conf"]);
        assert_eq!(code, Some(0), "stdout: {out}");
        assert!(
            out.contains("gzcat(1) - expand compressed files"),
            "a man(7) page must be searchable: {out}"
        );

        let (code, out, _) = man(&[
            "-M",
            "test_files",
            "-k",
            "concatenate",
            "-C",
            "man.test.conf",
        ]);
        assert_eq!(code, Some(0), "stdout: {out}");
        assert!(
            out.contains("cat(1) - concatenate and print files"),
            "an mdoc page must be searchable: {out}"
        );
    }

    #[test]
    fn apropos_reports_each_operand_that_matched_nothing() {
        // The status and the diagnostic were computed from the aggregate
        // result list, so an operand that matched nothing was silently dropped
        // whenever any other operand matched -- and the exit status was 0.
        for flag in ["-k", "-f"] {
            let (code, out, err) = man(&[
                "-M",
                "test_files",
                flag,
                "cat",
                "zzzznosuchpage",
                "-C",
                "man.test.conf",
            ]);
            assert!(out.contains("cat(1)"), "{flag}: stdout: {out}");
            assert!(
                err.contains("zzzznosuchpage: "),
                "{flag}: the unmatched operand must be named: {err}"
            );
            assert_eq!(code, Some(1), "{flag}: stdout: {out} stderr: {err}");
        }

        // All operands matching is still success.
        let (code, out, _) = man(&[
            "-M",
            "test_files",
            "-f",
            "cat",
            "gzcat",
            "-C",
            "man.test.conf",
        ]);
        assert_eq!(code, Some(0), "stdout: {out}");
    }

    #[test]
    fn apropos_reports_nothing_appropriate() {
        let (code, _, err) = man(&[
            "-M",
            "test_files",
            "-k",
            "zzzznotapageanywhere",
            "-C",
            "man.test.conf",
        ]);
        assert_eq!(code, Some(1));
        assert!(err.contains("nothing appropriate"), "stderr: {err}");
    }

    // -------------------------------------------------------------------------
    // -f / --whatis
    // -------------------------------------------------------------------------
    #[test]
    fn whatis_no_arguments() {
        let output = Command::new(env!("CARGO_BIN_EXE_man"))
            .args(["-f", "-C", "man.test.conf"])
            .output()
            .expect("Failed to run man -f");

        assert!(!output.status.success());
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            stderr.contains("no names specified"),
            "Expected 'no names specified' error, got:\n{stderr}"
        );
    }

    #[test]
    fn whatis_is_an_exact_name_lookup() {
        // -f shared the substring/ERE matcher with -k, so it listed every page
        // whose description merely mentioned the operand. POSIX omits -f
        // deliberately, so whatis(1)'s historical behaviour governs: the
        // operand must be a page name, whole.
        let (code, out, _) = man(&["-M", "test_files", "-f", "zcat", "-C", "man.test.conf"]);
        assert_eq!(code, Some(0), "stdout: {out}");
        assert_eq!(out.trim(), "zcat(1) - expand compressed files");

        // A word from the description is not a name.
        let (code, _, err) = man(&["-M", "test_files", "-f", "expand", "-C", "man.test.conf"]);
        assert_eq!(code, Some(1));
        assert!(err.contains("nothing appropriate"), "stderr: {err}");

        // Nor is a prefix of a name.
        let (code, _, _) = man(&["-M", "test_files", "-f", "zca", "-C", "man.test.conf"]);
        assert_eq!(code, Some(1));
    }

    // -------------------------------------------------------------------------
    // -a / --all
    // -------------------------------------------------------------------------
    #[test]
    fn all_flag_without_names() {
        let output = Command::new(env!("CARGO_BIN_EXE_man"))
            .args(["-a", "-C", "man.test.conf"])
            .output()
            .expect("Failed to run man -a");

        assert!(!output.status.success());
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            stderr.contains("no names specified"),
            "Expected 'no names specified' error, got:\n{stderr}"
        );
    }

    #[test]
    fn all_flag_with_names() {
        // -a lists every matching page rather than the first. The fixture tree
        // has cat(1) in both the generic and the amd64 directory.
        let (code, out, _) = man(&["-M", "test_files", "-a", "-w", "cat", "-C", "man.test.conf"]);
        assert_eq!(code, Some(0), "stdout: {out}");
        assert!(out.contains("test_files/man1/cat.1"), "{out}");
    }

    // -------------------------------------------------------------------------
    // -C / --config-file
    // -------------------------------------------------------------------------
    #[test]
    fn config_file_invalid() {
        let output = Command::new(env!("CARGO_BIN_EXE_man"))
            .args(["-C", "non_existent.conf", "ls"])
            .output()
            .expect("Failed to run man -C non_existent.conf ls");

        assert!(!output.status.success());
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            stderr.contains("configuration file was not found"),
            "Expected 'configuration file was not found' error, got:\n{stderr}"
        );
    }

    #[test]
    fn config_file_without_names() {
        let output = Command::new(env!("CARGO_BIN_EXE_man"))
            .args(["-C", "man.test.conf"])
            .output()
            .expect("Failed to run man -C /etc/man.conf");

        assert!(!output.status.success());
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            stderr.contains("no names specified"),
            "Expected 'no names specified' error, got:\n{stderr}"
        );
    }

    // -------------------------------------------------------------------------
    // -c / --copy
    // -------------------------------------------------------------------------
    #[test]
    fn copy_flag_without_name() {
        let output = Command::new(env!("CARGO_BIN_EXE_man"))
            .args(["-c", "-C", "man.test.conf"])
            .output()
            .expect("Failed to run man -c");

        assert!(!output.status.success());
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            stderr.contains("no names specified"),
            "Expected 'no names specified' error, got:\n{stderr}"
        );
    }

    #[test]
    fn copy_flag_with_name() {
        // -c writes the rendered page to stdout instead of paging it.
        let (code, out, _) = man(&["-M", "test_files", "-c", "cat", "-C", "man.test.conf"]);
        assert_eq!(code, Some(0), "stdout: {out}");
        assert!(out.contains("concatenate and print files"), "{out}");
        assert!(
            out.ends_with('\n'),
            "a rendered page ends with a newline: {out:?}"
        );
    }

    // -------------------------------------------------------------------------
    // -h / --synopsis
    // -------------------------------------------------------------------------
    #[test]
    fn synopsis_without_name() {
        let output = Command::new(env!("CARGO_BIN_EXE_man"))
            .args(["-h", "-C", "man.test.conf"])
            .output()
            .expect("Failed to run man -h");

        assert!(!output.status.success());
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            stderr.contains("no names specified"),
            "Expected 'no names specified' error, got:\n{stderr}"
        );
    }

    #[test]
    fn synopsis_renders_for_both_page_formats() {
        // -h routed man(7) pages into the mdoc engine, so it produced zero
        // bytes and exit 0 for every one of them.
        let (code, out, _) = man(&[
            "-M",
            "test_files",
            "-c",
            "-h",
            "gzcat",
            "-C",
            "man.test.conf",
        ]);
        assert_eq!(code, Some(0), "stdout: {out}");
        assert!(out.contains("SYNOPSIS"), "man(7) synopsis: {out}");
        assert!(out.contains("gzcat"), "man(7) synopsis: {out}");
        assert!(
            !out.contains("Expand files to standard output"),
            "the body must not be included: {out}"
        );

        let (code, out, _) = man(&["-M", "test_files", "-c", "-h", "cat", "-C", "man.test.conf"]);
        assert_eq!(code, Some(0), "stdout: {out}");
        assert!(out.contains("SYNOPSIS"), "mdoc synopsis: {out}");
    }

    // -------------------------------------------------------------------------
    // -l / --local-file
    // -------------------------------------------------------------------------
    #[test]
    fn local_file_not_found() {
        let output = Command::new(env!("CARGO_BIN_EXE_man"))
            .args(["-l", "fake/path.1", "-C", "man.test.conf"])
            .output()
            .expect("Failed to run man -l fake/path.1");

        assert!(!output.status.success());
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            stderr.contains("was not found"),
            "Expected 'file: fake/path.1 was not found' error, got:\n{stderr}"
        );
    }

    #[test]
    fn local_file_without_other_args() {
        // This named test.mdoc, which does not exist in the crate, so it only
        // ever exercised the not-found path -- and passed because exit 1 was
        // accepted. Render a file that is actually there.
        let (code, out, _) = man(&["-c", "-l", "test_files/man1/cat.1", "-C", "man.test.conf"]);
        assert_eq!(code, Some(0), "stdout: {out}");
        assert!(out.contains("concatenate and print files"), "{out}");
    }

    // -------------------------------------------------------------------------
    // -M / --override_paths
    // -------------------------------------------------------------------------
    /// Run man with the given arguments, returning (exit code, stdout, stderr).
    /// Tests that pass `-M test_files` never reach the host's manual tree, so
    /// their result does not depend on what is installed.
    fn man(args: &[&str]) -> (Option<i32>, String, String) {
        let output = Command::new(env!("CARGO_BIN_EXE_man"))
            .args(args)
            .output()
            .expect("Failed to run man");
        (
            output.status.code(),
            String::from_utf8_lossy(&output.stdout).into_owned(),
            String::from_utf8_lossy(&output.stderr).into_owned(),
        )
    }

    #[test]
    fn so_alias_resolves_in_a_custom_root() {
        // `.so` was resolved against the hard-coded system roots only, so an
        // alias page under -M could not find its target.
        let (code, out, _) = man(&["-M", "test_files", "-c", "catalias", "-C", "man.test.conf"]);
        assert_eq!(code, Some(0), "stdout: {out}");
        assert!(out.contains("concatenate and print files"), "{out}");
    }

    #[test]
    fn override_paths_replaces_the_search_list() {
        // -M wrote its value into the MANPATH environment variable and then
        // every source was concatenated anyway, so it augmented the list
        // instead of replacing it and the built-in roots were always searched:
        // `man -M /empty -w ls` still found /usr/share/man/man1/ls.1.gz.
        let (code, out, _) = man(&["-M", "test_files", "-w", "cat", "-C", "man.test.conf"]);
        assert_eq!(code, Some(0), "stdout: {out}");
        assert_eq!(out.trim(), "test_files/man1/cat.1");

        let (code, out, err) = man(&["-M", "/nonexistent", "-w", "cat", "-C", "man.test.conf"]);
        assert_eq!(code, Some(1), "stdout: {out}");
        assert!(err.contains("not found"), "stderr: {err}");
    }

    #[test]
    fn augment_paths_still_adds_to_the_search_list() {
        let (code, out, _) = man(&[
            "-M",
            "/nonexistent",
            "-m",
            "test_files",
            "-w",
            "cat",
            "-C",
            "man.test.conf",
        ]);
        assert_eq!(code, Some(0), "stdout: {out}");
        assert_eq!(out.trim(), "test_files/man1/cat.1");
    }

    #[test]
    fn override_paths_multiple() {
        // A ':'-separated value is split into several roots, and only those.
        let (code, out, _) = man(&[
            "-M",
            "/nonexistent:test_files",
            "-w",
            "cat",
            "-C",
            "man.test.conf",
        ]);
        assert_eq!(code, Some(0), "stdout: {out}");
        assert_eq!(out.trim(), "test_files/man1/cat.1");
    }

    #[test]
    fn search_paths_are_not_reported_twice() {
        // A root named by both the config and the built-in list produced one
        // -w line per source.
        let (_, out, _) = man(&["-w", "cat", "-C", "man.test.conf", "-M", "test_files"]);
        assert_eq!(out.lines().count(), 1, "stdout: {out}");
    }

    // -------------------------------------------------------------------------
    // -m / --augment_paths
    // -------------------------------------------------------------------------
    #[test]
    fn augment_paths_single() {
        // -m adds a root in front of the existing list.
        let (code, out, _) = man(&[
            "-M",
            "/nonexistent",
            "-m",
            "test_files",
            "-w",
            "cat",
            "-C",
            "man.test.conf",
        ]);
        assert_eq!(code, Some(0), "stdout: {out}");
        assert_eq!(out.trim(), "test_files/man1/cat.1");
    }

    #[test]
    fn augment_paths_multiple() {
        let output = Command::new(env!("CARGO_BIN_EXE_man"))
            .args([
                "-m",
                "/first/path:/second/path",
                "ls",
                "-C",
                "man.test.conf",
            ])
            .output()
            .expect("Failed to run man -m /first/path:/second/path ls");

        // A ':'-separated -m value contributes several roots.
        let (code, out, _) = man(&[
            "-M",
            "/nonexistent",
            "-m",
            "/first/path:test_files",
            "-w",
            "cat",
            "-C",
            "man.test.conf",
        ]);
        assert_eq!(code, Some(0), "stdout: {out}");
        assert_eq!(out.trim(), "test_files/man1/cat.1");
        let _ = output;
    }

    // -------------------------------------------------------------------------
    // -S / --subsection
    // -------------------------------------------------------------------------
    #[test]
    fn subsection_flag_no_name() {
        let output = Command::new(env!("CARGO_BIN_EXE_man"))
            .args(["-S", "amd64", "-C", "man.test.conf"])
            .output()
            .expect("Failed to run man -S amd64");

        assert!(!output.status.success());
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            stderr.contains("no names specified"),
            "Expected 'no names specified' error, got:\n{stderr}"
        );
    }

    #[test]
    fn subsection_selects_the_architecture_page() {
        // -S wrote a MACHINE environment variable that nothing in the process
        // read, so the option was accepted and did nothing. It now names an
        // architecture subdirectory, as in mandoc and the BSDs, searched ahead
        // of the section directory.
        let (code, out, _) = man(&[
            "-M",
            "test_files",
            "-S",
            "amd64",
            "-c",
            "cat",
            "-C",
            "man.test.conf",
        ]);
        assert_eq!(code, Some(0), "stdout: {out}");
        assert!(out.contains("Architecture-specific page."), "{out}");

        // Without -S, the generic page.
        let (_, out, _) = man(&["-M", "test_files", "-c", "cat", "-C", "man.test.conf"]);
        assert!(!out.contains("Architecture-specific page."), "{out}");

        // -S names a preference, not a filter: an architecture with no page of
        // its own still finds the generic one.
        let (code, out, _) = man(&[
            "-M",
            "test_files",
            "-S",
            "sparc64",
            "-c",
            "cat",
            "-C",
            "man.test.conf",
        ]);
        assert_eq!(code, Some(0), "stdout: {out}");
        assert!(out.contains("concatenate"), "{out}");
    }

    #[test]
    fn subsection_flag_with_name() {
        // An architecture with no page of its own still resolves the generic
        // one; the selecting case is covered by
        // subsection_selects_the_architecture_page.
        let (code, out, _) = man(&[
            "-M",
            "test_files",
            "-S",
            "sparc64",
            "-w",
            "cat",
            "-C",
            "man.test.conf",
        ]);
        assert_eq!(code, Some(0), "stdout: {out}");
        assert_eq!(out.trim(), "test_files/man1/cat.1");
    }

    // -------------------------------------------------------------------------
    // -s / --section
    // -------------------------------------------------------------------------
    #[test]
    fn section_invalid() {
        let output = Command::new(env!("CARGO_BIN_EXE_man"))
            .args(["-s", "99", "ls", "-C", "man.test.conf"])
            .output()
            .expect("Failed to run man -s 99 ls");

        assert!(!output.status.success());
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            stderr.contains("invalid value '99' for '-s <SECTION>'")
                && stderr.contains("invalid section: 99"),
            "Expected 'Invalid section: 99', got:\n{stderr}"
        );
    }

    #[test]
    fn section_accepts_posix_numbers() {
        // Section was a clap ValueEnum, whose derived value names are the
        // variant names, so the accepted spellings were `s1`..`s9` and
        // `man -s 1 ls` was rejected outright. This test used to pass "s1"
        // under an .expect string claiming it ran `man -s 1 ls`.
        let (code, out, _) = man(&[
            "-M",
            "test_files",
            "-s",
            "1",
            "-w",
            "cat",
            "-C",
            "man.test.conf",
        ]);
        assert_eq!(code, Some(0), "stdout: {out}");
        assert_eq!(out.trim(), "test_files/man1/cat.1");

        // And it really filters: cat is only in section 1.
        let (code, _, _) = man(&[
            "-M",
            "test_files",
            "-s",
            "2",
            "-w",
            "cat",
            "-C",
            "man.test.conf",
        ]);
        assert_eq!(code, Some(1));
    }

    #[test]
    fn section_rejects_the_value_enum_spelling() {
        // `s1` was never a section anywhere; it was an artifact of the derive.
        let (code, _, err) = man(&["-s", "s1", "ls", "-C", "man.test.conf"]);
        assert_eq!(code, Some(2));
        assert!(err.contains("invalid section: s1"), "stderr: {err}");
    }

    // -------------------------------------------------------------------------
    // -w / --list_pathnames
    // -------------------------------------------------------------------------
    #[test]
    fn list_pathnames_flag_no_name() {
        // -w with no operand is an error, like every other mode.
        let (code, _, err) = man(&["-w", "-C", "man.test.conf"]);
        assert_eq!(code, Some(1));
        assert!(err.contains("no names specified"), "stderr: {err}");
    }

    #[test]
    fn list_pathnames_flag_with_name() {
        let output = Command::new(env!("CARGO_BIN_EXE_man"))
            .args(["-w", "nonexistent_cmd", "-C", "man.test.conf"])
            .output()
            .expect("Failed to run man -w nonexistent_cmd");

        assert!(!output.status.success());
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            stderr.contains("system documentation for \"nonexistent_cmd\" not found"),
            "Expected 'system documentation for \"nonexistent_cmd\" not found', got:\n{stderr}"
        );
    }

    // -------------------------------------------------------------------------
    // --help
    // -------------------------------------------------------------------------
    #[test]
    fn help_flag() {
        let output = Command::new(env!("CARGO_BIN_EXE_man"))
            .args(["--help", "-C", "man.test.conf"])
            .output()
            .expect("Failed to run man --help");

        assert!(output.status.success());
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(
            stdout.contains("Usage:"),
            "Expected help text containing 'Usage:', got:\n{stdout}"
        );
        assert!(
            stdout.contains("-k, --apropos"),
            "Expected help text mentioning '-k, --apropos', got:\n{stdout}"
        );
    }

    // -------------------------------------------------------------------------
    // Basic check for "names"
    // -------------------------------------------------------------------------
    #[test]
    fn single_name_argument() {
        let (code, out, _) = man(&["-M", "test_files", "-c", "cat", "-C", "man.test.conf"]);
        assert_eq!(code, Some(0), "stdout: {out}");
        assert!(out.contains("concatenate and print files"), "{out}");
    }

    #[test]
    fn multiple_name_arguments() {
        // Every operand is attempted; one failure is reported but does not
        // abort the rest, and the overall status is still non-zero.
        let (code, out, err) = man(&[
            "-M",
            "test_files",
            "-c",
            "cat",
            "gzcat",
            "nonexistent",
            "-C",
            "man.test.conf",
        ]);
        assert_eq!(code, Some(1), "stdout: {out}");
        assert!(out.contains("concatenate and print files"), "{out}");
        assert!(out.contains("expand compressed files"), "{out}");
        assert!(err.contains("nonexistent"), "stderr: {err}");
    }

    // -------------------------------------------------------------------------
    // Robustness — malformed pages must not crash (audit Phase 1)
    // -------------------------------------------------------------------------

    /// Write `content` to a uniquely named temp file and return its path.
    fn write_temp_page(tag: &str, content: &str) -> std::path::PathBuf {
        let path = std::env::temp_dir().join(format!("man_audit_{}_{}.1", tag, std::process::id()));
        std::fs::write(&path, content).expect("write temp page");
        path
    }

    // Audit #1: `.Xr name` with a missing section number must not panic; it
    // renders as the bare name.
    #[test]
    fn xr_missing_section_does_not_crash() {
        let page = write_temp_page("xr", ".Dd x\n.Dt T 1\n.Os\n.Sh DESCRIPTION\n.Xr grep\n");
        let output = Command::new(env!("CARGO_BIN_EXE_man"))
            .args(["-c", "-l"])
            .arg(&page)
            .args(["-C", "man.test.conf"])
            .output()
            .expect("Failed to run man -c -l");
        let _ = std::fs::remove_file(&page);

        assert_eq!(
            output.status.code(),
            Some(0),
            "expected clean exit, got {:?}; stderr: {}",
            output.status.code(),
            String::from_utf8_lossy(&output.stderr)
        );
        assert!(
            String::from_utf8_lossy(&output.stdout).contains("grep"),
            "expected the bare name to render"
        );
    }

    /// Run `man -c -l` on a generated page and return (exit code, stderr).
    fn run_on_page(tag: &str, body: &str) -> (Option<i32>, String, std::time::Duration) {
        let page = write_temp_page(tag, body);
        let start = std::time::Instant::now();
        let output = Command::new(env!("CARGO_BIN_EXE_man"))
            .args(["-c", "-l"])
            .arg(&page)
            .args(["-C", "man.test.conf"])
            .output()
            .expect("Failed to run man -c -l");
        let elapsed = start.elapsed();
        let _ = std::fs::remove_file(&page);
        (
            output.status.code(),
            String::from_utf8_lossy(&output.stderr).into_owned(),
            elapsed,
        )
    }

    // A single line nesting partial-implicit macros past the parser's cap must
    // be reported, not aborted. The AST is walked recursively when it is cloned,
    // formatted and dropped, so an uncapped page overflowed the stack — which
    // raises SIGABRT and cannot be caught.
    //
    // The repeated token must NOT carry a leading dot: `.Aq .Aq .Aq` tokenizes
    // to `.Aq` tokens, which are not recognised as container macros and become
    // plain text two levels deep. That is why the previous version of this test
    // passed while a 10,000-deep page aborted the process.
    #[test]
    fn deeply_nested_inline_macros_rejected() {
        let mut body = String::from(".Dd x\n.Dt T 1\n.Os\n.Sh D\n.");
        body.push_str(&"Op ".repeat(10_000));
        body.push_str("x\n");
        let (code, stderr, elapsed) = run_on_page("deepinline", &body);

        assert_eq!(
            code,
            Some(1),
            "expected a diagnostic, got {code:?}: {stderr}"
        );
        assert!(
            stderr.contains("nesting"),
            "stderr must name the cause, got: {stderr}"
        );
        assert!(
            elapsed < std::time::Duration::from_secs(5),
            "must reject deep nesting fast, took {elapsed:?}"
        );
    }

    // The other depth source: one open block per line. This nests through the
    // frame stack with no inline recursion at all, so a cap on the inline parser
    // alone would still overflow here.
    #[test]
    fn deeply_nested_blocks_rejected() {
        let mut body = String::from(".Dd x\n.Dt T 1\n.Os\n.Sh D\n");
        body.push_str(&".Ao\n".repeat(20_000));
        let (code, stderr, elapsed) = run_on_page("deepblock", &body);

        assert_eq!(
            code,
            Some(1),
            "expected a diagnostic, got {code:?}: {stderr}"
        );
        assert!(
            stderr.contains("nesting"),
            "stderr must name the cause, got: {stderr}"
        );
        assert!(
            elapsed < std::time::Duration::from_secs(5),
            "must reject deep nesting fast, took {elapsed:?}"
        );
    }

    // Ordinary nesting must keep working: the cap is far above what real pages
    // use, so a handful of levels renders normally.
    #[test]
    fn ordinary_nesting_still_renders() {
        let body = String::from(
            ".Dd x\n.Dt T 1\n.Os\n.Sh D\n.Op Op Op Op Op deep\n.Ao\n.Bo\n.Po\ntext\n.Pc\n.Bc\n.Ac\n",
        );
        let (code, stderr, _) = run_on_page("shallow", &body);
        assert_eq!(code, Some(0), "expected success, got {code:?}: {stderr}");
    }

    // -------------------------------------------------------------------------
    // Pager & width fidelity (audit Phase 2)
    // -------------------------------------------------------------------------

    fn longest_line(stdout: &[u8]) -> usize {
        String::from_utf8_lossy(stdout)
            .lines()
            .map(|l| l.chars().count())
            .max()
            .unwrap_or(0)
    }

    fn format_local(env: &[(&str, &str)], page: &std::path::Path) -> std::process::Output {
        let mut cmd = Command::new(env!("CARGO_BIN_EXE_man"));
        cmd.args(["-c", "-l"])
            .arg(page)
            .args(["-C", "man.test.conf"]);
        for (k, v) in env {
            cmd.env(k, v);
        }
        cmd.output().expect("Failed to run man -c -l")
    }

    // Audit #7: COLUMNS sets the rendering width (wider and narrower than the
    // 78-column default), instead of being ignored.
    #[test]
    fn columns_env_sets_width() {
        let mut body = String::from(".Dd x\n.Dt T 1\n.Os\n.Sh DESCRIPTION\n");
        for i in 0..40 {
            body.push_str(&format!("word{i:02} "));
        }
        body.push('\n');
        let page = write_temp_page("cols", &body);

        let wide = longest_line(&format_local(&[("COLUMNS", "120")], &page).stdout);
        let narrow = longest_line(&format_local(&[("COLUMNS", "40")], &page).stdout);
        let _ = std::fs::remove_file(&page);

        assert!(wide > 90, "COLUMNS=120 should widen lines, got {wide}");
        assert!(narrow <= 39, "COLUMNS=40 should narrow lines, got {narrow}");
    }

    // Audit #12: a COLUMNS value of 0 must not underflow the width computation.
    #[test]
    fn columns_zero_does_not_underflow() {
        let page = write_temp_page("colz", ".Dd x\n.Dt T 1\n.Os\n.Sh D\nhello\n");
        let output = format_local(&[("COLUMNS", "0")], &page);
        let _ = std::fs::remove_file(&page);
        assert_eq!(output.status.code(), Some(0));
        assert!(longest_line(&output.stdout) <= 200, "width must stay sane");
    }

    // Audit #13: a `.Bl -width` larger than 20 is honored, not silently clamped.
    #[test]
    fn bl_width_above_20_is_honored() {
        let page = write_temp_page(
            "blw",
            ".Dd x\n.Dt T 1\n.Os\n.Sh D\n.Bl -tag -width 30\n.It tag\nbody\n.El\n",
        );
        // Wide page so 30 fits.
        let output = format_local(&[("COLUMNS", "100")], &page);
        let _ = std::fs::remove_file(&page);
        let stdout = String::from_utf8_lossy(&output.stdout);
        // The tag and body share a line: `<indent>tag<pad>body`. The column
        // where `body` begins reflects the tag-column width; with -width 30 it
        // must sit past the old 20-clamp (base indent + 20 = 26).
        let body_line = stdout
            .lines()
            .find(|l| l.contains("tag") && l.contains("body"))
            .expect("tag+body line present");
        let body_col = body_line.find("body").unwrap();
        assert!(
            body_col > 26,
            "body should start past the old 20-clamp, got column {body_col}"
        );
    }

    // Audit #6: with no `-c` and stdout not a terminal (piped), output is written
    // directly, NOT through PAGER.
    #[test]
    fn pager_not_invoked_when_piped() {
        // A PAGER marker script; if invoked it prepends a sentinel line.
        let pager = std::env::temp_dir().join(format!("man_audit_pager_{}.sh", std::process::id()));
        std::fs::write(&pager, "#!/bin/sh\necho __PAGER_RAN__\ncat\n").unwrap();
        let mut perms = std::fs::metadata(&pager).unwrap().permissions();
        use std::os::unix::fs::PermissionsExt;
        perms.set_mode(0o755);
        std::fs::set_permissions(&pager, perms).unwrap();

        let page = write_temp_page("pgr", ".Dd x\n.Dt T 1\n.Os\n.Sh NAME\n.Nm t\n");
        let output = Command::new(env!("CARGO_BIN_EXE_man"))
            .args(["-l"])
            .arg(&page)
            .args(["-C", "man.test.conf"])
            .env("PAGER", &pager)
            .output()
            .expect("Failed to run man -l");
        let _ = std::fs::remove_file(&page);
        let _ = std::fs::remove_file(&pager);

        assert!(
            !String::from_utf8_lossy(&output.stdout).contains("__PAGER_RAN__"),
            "PAGER must not be invoked when stdout is not a terminal"
        );
    }

    // -------------------------------------------------------------------------
    // Search, encoding & misc (audit Phase 4)
    // -------------------------------------------------------------------------

    // Audit #10: a non-UTF-8 (Latin-1) page renders instead of erroring out.
    #[test]
    fn non_utf8_page_renders() {
        let mut bytes = b".Dd x\n.Dt T 1\n.Os\n.Sh D\n".to_vec();
        bytes.extend_from_slice(b"caf\xe9\n"); // Latin-1 'é'
        let path = std::env::temp_dir().join(format!("man_audit_latin1_{}.1", std::process::id()));
        std::fs::write(&path, &bytes).unwrap();

        let output = format_local(&[], &path);
        let _ = std::fs::remove_file(&path);
        assert_eq!(output.status.code(), Some(0), "should not error on Latin-1");
        assert!(String::from_utf8_lossy(&output.stdout).contains("caf"));
    }

    // Audit #11: a `.It` outside any `.Bl` renders its text rather than vanishing.
    #[test]
    fn stray_it_renders() {
        let page = write_temp_page("it", ".Dd x\n.Dt T 1\n.Os\n.Sh D\n.It orphanitem\n");
        let output = format_local(&[], &page);
        let _ = std::fs::remove_file(&page);
        assert!(
            String::from_utf8_lossy(&output.stdout).contains("orphanitem"),
            "stray .It text should render"
        );
    }

    // Audit #15: a `.Tg` line no longer breaks parsing or leaks; it renders
    // nothing while surrounding text is preserved.
    #[test]
    fn tg_line_is_harmless() {
        let page = write_temp_page("tg", ".Dd x\n.Dt T 1\n.Os\n.Sh D\n.Tg sometag\nbody text\n");
        let output = format_local(&[], &page);
        let _ = std::fs::remove_file(&page);
        assert_eq!(output.status.code(), Some(0));
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(stdout.contains("body text") && !stdout.contains("sometag"));
    }

    // Audit #16: a missing name does not abort the batch; later operands are
    // still processed and the overall status is non-zero.
    #[test]
    fn missing_name_does_not_abort_batch() {
        let output = Command::new(env!("CARGO_BIN_EXE_man"))
            .args([
                "man_audit_absent_one",
                "man_audit_absent_two",
                "-C",
                "man.test.conf",
            ])
            .output()
            .expect("Failed to run man");
        assert_eq!(output.status.code(), Some(1));
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            stderr.contains("man_audit_absent_one") && stderr.contains("man_audit_absent_two"),
            "both missing names should be reported, got: {stderr}"
        );
    }

    // Audit #3: a legacy man(7)/roff page (.TH/.SH/.B/.TP) renders its content
    // (it previously produced an empty page).
    #[test]
    fn man7_page_renders() {
        let page = write_temp_page(
            "man7",
            ".TH TEST 1 2024 \"util 1.0\"\n\
             .SH NAME\n\
             test \\- a test program\n\
             .SH DESCRIPTION\n\
             This is the description.\n\
             .SH EXIT STATUS\n\
             .TP\n\
             .B 0\n\
             Success.\n",
        );
        let output = format_local(&[], &page);
        let _ = std::fs::remove_file(&page);
        assert_eq!(output.status.code(), Some(0), "man(7) page should render");
        let stdout = String::from_utf8_lossy(&output.stdout);
        for needle in [
            "TEST(1)",
            "NAME",
            "test - a test program",
            "DESCRIPTION",
            "This is the description.",
            "EXIT STATUS",
        ] {
            assert!(stdout.contains(needle), "missing {needle:?} in:\n{stdout}");
        }
    }

    // Audit #3 (exit-code half): a man(7)-detected page that produces no body is
    // reported as an error (non-zero), not a silent empty success.
    #[test]
    fn man7_empty_page_errors() {
        let page = write_temp_page("man7e", ".TH TEST 1\n");
        let output = format_local(&[], &page);
        let _ = std::fs::remove_file(&page);
        assert_eq!(
            output.status.code(),
            Some(1),
            "empty page should be an error"
        );
    }

    // Audit #8: `-k` with a regex metacharacter keyword must not crash (the
    // native search compiles keywords as case-insensitive EREs, with a literal
    // fallback for invalid syntax).
    #[test]
    fn apropos_regex_keyword_does_not_crash() {
        for kw in ["pri.tf", "^l", "(unclosed"] {
            let output = Command::new(env!("CARGO_BIN_EXE_man"))
                .args(["-k", kw, "-C", "man.test.conf"])
                .output()
                .expect("Failed to run man -k");
            assert!(
                matches!(output.status.code(), Some(0) | Some(1)),
                "keyword {kw:?} should exit 0/1, got {:?}",
                output.status.code()
            );
        }
    }
}

#[cfg(test)]
mod malformed {
    use std::path::PathBuf;
    use std::process::Command;

    /// Render every page in `test_files/malformed/` and require that `man`
    /// *terminates*, at any exit status, without crashing.
    ///
    /// The existing 104-page corpus is entirely well-formed, which is why a
    /// stack overflow on a two-line page, five reachable `unreachable!()` arms
    /// and two arithmetic underflows all survived. Each file here is a shape
    /// that previously aborted, panicked or ran unbounded; several are taken
    /// from pages shipped on a stock system, not invented.
    ///
    /// A renderer must be total: garbage in, diagnostic out — never a crash.
    #[test]
    fn malformed_pages_do_not_crash() {
        let dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("test_files/malformed");
        let mut checked = 0;

        for entry in std::fs::read_dir(&dir).expect("malformed corpus present") {
            let path = entry.expect("readable entry").path();
            if !path.is_file() {
                continue;
            }
            let output = Command::new(env!("CARGO_BIN_EXE_man"))
                .args([
                    "-C".as_ref(),
                    "man.test.conf".as_ref(),
                    "-l".as_ref(),
                    path.as_os_str(),
                ])
                .env("COLUMNS", "40")
                .output()
                .unwrap_or_else(|e| panic!("failed to run man on {}: {e}", path.display()));

            // 0 (rendered) and 1 (rejected with a diagnostic) are both fine.
            // Anything else is a panic (101) or a signal (`code()` is None).
            let code = output.status.code();
            assert!(
                matches!(code, Some(0) | Some(1)),
                "man crashed on {}: status {:?}\nstderr:\n{}",
                path.display(),
                output.status,
                String::from_utf8_lossy(&output.stderr)
            );
            assert!(
                output.stdout.len() < 16 << 20,
                "man produced {} bytes for {}",
                output.stdout.len(),
                path.display()
            );
            checked += 1;
        }

        assert!(checked >= 23, "corpus shrank: only {checked} files");
    }
}
