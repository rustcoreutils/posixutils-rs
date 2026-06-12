//
// Copyright (c) 2024 Hemi Labs, Inc.
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
    fn apropos_with_keywords() {
        let output = Command::new(env!("CARGO_BIN_EXE_man"))
            .args(["-k", "printf", "-C", "man.test.conf"])
            .output()
            .expect("Failed to run man -k printf");

        assert!(
            output.status.success() || output.status.code() == Some(1),
            "Expected exit code 0 or 1, got: {:?}",
            output.status.code()
        );
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
    fn whatis_one_argument() {
        let output = Command::new(env!("CARGO_BIN_EXE_man"))
            .args(["-f", "ls", "-C", "man.test.conf"])
            .output()
            .expect("Failed to run man -f ls");

        assert!(
            output.status.success() || output.status.code() == Some(1),
            "Expected exit code 0 or 1, got: {:?}",
            output.status.code()
        );
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
        let output = Command::new(env!("CARGO_BIN_EXE_man"))
            .args(["-a", "ls", "-C", "man.test.conf"])
            .output()
            .expect("Failed to run man -a ls");

        assert!(
            output.status.success() || output.status.code() == Some(1),
            "Expected exit code 0 or 1, got: {:?}",
            output.status.code()
        );
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
        let output = Command::new(env!("CARGO_BIN_EXE_man"))
            .args(["-c", "ls", "-C", "man.test.conf"])
            .output()
            .expect("Failed to run man -c ls");

        assert!(
            output.status.success() || output.status.code() == Some(1),
            "Expected exit code 0 or 1, got: {:?}",
            output.status.code()
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
    fn synopsis_with_name() {
        let output = Command::new(env!("CARGO_BIN_EXE_man"))
            .args(["-h", "printf", "-C", "man.test.conf"])
            .output()
            .expect("Failed to run man -h printf");

        println!("Output: \"{}\"", String::from_utf8(output.stdout).unwrap());
        println!("Error: \"{}\"", String::from_utf8(output.stderr).unwrap());

        assert!(
            output.status.success() || output.status.code() == Some(1),
            "Expected exit code 0 or 1, got: {:?}",
            output.status.code()
        );
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
        let output = Command::new(env!("CARGO_BIN_EXE_man"))
            .args(["-l", "test.mdoc", "-C", "man.test.conf"])
            .output()
            .expect("Failed to run man -l tests/test_data.1");

        assert!(
            output.status.success() || output.status.code() == Some(1),
            "Expected exit code 0 or 1, got: {:?}",
            output.status.code()
        );
    }

    // -------------------------------------------------------------------------
    // -M / --override_paths
    // -------------------------------------------------------------------------
    #[test]
    fn override_paths_single() {
        let output = Command::new(env!("CARGO_BIN_EXE_man"))
            .args(["-M", "/tmp", "ls", "-C", "man.test.conf"])
            .output()
            .expect("Failed to run man -M /tmp ls");

        assert!(
            output.status.success() || output.status.code() == Some(1),
            "Expected exit code 0 or 1, got: {:?}",
            output.status.code()
        );
    }

    #[test]
    fn override_paths_multiple() {
        let output = Command::new(env!("CARGO_BIN_EXE_man"))
            .args([
                "-M",
                "/tmp:/nonexistent:/usr/local/man",
                "ls",
                "-C",
                "man.test.conf",
            ])
            .output()
            .expect("Failed to run man -M with multiple paths ls");

        assert!(
            output.status.success() || output.status.code() == Some(1),
            "Expected exit code 0 or 1, got: {:?}",
            output.status.code()
        );
    }

    // -------------------------------------------------------------------------
    // -m / --augment_paths
    // -------------------------------------------------------------------------
    #[test]
    fn augment_paths_single() {
        let output = Command::new(env!("CARGO_BIN_EXE_man"))
            .args(["-m", "/opt/mylocalman", "ls", "-C", "man.test.conf"])
            .output()
            .expect("Failed to run man -m /opt/mylocalman ls");

        assert!(
            output.status.success() || output.status.code() == Some(1),
            "Expected exit code 0 or 1, got: {:?}",
            output.status.code()
        );
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

        assert!(
            output.status.success() || output.status.code() == Some(1),
            "Expected exit code 0 or 1, got: {:?}",
            output.status.code()
        );
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
    fn subsection_flag_with_name() {
        let output = Command::new(env!("CARGO_BIN_EXE_man"))
            .args(["-S", "amd64", "ls", "-C", "man.test.conf"])
            .output()
            .expect("Failed to run man -S amd64 ls");

        assert!(
            output.status.success() || output.status.code() == Some(1),
            "Expected exit code 0 or 1, got: {:?}",
            output.status.code()
        );
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
            stderr.contains("invalid value '99' for '-s <SECTION>'"),
            "Expected 'Invalid section: 99', got:\n{stderr}"
        );
    }

    #[test]
    fn section_valid() {
        let output = Command::new(env!("CARGO_BIN_EXE_man"))
            .args(["-s", "s1", "ls", "-C", "man.test.conf"])
            .output()
            .expect("Failed to run man -s 1 ls");

        assert!(
            output.status.success() || output.status.code() == Some(1),
            "Expected exit code 0 or 1, got: {:?}",
            output.status.code()
        );
    }

    // -------------------------------------------------------------------------
    // -w / --list_pathnames
    // -------------------------------------------------------------------------
    #[test]
    fn list_pathnames_flag_no_name() {
        let output = Command::new(env!("CARGO_BIN_EXE_man"))
            .args(["-w", "-C", "man.test.conf"])
            .output()
            .expect("Failed to run man -w");

        assert!(
            output.status.success() || output.status.code() == Some(1),
            "Expected exit code 0 or 1, got: {:?}",
            output.status.code()
        );
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
        let output = Command::new(env!("CARGO_BIN_EXE_man"))
            .args(["ls", "-C", "man.test.conf"])
            .output()
            .expect("Failed to run man ls");

        assert!(
            output.status.success() || output.status.code() == Some(1),
            "Expected exit code 0 or 1, got: {:?}",
            output.status.code()
        );
    }

    #[test]
    fn multiple_name_arguments() {
        let output = Command::new(env!("CARGO_BIN_EXE_man"))
            .args(["ls", "cat", "nonexistent", "-C", "man.test.conf"])
            .output()
            .expect("Failed to run man ls cat nonexistent");

        assert!(
            output.status.success() || output.status.code() == Some(1),
            "Expected exit code 0 or 1, got: {:?}",
            output.status.code()
        );
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

    // Audit #2: a single line with an absurd number of nested partial macros is
    // rejected up front (the PEG grammar otherwise backtracks exponentially and
    // overflows the stack). Must fail fast with a parse error, not hang/crash.
    #[test]
    fn deeply_nested_macros_rejected() {
        let mut body = String::from(".Dd x\n.Dt T 1\n.Os\n.Sh D\n");
        body.push_str(&".Aq ".repeat(20_000));
        body.push_str("x\n");
        let page = write_temp_page("deep", &body);

        let start = std::time::Instant::now();
        let output = Command::new(env!("CARGO_BIN_EXE_man"))
            .args(["-c", "-l"])
            .arg(&page)
            .args(["-C", "man.test.conf"])
            .output()
            .expect("Failed to run man -c -l");
        let elapsed = start.elapsed();
        let _ = std::fs::remove_file(&page);

        // A clean parse error (exit 1), not a panic (101) or stack overflow (134).
        assert_eq!(output.status.code(), Some(1), "expected a graceful error");
        assert!(
            String::from_utf8_lossy(&output.stderr).contains("nested macros"),
            "expected the nesting diagnostic, got: {}",
            String::from_utf8_lossy(&output.stderr)
        );
        assert!(
            elapsed < std::time::Duration::from_secs(5),
            "rejection must be fast, took {elapsed:?}"
        );
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
}
