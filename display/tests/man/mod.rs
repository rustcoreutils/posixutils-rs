// tests/man.rs

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
}
