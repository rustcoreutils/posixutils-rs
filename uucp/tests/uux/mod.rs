//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::fake_ssh::{read, FakeSsh};
use plib::testing::{run_test_with_checker, TestPlan};
use std::fs;
use std::process::Output;

fn uux_test_with_checker<F: FnMut(&TestPlan, &Output)>(args: &[&str], checker: F) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test_with_checker(
        TestPlan {
            cmd: String::from("uux"),
            args: str_args,
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        checker,
    );
}

#[test]
fn test_uux_no_args() {
    // No arguments should show usage and fail
    uux_test_with_checker(&[], |_, output| {
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(!output.status.success());
        assert!(
            stderr.contains("Usage:") || stderr.contains("usage:"),
            "Expected usage message in stderr, got: {}",
            stderr
        );
    });
}

#[test]
fn test_uux_invalid_option() {
    // Invalid option should fail
    uux_test_with_checker(&["-X", "echo hello"], |_, output| {
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(!output.status.success());
        assert!(
            stderr.contains("unexpected argument") || stderr.contains("unknown option"),
            "Expected error about invalid option in stderr, got: {}",
            stderr
        );
    });
}

#[test]
fn test_uux_local_command() {
    // Run a simple local command (no system prefix = local)
    uux_test_with_checker(&["echo hello"], |_, output| {
        assert!(output.status.success());
    });
}

#[test]
fn test_uux_local_command_with_prefix() {
    // Explicit local system with ! prefix
    uux_test_with_checker(&["!echo hello"], |_, output| {
        assert!(output.status.success());
    });
}

#[test]
fn test_uux_with_j_option() {
    // -j should print job ID
    uux_test_with_checker(&["-j", "echo test"], |_, output| {
        assert!(output.status.success());
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(!stdout.trim().is_empty(), "Should print job ID with -j");
    });
}

#[test]
fn test_uux_disallowed_append_redirect() {
    // >> is not allowed
    uux_test_with_checker(&["echo hello >> /tmp/file"], |_, output| {
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(!output.status.success());
        assert!(
            stderr.contains(">>"),
            "Expected '>>' in stderr, got: {}",
            stderr
        );
    });
}

#[test]
fn test_uux_disallowed_heredoc() {
    // << is not allowed
    uux_test_with_checker(&["cat << EOF"], |_, output| {
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(!output.status.success());
        assert!(
            stderr.contains("<<"),
            "Expected '<<' in stderr, got: {}",
            stderr
        );
    });
}

#[test]
fn test_uux_disallowed_clobber() {
    // >| is not allowed
    uux_test_with_checker(&["echo test >| /tmp/file"], |_, output| {
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(!output.status.success());
        assert!(
            stderr.contains(">|"),
            "Expected '>|' in stderr, got: {}",
            stderr
        );
    });
}

#[test]
fn test_uux_disallowed_redirect_both() {
    // >& is not allowed
    uux_test_with_checker(&["command >& /tmp/file"], |_, output| {
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(!output.status.success());
        assert!(
            stderr.contains(">&"),
            "Expected '>&' in stderr, got: {}",
            stderr
        );
    });
}

#[test]
fn test_uux_pipe_stdin() {
    // -p should be accepted
    uux_test_with_checker(&["-p", "cat"], |_, output| {
        // This will succeed since stdin is empty
        assert!(output.status.success());
    });
}

#[test]
fn test_uux_n_option_accepted() {
    // -n should be accepted (suppress notification)
    uux_test_with_checker(&["-n", "echo test"], |_, output| {
        assert!(output.status.success());
    });
}

#[test]
fn test_uux_combined_options() {
    // Combined options should work
    uux_test_with_checker(&["-jn", "echo combined"], |_, output| {
        assert!(output.status.success());
        // Should print job ID
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(!stdout.trim().is_empty());
    });
}

#[test]
fn test_uux_dash_as_p() {
    // Single dash is equivalent to -p
    uux_test_with_checker(&["-", "cat"], |_, output| {
        assert!(output.status.success());
    });
}

#[test]
fn test_uux_command_with_args() {
    // Command with multiple arguments
    uux_test_with_checker(&["echo one two three"], |_, output| {
        assert!(output.status.success());
    });
}

#[test]
fn test_uux_command_exit_status() {
    // Command that succeeds
    uux_test_with_checker(&["true"], |_, output| {
        assert!(output.status.success());
    });

    // Command that fails
    uux_test_with_checker(&["false"], |_, output| {
        assert!(!output.status.success());
    });
}

#[test]
fn test_uux_simple_redirect() {
    let test_dir = &format!("{}/test_uux_redirect", env!("CARGO_TARGET_TMPDIR"));
    let output_file = &format!("{}/output.txt", test_dir);

    // Setup
    fs::create_dir_all(test_dir).unwrap();

    // Simple > redirect should be allowed
    let cmd = format!("echo redirected > {}", output_file);
    uux_test_with_checker(&[&cmd], |_, output| {
        assert!(output.status.success());
        // Verify file was created
        assert!(std::path::Path::new(output_file).exists());
        let content = fs::read_to_string(output_file).unwrap();
        assert!(content.contains("redirected"));
    });

    // Cleanup
    fs::remove_dir_all(test_dir).unwrap();
}

/// Check what `ls -ld "$PWD"`, run by uux, wrote to `listing`: the command
/// ran in a directory of its own under `$TMPDIR`, readable only by its owner.
fn assert_private_work_dir(fake: &FakeSsh, listing: &str) {
    assert!(listing.starts_with("drwx------"), "work dir: {listing}");
    let tmp = fake.tmp().display().to_string();
    assert!(listing.contains(&format!("{tmp}/")), "work dir: {listing}");
    assert!(
        fake.tmp_entries().is_empty(),
        "left behind: {:?}",
        fake.tmp_entries()
    );
}

/// A local command runs in a fresh private directory under `$TMPDIR`, not in
/// a predictable `/tmp` name another user could have made first, and the
/// directory is removed afterwards.
#[test]
fn test_uux_local_work_dir_is_private() {
    let fake = FakeSsh::new("uux_local");
    let out = fake.join("out");
    let cmd = format!("ls -ld \"$PWD\" > {}", out.display());

    let output = fake.run("uux", &[&cmd], "");

    assert!(output.status.success(), "{output:?}");
    assert_private_work_dir(&fake, &read(&out));
}

/// The same holds for the work directory made on a remote execution host.
#[test]
fn test_uux_remote_work_dir_is_private() {
    let fake = FakeSsh::new("uux_remote");
    let out = fake.join("out");
    let cmd = format!("hosta!ls -ld \"$PWD\" > !{}", out.display());

    let output = fake.run("uux", &[&cmd], "");

    assert!(output.status.success(), "{output:?}");
    assert_private_work_dir(&fake, &read(&out));
}

/// A remote shell whose start-up files print something (a banner, a
/// fortune) on standard output must not break the creation of the remote
/// work directory, which reports its path there.
#[test]
fn test_uux_remote_work_dir_survives_startup_output() {
    let fake = FakeSsh::new("uux_remote_noisy");
    let out = fake.join("out");
    let cmd = format!("hosta!ls -ld \"$PWD\" > !{}", out.display());
    let noise = "case \"$5\" in *mkdir*) printf 'Welcome to hosta\\n/not/a/dir\\n';; esac";

    let output = fake.run("uux", &[&cmd], noise);

    assert!(output.status.success(), "{output:?}");
    assert_private_work_dir(&fake, &read(&out));
}

/// The remote work directory's name is random in its own right: one that
/// could be read off a listing of the local temporary directory would let
/// anyone there make it first on the execution host.
#[test]
fn test_uux_remote_work_dir_name_is_not_the_local_one() {
    let fake = FakeSsh::new("uux_remote_name");
    let (out, log) = (fake.join("out"), fake.join("log"));
    let cmd = format!("hosta!ls -ld \"$PWD\" > !{}", out.display());
    let hook = format!(
        "case \"$5\" in *mkdir*) ls \"$TMPDIR\" > '{}';; esac",
        log.display()
    );

    let output = fake.run("uux", &[&cmd], &hook);

    assert!(output.status.success(), "{output:?}");
    let listing = read(&out);
    let remote = listing.trim_end().rsplit('/').next().unwrap().to_string();
    let local = read(&log);
    let local = local.trim();
    let suffix = local.rsplit('.').next().unwrap();
    assert!(local.starts_with("uux."), "local dir: {local:?}");
    assert!(
        !remote.contains(suffix),
        "remote {remote:?} derived from local {local:?}"
    );
}

/// If the remote work directory is made but its path cannot be read back
/// (here the stand-in ssh discards the command's output), uux fails and
/// still removes the directory.
#[test]
fn test_uux_remote_work_dir_removed_when_path_is_lost() {
    let fake = FakeSsh::new("uux_remote_lost");
    let cmd = "hosta!true";
    let discard =
        "case \"$5\" in *mkdir*) set -- \"$1\" \"$2\" \"$3\" \"$4\" \"$5 >/dev/null\";; esac";

    let output = fake.run("uux", &["-n", cmd], discard);

    assert!(!output.status.success(), "{output:?}");
    assert!(
        fake.tmp_entries().is_empty(),
        "left behind: {:?}",
        fake.tmp_entries()
    );
}

/// An input file from a third system is staged locally on its way to the
/// execution host: in a private directory under `$TMPDIR`, which is gone
/// afterwards. The hook lists `$TMPDIR` at each ssh call, so the listing
/// taken while the staged copy is sent shows where it was.
#[test]
fn test_uux_stages_a_remote_input_privately() {
    let fake = FakeSsh::new("uux_stage");
    let (src, out, log) = (fake.join("src"), fake.join("out"), fake.join("log"));
    fs::write(&src, "staged input\n").unwrap();
    let hook = format!("ls -lAR \"$TMPDIR\" >> '{}'", log.display());
    let cmd = format!("hosta!cat hostb!{} > !{}", src.display(), out.display());

    let output = fake.run("uux", &[&cmd], &hook);

    assert!(output.status.success(), "{output:?}");
    assert_eq!(read(&out), "staged input\n");
    let log = read(&log);
    assert!(
        log.lines()
            .any(|l| l.starts_with('-') && l.ends_with(" src")),
        "staged copy not under $TMPDIR:\n{log}"
    );
    for dir in log.lines().filter(|l| l.starts_with('d')) {
        assert!(dir.starts_with("drwx------"), "listing:\n{log}");
    }
    assert!(fake.tmp_entries().is_empty(), "{:?}", fake.tmp_entries());
}

#[test]
fn test_uux_too_many_args() {
    // Only one command string should be accepted
    uux_test_with_checker(&["cmd1", "cmd2"], |_, output| {
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(!output.status.success());
        assert!(
            stderr.contains("too many"),
            "Expected 'too many' in stderr, got: {}",
            stderr
        );
    });
}
