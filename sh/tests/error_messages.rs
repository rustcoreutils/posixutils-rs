//
// Copyright (c) 2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! The diagnostics the shell writes, and the exit statuses that go with
//! them.

use plib::testing::{run_test, TestPlan};

fn sh_c(script: &str, expected_out: &str, expected_err: &str, expected_exit_code: i32) {
    run_test(TestPlan {
        cmd: "sh".to_string(),
        args: vec!["-c".to_string(), script.to_string()],
        stdin_data: String::new(),
        expected_out: expected_out.to_string(),
        expected_err: expected_err.to_string(),
        expected_exit_code,
    });
}

// A built-in utility's diagnostic is one line, with no blank line after it.
#[test]
fn builtin_diagnostic_is_one_line() {
    sh_c("cd ''; echo $?", "1\n", "cd: empty directory operand\n", 0);
    sh_c(
        "set -o nosuchopt; echo $?",
        "",
        "set: invalid option 'nosuchopt'\n",
        1,
    );
}

const MISSING_DIR: &str = "/nonexistent-posixutils-sh-dir";
const MISSING_FILE: &str = "/nonexistent-posixutils-sh-dir/x";

/// The system's text for opening a path in a directory that does not exist.
fn enoent_text() -> String {
    plib::testing::open_error_text(MISSING_FILE)
}

// A failed redirection names the file and whether it was being opened for
// reading or created; the status is 1 and the command does not run.
#[test]
fn failed_redirection_names_the_file() {
    let open = format!("sh: cannot open '{MISSING_FILE}': {}\n", enoent_text());
    let create = format!("sh: cannot create '{MISSING_FILE}': {}\n", enoent_text());
    sh_c(&format!("cat < {MISSING_FILE}; echo $?"), "1\n", &open, 0);
    sh_c(
        &format!("echo a > {MISSING_FILE}; echo $?"),
        "1\n",
        &create,
        0,
    );
    sh_c(
        &format!("echo a >> {MISSING_FILE}; echo $?"),
        "1\n",
        &create,
        0,
    );
    sh_c(
        &format!("echo a >| {MISSING_FILE}; echo $?"),
        "1\n",
        &create,
        0,
    );
    sh_c(
        &format!("cat <> {MISSING_FILE}; echo $?"),
        "1\n",
        &create,
        0,
    );
    sh_c(
        &format!("set -C; echo a > {MISSING_FILE}; echo $?"),
        "1\n",
        &create,
        0,
    );
    sh_c(
        &format!("x=$(cat < {MISSING_FILE}); echo $?"),
        "1\n",
        &open,
        0,
    );
}

// XCU 2.8.1: a redirection error on a special built-in exits a
// non-interactive shell; on a compound command, a function or another
// utility it does not. Only the message changed.
#[test]
fn failed_redirection_status_follows_xcu_2_8_1() {
    let open = format!("sh: cannot open '{MISSING_FILE}': {}\n", enoent_text());
    let create = format!("sh: cannot create '{MISSING_FILE}': {}\n", enoent_text());
    sh_c(&format!("exec < {MISSING_FILE}; echo after"), "", &open, 1);
    sh_c(&format!(": > {MISSING_FILE}; echo after"), "", &create, 1);
    sh_c(
        &format!("{{ :; }} > {MISSING_FILE}; echo after $?"),
        "after 1\n",
        &create,
        0,
    );
    sh_c(
        &format!("f() {{ :; }}; f > {MISSING_FILE}; echo after $?"),
        "after 1\n",
        &create,
        0,
    );
    sh_c(
        &format!("command : > {MISSING_FILE}; echo after $?"),
        "after 1\n",
        &create,
        0,
    );
}

// A file that cannot be created for another reason gets that reason.
#[test]
fn failed_redirection_reports_the_reason() {
    // The reason is the system's: creating "/" is EISDIR on Linux and EEXIST
    // on macOS, so ask the system with the flags `>` uses.
    let err = std::fs::OpenOptions::new()
        .write(true)
        .create(true)
        .truncate(true)
        .open("/")
        .expect_err("/ must not be creatable as a file");
    let reason = plib::diag::io_error_text(&err);
    sh_c(
        "echo a > /; echo $?",
        "1\n",
        &format!("sh: cannot create '/': {reason}\n"),
        0,
    );
}

// cd and dot name their operand too.
#[test]
fn cd_and_dot_failures_name_the_operand() {
    sh_c(
        &format!("cd {MISSING_DIR}; echo $?"),
        "1\n",
        &format!(
            "cd: cannot change directory to '{MISSING_DIR}': {}\n",
            plib::testing::open_error_text(MISSING_DIR)
        ),
        0,
    );
    sh_c(
        ". /; echo $?",
        "",
        "dot: cannot read '/': Is a directory\n",
        1,
    );
}

// A built-in utility that cannot write its output reports it once and
// exits 1, as dash and bash do, rather than reporting every failed write
// and exiting 0.
#[test]
fn builtin_write_error_is_reported_once_with_status_1() {
    sh_c(
        "pwd >&-; echo $?",
        "1\n",
        "pwd: write error: Bad file descriptor\n",
        0,
    );
    sh_c(
        "export A=1 B=2; export -p >&-; echo $?",
        "1\n",
        "export: write error: Bad file descriptor\n",
        0,
    );
    sh_c("pwd 2>&- >/dev/null; echo $?", "0\n", "", 0);
}

// "not found" names the command without quotes, as `NAME: not found`, the
// form dash and bash use. GNU make's test suite captures the message and
// echoes it inside single quotes, so quotes in it were lost and its
// functions/shell test failed.
#[test]
fn command_not_found_message_form() {
    sh_c(
        "no_such_cmd_xyz",
        "",
        "sh: no_such_cmd_xyz: not found\n",
        127,
    );
    sh_c(
        "out=$(no_such_cmd_xyz 2>&1); echo \"$out\"",
        "sh: no_such_cmd_xyz: not found\n",
        "",
        0,
    );
}

// The built-ins that search for a command report a failed search in the
// same unquoted `NAME: not found` form.
#[test]
fn builtin_not_found_message_form() {
    sh_c(
        "exec no_such_cmd_xyz",
        "",
        "exec: no_such_cmd_xyz: not found\n",
        127,
    );
    sh_c(
        "command no_such_cmd_xyz",
        "",
        "command: no_such_cmd_xyz: not found\n",
        127,
    );
    sh_c(
        "unalias no_such_alias_xyz",
        "",
        "unalias: no_such_alias_xyz: not found\n",
        1,
    );
}

// An exec that fails with an errno the shell has no name for is reported
// like any other failure (status 126), not a panic. The kernel gives
// ELIBBAD for a program whose ELF interpreter is not an ELF file; the shell
// panicked converting it.
#[cfg(target_os = "linux")]
#[test]
fn exec_failure_with_an_unnamed_errno_is_reported() {
    use std::os::unix::fs::PermissionsExt;
    use std::process::Command;

    let dir = concat!(env!("CARGO_TARGET_TMPDIR"), "/sh_unnamed_errno");
    let _ = std::fs::remove_dir_all(dir);
    std::fs::create_dir_all(dir).unwrap();

    // A copy of a dynamically linked program, with its PT_INTERP pointing at
    // a text file. Read in the host's byte order, which is the ELF's.
    let mut elf = std::fs::read("/bin/sh").unwrap();
    let u16_at = |e: &[u8], o: usize| u16::from_ne_bytes(e[o..o + 2].try_into().unwrap());
    let u32_at = |e: &[u8], o: usize| u32::from_ne_bytes(e[o..o + 4].try_into().unwrap());
    let u64_at = |e: &[u8], o: usize| u64::from_ne_bytes(e[o..o + 8].try_into().unwrap());
    if &elf[..4] != b"\x7fELF" || elf[4] != 2 {
        return; // not a 64-bit ELF
    }
    let phoff = u64_at(&elf, 0x20) as usize;
    let phentsize = u16_at(&elf, 0x36) as usize;
    let phnum = u16_at(&elf, 0x38) as usize;
    let Some(interp) = (0..phnum)
        .map(|i| phoff + i * phentsize)
        .find(|&ph| u32_at(&elf, ph) == 3)
    else {
        return; // statically linked
    };
    let offset = u64_at(&elf, interp + 8) as usize;
    let size = u64_at(&elf, interp + 32) as usize;
    let path = b"./i";
    elf[offset..offset + size].fill(0);
    elf[offset..offset + path.len()].copy_from_slice(path);
    for (name, contents) in [("prog", elf), ("i", b"not an ELF file\n".repeat(16))] {
        let file = format!("{dir}/{name}");
        std::fs::write(&file, contents).unwrap();
        std::fs::set_permissions(&file, std::fs::Permissions::from_mode(0o755)).unwrap();
    }

    let output = Command::new(env!("CARGO_BIN_EXE_sh"))
        .args(["-c", &format!("cd {dir} && ./prog; echo $?")])
        .output()
        .unwrap();
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert_eq!(String::from_utf8_lossy(&output.stdout), "126\n", "{stderr}");
    assert!(
        stderr.starts_with("sh: failed to execute ./prog"),
        "{stderr}"
    );
    assert!(output.status.success());
    std::fs::remove_dir_all(dir).unwrap();
}

// A diagnostic that cannot be written (stderr on a full device) is lost,
// but the shell goes on as if it had been written: a syntax error still
// exits 2 and a bad trap action does not stop the script, as in bash. The
// std print macros panicked instead (status 101).
#[cfg(target_os = "linux")]
#[test]
fn unwritable_diagnostics_do_not_panic() {
    use std::process::{Command, Stdio};

    let run = |script: &str| {
        let full = std::fs::OpenOptions::new()
            .write(true)
            .open("/dev/full")
            .unwrap();
        Command::new(env!("CARGO_BIN_EXE_sh"))
            .args(["-c", script])
            .stderr(Stdio::from(full))
            .output()
            .unwrap()
    };
    let syntax = run("if");
    assert_eq!(syntax.status.code(), Some(2));
    let trap = run("trap 'if' USR1; kill -USR1 $$; /bin/true; echo ok");
    assert_eq!(String::from_utf8_lossy(&trap.stdout), "ok\n");
    assert_eq!(trap.status.code(), Some(0));
}
