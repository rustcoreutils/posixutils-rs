//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! `if`/`else`/`endif` in command mode, start-up and sourced files.

use super::capitalized::capitalized_fixture;
use crate::common::{copy_test_data, create_temp_mailrc, mailx, mailx_env};

/// Test if r (receive mode) conditional in mailrc
#[test]
fn conditional_if_receive_mode() {
    let mbox = copy_test_data("testdata.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    // Create mailrc with conditional for receive mode
    let mailrc = create_temp_mailrc("if r\nset receive_var\nendif\nif s\nset send_var\nendif\n");
    let mailrc_path = mailrc.path().to_str().unwrap();

    let output = mailx_env(
        &["-N", "-f", mbox_path],
        "set\nquit\n",
        &[("MAILRC", mailrc_path)],
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    // In receive mode, receive_var should be set, send_var should not
    assert!(
        stdout.contains("receive_var"),
        "if r should execute in receive mode: {}",
        stdout
    );
    assert!(
        !stdout.contains("send_var"),
        "if s should not execute in receive mode: {}",
        stdout
    );
    assert!(output.status.success());
}

/// Test if s (send mode) conditional in mailrc
#[test]
fn conditional_if_send_mode() {
    // Create mailrc with conditional for send mode
    let mailrc =
        create_temp_mailrc("if s\nset send_var\nendif\nif r\nset receive_var\nendif\nset debug\n");
    let mailrc_path = mailrc.path().to_str().unwrap();

    let output = mailx_env(
        &["-s", "Test", "user@example.com"],
        "body\n",
        &[("MAILRC", mailrc_path)],
    );
    // Debug mode should show we're in send mode
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("Debug mode"),
        "Should be in debug/send mode: {}",
        stderr
    );
    assert!(output.status.success());
}

/// Test else branch in conditional
#[test]
fn conditional_else_branch() {
    let mbox = copy_test_data("testdata.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    // In receive mode: if s (false) -> else branch should execute
    let mailrc = create_temp_mailrc("if s\nset wrong_var\nelse\nset correct_var\nendif\n");
    let mailrc_path = mailrc.path().to_str().unwrap();

    let output = mailx_env(
        &["-N", "-f", mbox_path],
        "set\nquit\n",
        &[("MAILRC", mailrc_path)],
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    // In receive mode, else branch should execute
    assert!(
        stdout.contains("correct_var"),
        "else branch should execute when if condition is false: {}",
        stdout
    );
    assert!(
        !stdout.contains("wrong_var"),
        "if branch should not execute when condition is false: {}",
        stdout
    );
    assert!(output.status.success());
}

/// Test nested conditionals
#[test]
fn conditional_nested() {
    let mbox = copy_test_data("testdata.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    // Nested if r inside if r (both true in receive mode)
    let mailrc = create_temp_mailrc("if r\nset outer_var\nif r\nset inner_var\nendif\nendif\n");
    let mailrc_path = mailrc.path().to_str().unwrap();

    let output = mailx_env(
        &["-N", "-f", mbox_path],
        "set\nquit\n",
        &[("MAILRC", mailrc_path)],
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    // Both should be set
    assert!(
        stdout.contains("outer_var") && stdout.contains("inner_var"),
        "Nested conditionals should work: {}",
        stdout
    );
    assert!(output.status.success());
}

/// Test commands outside conditionals always execute
#[test]
fn conditional_commands_outside() {
    let mbox = copy_test_data("testdata.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    let mailrc =
        create_temp_mailrc("set always_set\nif s\nset send_only\nendif\nset also_always\n");
    let mailrc_path = mailrc.path().to_str().unwrap();

    let output = mailx_env(
        &["-N", "-f", mbox_path],
        "set\nquit\n",
        &[("MAILRC", mailrc_path)],
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    // Commands outside if should always execute
    assert!(
        stdout.contains("always_set") && stdout.contains("also_always"),
        "Commands outside conditionals should execute: {}",
        stdout
    );
    assert!(
        !stdout.contains("send_only"),
        "if s commands should not run in receive mode: {}",
        stdout
    );
    assert!(output.status.success());
}

/// `if`/`else`/`endif` gate commands in command mode. Receive Mode
/// makes the `r` condition true and `s` false.
#[test]
fn if_conditional_in_command_mode() {
    let mbox = copy_test_data("testdata.mbox");

    let output = mailx(
        &["-n", "-N", "-f", mbox.path().to_str().unwrap()],
        "if r\necho YESBLOCK\nendif\nif s\necho NOBLOCK\nendif\nquit\n",
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("YESBLOCK"),
        "`if r` body should execute in receive mode: {}",
        stdout
    );
    assert!(
        !stdout.contains("NOBLOCK"),
        "`if s` body must not execute in receive mode: {}",
        stdout
    );
    assert_eq!(output.status.code(), Some(0), "exit status");
}

/// A second `else` in the same conditional block is diagnosed.
#[test]
fn double_else_is_diagnosed() {
    let mbox = copy_test_data("testdata.mbox");

    let output = mailx(
        &["-n", "-N", "-f", mbox.path().to_str().unwrap()],
        "if r\nelse\nelse\nquit\n",
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("unexpected else"),
        "a second else should be diagnosed: {}",
        stderr
    );
    assert_eq!(output.status.code(), Some(0), "exit status");
}

/// `if`/`else`/`endif` work in a file read by `source`.
#[test]
fn source_honors_conditionals() {
    let mbox = capitalized_fixture();
    let mbox_path = mbox.path().to_str().unwrap().to_string();
    let script = create_temp_mailrc(
        "if r\nset marker=receive-branch\nelse\nset marker=send-branch\nendif\n",
    );
    let script_path = script.path().to_str().unwrap().to_string();

    let output = mailx(
        &["-n", "-N", "-f", &mbox_path],
        format!("source {}\nset\nexit\n", script_path),
    );
    let out = String::from_utf8_lossy(&output.stdout);
    assert!(
        out.contains("marker=\"receive-branch\""),
        "a conditional in a sourced file was ignored: {}",
        out
    );
    assert_eq!(output.status.code(), Some(0), "exit status");
}
