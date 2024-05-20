use assert_cmd::Command;
use predicates::prelude::*;
use std::fs;

fn run_sh(command: &str) -> assert_cmd::assert::Assert {
    let mut cmd = Command::cargo_bin("sh").expect("Failed to find sh binary");
    cmd.arg("-c").arg(command);
    cmd.assert()
}

#[test]
fn sh_simple_command() {
    run_sh("echo Hello, world!")
        .success()
        .stdout(predicates::str::contains("Hello, world!"));
}

#[test]
fn sh_piped_commands() {
    run_sh("echo Hello, world! | tr a-z A-Z")
        .success()
        .stdout(predicates::str::contains("HELLO, WORLD!"));
}

#[test]
fn sh_input_redirection() {
    fs::write("input.txt", "Hello, world!").expect("Failed to write to input.txt");
    run_sh("cat < input.txt")
        .success()
        .stdout(predicates::str::contains("Hello, world!"));
    fs::remove_file("input.txt").expect("Failed to delete input.txt");
}

#[test]
fn sh_output_redirection() {
    run_sh("echo Hello, world! > output.txt").success();
    let output_content = fs::read_to_string("output.txt").expect("Failed to read output.txt");
    assert!(output_content.contains("Hello, world!"));
    fs::remove_file("output.txt").expect("Failed to delete output.txt");
}

#[test]
fn sh_append_redirection() {
    fs::write("append.txt", "First line.").expect("Failed to write to append.txt");
    run_sh("echo Second line. >> append.txt").success();
    let output_content = fs::read_to_string("append.txt").expect("Failed to read append.txt");
    assert!(output_content.contains("First line."));
    assert!(output_content.contains("Second line."));
    fs::remove_file("append.txt").expect("Failed to delete append.txt");
}

#[test]
fn sh_and_operator() {
    run_sh("echo Hello && echo world")
        .success()
        .stdout(predicates::str::contains("Hello").and(predicates::str::contains("world")));
}

#[test]
fn sh_or_operator() {
    run_sh("false || echo Hello")
        .success()
        .stdout(predicates::str::contains("Hello"));
}

#[test]
fn sh_combined_operators() {
    run_sh("echo Hello && false || echo world")
        .success()
        .stdout(predicates::str::contains("Hello").and(predicates::str::contains("world")));
}

#[test]
fn sh_cd_command() {
    // Create a temporary directory
    let temp_dir = tempfile::tempdir().expect("Failed to create temp dir");
    let temp_path = temp_dir.path().to_str().expect("Failed to get temp path");

    // Change directory to the temporary directory and check the current directory
    run_sh(&format!("cd {} && pwd", temp_path))
        .success()
        .stdout(predicates::str::contains(temp_path));

    // Cleanup is automatic when temp_dir goes out of scope
}

#[test]
fn sh_exit_command() {
    // The `exit` command should terminate the shell without an error
    run_sh("exit").success();
}
