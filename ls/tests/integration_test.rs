use std::process::Command;

fn run_command(command: &str, args: &[&str]) -> String {
    let output = Command::new(command)
        .args(args)
        .output()
        .expect("failed to execute command");

    assert!(output.status.success(), "Command did not run successfully");

    // Trim and return the output as a string
    String::from_utf8_lossy(&output.stdout).trim().to_string()
}


#[test]
fn test__() {
    let args = []; // Long listing format argument
    let ls_output = run_command("ls", &args);
    let rust_ls_output = run_command("./target/debug/rust_ls", &args);
   assert_eq!(ls_output, rust_ls_output, "Outputs of ls and rust_ls do not match");
}

#[test]
fn test_l() {
    let args = ["-l"]; // Long listing format argument
    let ls_output = run_command("ls", &args);
    let rust_ls_output = run_command("./target/debug/rust_ls", &args);
    assert_eq!(ls_output, rust_ls_output, "Outputs of ls -l and rust_ls -l do not match");
}

#[test]
fn test_i() {
    let args = ["-i"]; // Long listing format argument
    let ls_output = run_command("ls", &args);
    let rust_ls_output = run_command("./target/debug/rust_ls", &args);
    assert_eq!(ls_output, rust_ls_output, "Outputs of ls -i and rust_ls -i do not match");
}

#[test]
fn test_a() {
    let args = ["-a"]; // Long listing format argument
    let ls_output = run_command("ls", &args);
    let rust_ls_output = run_command("./target/debug/rust_ls", &args);
    assert_eq!(ls_output, rust_ls_output, "Outputs of ls -a and rust_ls -a do not match");
}

#[test]
fn test_A() {
    let args = ["-A"]; // Long listing format argument
    let ls_output = run_command("ls", &args);
    let rust_ls_output = run_command("./target/debug/rust_ls", &args);
    assert_eq!(ls_output, rust_ls_output, "Outputs of ls -A and rust_ls -A do not match");
}

#[test]
fn test_m() {
    let args = ["-m"]; // Long listing format argument
    let ls_output = run_command("ls", &args);
    let rust_ls_output = run_command("./target/debug/rust_ls", &args);
    assert_eq!(ls_output, rust_ls_output, "Outputs of ls -m and rust_ls -m do not match");
}


#[test]
fn test_r() {
    let args = ["-r"]; // Long listing format argument
    let ls_output = run_command("ls", &args);
    let rust_ls_output = run_command("./target/debug/rust_ls", &args);
    assert_eq!(ls_output, rust_ls_output, "Outputs of ls -r and rust_ls -r do not match");
}
#[test]
fn test_lr() {
    let args = ["-lr"]; // Long listing format argument
    let ls_output = run_command("ls", &args);
    let rust_ls_output = run_command("./target/debug/rust_ls", &args);
    assert_eq!(ls_output, rust_ls_output, "Outputs of ls -r and rust_ls -r do not match");
}


#[test]
fn test_q() {
    let args = ["-q"]; // Long listing format argument
    let ls_output = run_command("ls", &args);
    let rust_ls_output = run_command("./target/debug/rust_ls", &args);
    assert_eq!(ls_output, rust_ls_output, "Outputs of ls -q and rust_ls -q do not match");
}


#[test]
fn test_n() {
    let args = ["-n"]; // Long listing format argument
    let ls_output = run_command("ls", &args);
    let rust_ls_output = run_command("./target/debug/rust_ls", &args);
    assert_eq!(ls_output, rust_ls_output, "Outputs of ls -n and rust_ls -n do not match");
}

#[test]
fn test_s() {
    let args = ["-s"]; // Long listing format argument
    let ls_output = run_command("ls", &args);
    let rust_ls_output = run_command("./target/debug/rust_ls", &args);
    assert_eq!(ls_output, rust_ls_output, "Outputs of ls -s and rust_ls -s do not match");
}


#[test]
fn test_S() {
    let args = ["-S"]; // Long listing format argument
    let ls_output = run_command("ls", &args);
    let rust_ls_output = run_command("./target/debug/rust_ls", &args);
    assert_eq!(ls_output, rust_ls_output, "Outputs of ls -S and rust_ls -S do not match");
}
#[test]
fn test_p() {
    let args = ["-p"]; // Long listing format argument
    let ls_output = run_command("ls", &args);
    let rust_ls_output = run_command("./target/debug/rust_ls", &args);
   assert_eq!(ls_output, rust_ls_output, "Outputs of ls -p and rust_ls -p do not match");
}