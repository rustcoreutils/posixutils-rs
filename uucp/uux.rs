//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
//! uux - remote command execution
//!
//! POSIX-compliant implementation using SSH for transport.

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use posixutils_uucp::common::{
    current_login, expand_remote_path, generate_job_id, is_local_system, parse_path_spec,
    send_mail, shell_escape, ssh_exec, ssh_fetch_file, ssh_send_file,
};
use std::io::{self, Read};
use std::process::ExitCode;

/// uux - remote command execution
#[derive(Parser)]
#[command(
    version,
    about = gettext("uux - remote command execution"),
    override_usage = "uux [-jnp] [-] command-string"
)]
struct Args {
    #[arg(short = 'j', help = gettext("Write job ID to stdout"))]
    print_job_id: bool,

    #[arg(short = 'n', help = gettext("Do not send mail notification on failure"))]
    no_notify: bool,

    #[arg(short = 'p', help = gettext("Read stdin and pipe to command"))]
    pipe_stdin: bool,

    #[arg(required = true, help = gettext("Command string to execute"))]
    command: Vec<String>,
}

fn main() -> ExitCode {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs").ok();
    bind_textdomain_codeset("posixutils-rs", "UTF-8").ok();

    let args = Args::parse();

    // Handle command arguments - check for "-" which means pipe stdin
    let mut pipe_stdin = args.pipe_stdin;
    let mut command_parts: Vec<&str> = Vec::new();

    for part in &args.command {
        if part == "-" {
            pipe_stdin = true;
        } else {
            command_parts.push(part);
        }
    }

    // Check for too many non-option arguments (should be exactly one command string)
    if command_parts.len() > 1 {
        eprintln!("uux: {}", gettext("too many arguments"));
        eprintln!("{}", gettext("usage: uux [-jnp] [-] command-string"));
        return ExitCode::from(1);
    }

    let cmd_str = match command_parts.first() {
        Some(s) => *s,
        None => {
            eprintln!("{}", gettext("usage: uux [-jnp] [-] command-string"));
            return ExitCode::from(1);
        }
    };

    // Check for disallowed redirection operators
    if cmd_str.contains(">>")
        || cmd_str.contains("<<")
        || cmd_str.contains(">|")
        || cmd_str.contains(">&")
    {
        eprintln!(
            "uux: {}",
            gettext("redirection operators >>, <<, >|, >& not allowed")
        );
        return ExitCode::from(1);
    }

    // Parse the command string to find execution host and files
    let parsed = match parse_command_string(cmd_str) {
        Ok(p) => p,
        Err(e) => {
            eprintln!("uux: {}", e);
            return ExitCode::from(1);
        }
    };

    // Reject an output pathname containing an encoded <newline> (POSIX FUTURE
    // DIRECTIONS, Austin Group Defect 251).
    if let Some(ref out) = parsed.output_file {
        if out.path.contains('\n') {
            eprintln!("uux: {}", gettext("output pathname contains a newline"));
            return ExitCode::from(1);
        }
    }

    // Read stdin if -p specified
    let stdin_data = if pipe_stdin {
        let mut data = Vec::new();
        if io::stdin().read_to_end(&mut data).is_err() {
            eprintln!("uux: {}", gettext("failed to read stdin"));
            return ExitCode::from(1);
        }
        Some(data)
    } else {
        None
    };

    // Generate job ID
    let job_id = generate_job_id();

    if args.print_job_id {
        println!("{}", job_id);
    }

    // Execute the command
    let result = execute_uux(&parsed, stdin_data.as_deref(), &job_id);

    match result {
        Ok(_) => ExitCode::SUCCESS,
        Err(e) => {
            eprintln!("uux: {}", e);

            // Send mail notification on failure unless -n
            if !args.no_notify {
                let user = current_login();
                let msg = format!("uux command failed: {}\nError: {}", cmd_str, e);
                let _ = send_mail(&user, "uux job failed", &msg);
            }

            ExitCode::from(1)
        }
    }
}

/// Parsed command information
#[derive(Debug)]
struct ParsedCommand {
    exec_host: String,
    command: String,
    input_files: Vec<FileRef>,
    output_file: Option<FileRef>,
}

/// Reference to a file in the command
#[derive(Debug)]
struct FileRef {
    system: String,
    path: String,
}

/// Parse the uux command string
fn parse_command_string(cmd: &str) -> Result<ParsedCommand, String> {
    // Find the execution host (first command token with system! prefix)
    let tokens: Vec<&str> = cmd.split_whitespace().collect();
    if tokens.is_empty() {
        return Err("empty command".to_string());
    }

    // Parse first token for execution host
    let (exec_host, first_cmd) = parse_path_spec(tokens[0]);

    let mut input_files = Vec::new();
    let mut output_file: Option<FileRef> = None;
    let mut command_parts = Vec::new();

    // First command part (without system prefix)
    command_parts.push(first_cmd.clone());

    let mut i = 1;
    let mut in_redirect = false;

    while i < tokens.len() {
        let token = tokens[i];

        if token == ">" {
            // Output redirection - next token is the file
            in_redirect = true;
            command_parts.push(token.to_string());
            i += 1;
            continue;
        }

        if in_redirect {
            // This is the output file. An explicit `!` names a system (a null
            // system-name, i.e. a leading `!`, means the local system); a target
            // with no `!` at all defaults to the execution host.
            let (sys, path) = parse_path_spec(token);
            output_file = Some(FileRef {
                system: if token.contains('!') {
                    sys
                } else {
                    exec_host.clone()
                },
                path: expand_remote_path(&path),
            });
            command_parts.push(expand_remote_path(&path));
            in_redirect = false;
            i += 1;
            continue;
        }

        // Check if token starts with > (combined like >file)
        if let Some(rest) = token.strip_prefix('>') {
            let (sys, path) = parse_path_spec(rest);
            output_file = Some(FileRef {
                system: if rest.contains('!') {
                    sys
                } else {
                    exec_host.clone()
                },
                path: expand_remote_path(&path),
            });
            command_parts.push(format!(">{}", expand_remote_path(&path)));
            i += 1;
            continue;
        }

        // Check if this is a file reference (contains !)
        if token.contains('!') && !token.starts_with('!') {
            let (sys, path) = parse_path_spec(token);
            // This is an input file from another system
            if !is_local_system(&sys) && sys != exec_host {
                input_files.push(FileRef {
                    system: sys,
                    path: expand_remote_path(&path),
                });
            }
            // Use just the basename in the command
            let basename = std::path::Path::new(&path)
                .file_name()
                .map(|s| s.to_string_lossy().to_string())
                .unwrap_or(path);
            command_parts.push(basename);
        } else {
            command_parts.push(token.to_string());
        }

        i += 1;
    }

    Ok(ParsedCommand {
        exec_host,
        command: command_parts.join(" "),
        input_files,
        output_file,
    })
}

/// Execute the uux command
fn execute_uux(
    parsed: &ParsedCommand,
    stdin_data: Option<&[u8]>,
    job_id: &str,
) -> Result<(), String> {
    let exec_local = is_local_system(&parsed.exec_host);

    // Create working directory
    let work_dir = format!("/tmp/uux_{}", job_id);

    if exec_local {
        // Create local working directory
        std::fs::create_dir_all(&work_dir)
            .map_err(|e| format!("failed to create work dir: {}", e))?;
    } else {
        // Create remote working directory
        let work_dir_escaped = shell_escape(&work_dir);
        let (code, _, stderr) = ssh_exec(
            &parsed.exec_host,
            &format!("mkdir -p {}", work_dir_escaped),
            None,
        )
        .map_err(|e| format!("ssh failed: {}", e))?;
        if code != 0 {
            return Err(format!(
                "failed to create remote work dir: {}",
                String::from_utf8_lossy(&stderr)
            ));
        }
    }

    // Fetch input files to execution host
    for file in &parsed.input_files {
        let basename = std::path::Path::new(&file.path)
            .file_name()
            .map(|s| s.to_string_lossy().to_string())
            .unwrap_or(file.path.clone());

        if exec_local {
            // Fetch from remote to local work dir
            let local_path = format!("{}/{}", work_dir, basename);
            ssh_fetch_file(&file.system, &file.path, &local_path, true)
                .map_err(|e| format!("failed to fetch {}: {}", file.path, e))?;
        } else {
            // Fetch from source remote to local temp, then send to exec host
            let temp_path = format!("/tmp/uux_tmp_{}_{}", job_id, basename);
            ssh_fetch_file(&file.system, &file.path, &temp_path, true)
                .map_err(|e| format!("failed to fetch {}: {}", file.path, e))?;

            let remote_path = format!("{}/{}", work_dir, basename);
            ssh_send_file(&parsed.exec_host, &temp_path, &remote_path, true)
                .map_err(|e| format!("failed to send to exec host: {}", e))?;

            let _ = std::fs::remove_file(&temp_path);
        }
    }

    // Execute the command
    let work_dir_escaped = shell_escape(&work_dir);
    let full_cmd = format!("cd {} && {}", work_dir_escaped, parsed.command);

    let (code, stdout, stderr) = if exec_local {
        // Local execution
        use std::process::{Command, Stdio};
        let mut child = Command::new("sh")
            .args(["-c", &full_cmd])
            .stdin(if stdin_data.is_some() {
                Stdio::piped()
            } else {
                Stdio::null()
            })
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .map_err(|e| format!("failed to spawn: {}", e))?;

        if let Some(data) = stdin_data {
            use std::io::Write;
            if let Some(ref mut stdin) = child.stdin {
                let _ = stdin.write_all(data);
            }
        }

        let output = child
            .wait_with_output()
            .map_err(|e| format!("failed to wait: {}", e))?;

        (
            output.status.code().unwrap_or(1),
            output.stdout,
            output.stderr,
        )
    } else {
        // Remote execution
        ssh_exec(&parsed.exec_host, &full_cmd, stdin_data)
            .map_err(|e| format!("ssh exec failed: {}", e))?
    };

    // Print the command's standard error.
    if !stderr.is_empty() {
        eprint!("{}", String::from_utf8_lossy(&stderr));
    }

    // On success, deliver the output file to its target system if that differs
    // from the execution host (the `>` redirect ran on the exec host, so the
    // file currently lives there). Done before cleanup removes the work dir.
    let delivery = if code == 0 {
        match parsed.output_file {
            Some(ref out_file) => deliver_output(out_file, &parsed.exec_host, &work_dir),
            None => Ok(()),
        }
    } else {
        Ok(())
    };

    // Clean up the work directory.
    if exec_local {
        let _ = std::fs::remove_dir_all(&work_dir);
    } else {
        let work_dir_escaped = shell_escape(&work_dir);
        let _ = ssh_exec(
            &parsed.exec_host,
            &format!("rm -rf {}", work_dir_escaped),
            None,
        );
    }

    if code != 0 {
        return Err(format!("command exited with status {}", code));
    }
    delivery?;

    // The command's standard output is discarded per POSIX unless redirected.
    let _ = stdout;

    Ok(())
}

/// Deliver a `uux` output file from the execution host to the system named by
/// the redirect target, when that system differs from the exec host. The file
/// was produced by the `>` redirect on the exec host (relative targets live
/// under `work_dir`). Returns `Ok(())` when the target is the same host as the
/// exec host (already in place). A target on a third remote system (neither
/// local nor the exec host) is not supported in this minimal SSH design.
fn deliver_output(out_file: &FileRef, exec_host: &str, work_dir: &str) -> Result<(), String> {
    let out_local = is_local_system(&out_file.system);
    let exec_local = is_local_system(exec_host);

    // Same effective host: the redirect already wrote the file in place.
    if (out_local && exec_local) || (!out_local && out_file.system == exec_host) {
        return Ok(());
    }

    // Exec-host-side path of the produced file (the command ran `cd work_dir`,
    // so a relative redirect target lives under work_dir).
    let exec_path = if out_file.path.starts_with('/') {
        out_file.path.clone()
    } else {
        format!("{}/{}", work_dir, out_file.path)
    };

    if out_local && !exec_local {
        // Fetch the produced file from the remote exec host to the local target.
        let local_target = if out_file.path.starts_with('/') {
            out_file.path.clone()
        } else if let Ok(cwd) = std::env::current_dir() {
            cwd.join(&out_file.path).to_string_lossy().to_string()
        } else {
            out_file.path.clone()
        };
        ssh_fetch_file(exec_host, &exec_path, &local_target, true).map_err(|e| {
            format!(
                "{}: {}",
                gettext("failed to fetch output file from exec host"),
                e
            )
        })
    } else if !out_local && exec_local {
        // Send the locally-produced file to the remote output system.
        ssh_send_file(&out_file.system, &exec_path, &out_file.path, true)
            .map_err(|e| format!("{}: {}", gettext("failed to send output file to remote"), e))
    } else {
        // Output on a third remote system (neither local nor the exec host).
        Err(format!(
            "{}: {}",
            gettext("cross-system output to a third system is not supported"),
            out_file.system
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_local_command() {
        let result = parse_command_string("echo hello").unwrap();
        assert_eq!(result.exec_host, "");
        assert_eq!(result.command, "echo hello");
        assert!(result.input_files.is_empty());
        assert!(result.output_file.is_none());
    }

    #[test]
    fn test_parse_remote_command() {
        let result = parse_command_string("remotehost!cat file.txt").unwrap();
        assert_eq!(result.exec_host, "remotehost");
        assert_eq!(result.command, "cat file.txt");
        assert!(result.input_files.is_empty());
        assert!(result.output_file.is_none());
    }

    #[test]
    fn test_parse_command_with_output_redirect_separate() {
        let result = parse_command_string("host!cmd > output.txt").unwrap();
        assert_eq!(result.exec_host, "host");
        assert_eq!(result.command, "cmd > output.txt");
        assert!(result.output_file.is_some());
        let out = result.output_file.unwrap();
        assert_eq!(out.system, "host"); // defaults to exec host
        assert_eq!(out.path, "output.txt");
    }

    #[test]
    fn test_parse_command_with_output_redirect_combined() {
        let result = parse_command_string("host!cmd >output.txt").unwrap();
        assert_eq!(result.exec_host, "host");
        assert_eq!(result.command, "cmd >output.txt");
        assert!(result.output_file.is_some());
        let out = result.output_file.unwrap();
        assert_eq!(out.system, "host");
        assert_eq!(out.path, "output.txt");
    }

    #[test]
    fn test_parse_command_with_remote_output() {
        let result = parse_command_string("host!cmd > otherhost!output.txt").unwrap();
        assert_eq!(result.exec_host, "host");
        assert!(result.output_file.is_some());
        let out = result.output_file.unwrap();
        assert_eq!(out.system, "otherhost");
        assert_eq!(out.path, "output.txt");
    }

    #[test]
    fn test_parse_output_null_system_is_local() {
        // `>!path` has an explicit null system-name, which POSIX defines as the
        // local system — NOT the (remote) execution host.
        let sep = parse_command_string("host!cmd > !output.txt").unwrap();
        assert_eq!(sep.output_file.unwrap().system, "", "separate `> !file`");

        let combined = parse_command_string("host!cmd >!output.txt").unwrap();
        assert_eq!(
            combined.output_file.unwrap().system,
            "",
            "combined `>!file`"
        );

        // A bare target (no `!`) still defaults to the execution host.
        let bare = parse_command_string("host!cmd > output.txt").unwrap();
        assert_eq!(bare.output_file.unwrap().system, "host");
    }

    #[test]
    fn test_deliver_output_same_host_is_noop() {
        // Output target on the same (remote) host as execution: nothing to move.
        let out = FileRef {
            system: "exechost".to_string(),
            path: "/p/out".to_string(),
        };
        assert!(deliver_output(&out, "exechost", "/tmp/wd").is_ok());
    }

    #[test]
    fn test_deliver_output_third_system_is_error() {
        // Output on a system that is neither local nor the execution host is not
        // supported in the minimal SSH design — a hard error, not a silent skip.
        let out = FileRef {
            system: "thirdsys".to_string(),
            path: "/p/out".to_string(),
        };
        let err = deliver_output(&out, "exechost", "/tmp/wd").unwrap_err();
        assert!(
            err.contains("third system"),
            "expected third-system error, got: {err}"
        );
    }

    #[test]
    fn test_parse_command_with_input_file_from_other_system() {
        let result = parse_command_string("exechost!cat otherhost!/path/to/file.txt").unwrap();
        assert_eq!(result.exec_host, "exechost");
        // Input file should be extracted, basename used in command
        assert_eq!(result.command, "cat file.txt");
        assert_eq!(result.input_files.len(), 1);
        assert_eq!(result.input_files[0].system, "otherhost");
        assert_eq!(result.input_files[0].path, "/path/to/file.txt");
    }

    #[test]
    fn test_parse_command_with_multiple_input_files() {
        let result = parse_command_string("exechost!diff remote1!/a.txt remote2!/b.txt").unwrap();
        assert_eq!(result.exec_host, "exechost");
        assert_eq!(result.command, "diff a.txt b.txt");
        assert_eq!(result.input_files.len(), 2);
        assert_eq!(result.input_files[0].system, "remote1");
        assert_eq!(result.input_files[0].path, "/a.txt");
        assert_eq!(result.input_files[1].system, "remote2");
        assert_eq!(result.input_files[1].path, "/b.txt");
    }

    #[test]
    fn test_parse_command_with_tilde_expansion() {
        let result = parse_command_string("host!cat > ~/output.txt").unwrap();
        assert!(result.output_file.is_some());
        let out = result.output_file.unwrap();
        // ~/output.txt should expand to PUBDIR/output.txt
        assert!(out.path.contains("uucppublic"));
        assert!(out.path.ends_with("/output.txt"));
    }

    #[test]
    fn test_parse_command_input_file_with_tilde() {
        let result = parse_command_string("exechost!cat otherhost!~/data.txt").unwrap();
        assert_eq!(result.input_files.len(), 1);
        assert!(result.input_files[0].path.contains("uucppublic"));
        assert!(result.input_files[0].path.ends_with("/data.txt"));
    }

    #[test]
    fn test_parse_empty_command() {
        let result = parse_command_string("");
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), "empty command");
    }

    #[test]
    fn test_parse_whitespace_only_command() {
        let result = parse_command_string("   ");
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), "empty command");
    }

    #[test]
    fn test_parse_local_command_with_bang_in_argument() {
        // A ! at the start of a token should not be treated as a system reference
        let result = parse_command_string("echo !important").unwrap();
        assert_eq!(result.exec_host, "");
        assert_eq!(result.command, "echo !important");
        assert!(result.input_files.is_empty());
    }

    #[test]
    fn test_parse_command_complex_with_redirect_and_input() {
        let result =
            parse_command_string("exechost!process remote!/input.dat > local!/output.dat").unwrap();
        assert_eq!(result.exec_host, "exechost");
        // input.dat basename should be in command
        assert!(result.command.contains("input.dat"));
        // Should have one input file
        assert_eq!(result.input_files.len(), 1);
        assert_eq!(result.input_files[0].system, "remote");
        // Should have output file
        assert!(result.output_file.is_some());
        let out = result.output_file.unwrap();
        assert_eq!(out.system, "local");
    }

    #[test]
    fn test_parse_command_with_multiple_args() {
        let result = parse_command_string("host!grep -r pattern dir/").unwrap();
        assert_eq!(result.exec_host, "host");
        assert_eq!(result.command, "grep -r pattern dir/");
    }

    #[test]
    fn test_parse_command_preserves_flags() {
        let result = parse_command_string("host!ls -la /tmp").unwrap();
        assert_eq!(result.exec_host, "host");
        assert_eq!(result.command, "ls -la /tmp");
    }
}
