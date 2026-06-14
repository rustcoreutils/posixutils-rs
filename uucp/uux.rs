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
            // Output redirection: the next token is the target file. The
            // redirect is NOT passed through to the shell here — `execute_uux`
            // appends it, pointing at the exec host's work dir for cross-system
            // delivery so the output is never written to the target path on the
            // wrong host.
            in_redirect = true;
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
            in_redirect = false;
            i += 1;
            continue;
        }

        // Combined redirect like `>file` / `>sys!file`.
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

    // Resolve the output redirect (if any) up front: where the command should
    // write on the exec host, and whether the produced file must afterwards be
    // delivered to another system. An unsupported third-system target fails here
    // before any work is done.
    let (redirect_target, delivery) = resolve_output(
        &parsed.output_file,
        &parsed.exec_host,
        exec_local,
        &work_dir,
    )?;

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

    // Execute the command, appending the resolved output redirect (if any). The
    // redirect target is shell-quoted; for cross-system output it points at a
    // temp under the work dir, never at the target path on the exec host.
    let work_dir_escaped = shell_escape(&work_dir);
    let mut command = parsed.command.clone();
    if let Some(ref target) = redirect_target {
        command.push_str(&format!(" > {}", shell_escape(target)));
    }
    let full_cmd = format!("cd {} && {}", work_dir_escaped, command);

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

    // On success, deliver the produced output file to its target system (for a
    // cross-system redirect it was written to a temp under the exec host's work
    // dir). Done before cleanup removes the work dir.
    let delivery_result = if code == 0 {
        match delivery {
            Some(ref d) => deliver(d, &parsed.exec_host),
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
    delivery_result?;

    // The command's standard output is discarded per POSIX unless redirected.
    let _ = stdout;

    Ok(())
}

/// A pending delivery of a `uux` output file from the execution host's work-dir
/// temp to the target system after a successful run.
#[derive(Debug)]
struct Delivery {
    /// True to fetch the temp from a remote exec host down to the local target;
    /// false to send a locally-produced temp up to the remote target system.
    fetch: bool,
    /// Path of the produced file on the exec host (a temp under the work dir).
    tmp: String,
    /// Target system name (empty for local).
    system: String,
    /// Target path on the target system.
    path: String,
}

/// Decide where a `uux` command's `>` redirect should write on the execution
/// host, and whether the produced file must afterwards be delivered to another
/// system. Returns `(redirect_target_on_exec_host, optional delivery)`:
/// - no output redirect → `(None, None)`;
/// - output on the same host as execution → write directly to the requested
///   path there, no delivery;
/// - cross-system (local↔remote) → write to a temp under the exec host's work
///   dir, then deliver it to the target (so the target path is interpreted only
///   on the target system, and nothing is left on the exec host);
/// - output on a third remote system (neither local nor the exec host) → error.
fn resolve_output(
    output_file: &Option<FileRef>,
    exec_host: &str,
    exec_local: bool,
    work_dir: &str,
) -> Result<(Option<String>, Option<Delivery>), String> {
    let out = match output_file {
        Some(o) => o,
        None => return Ok((None, None)),
    };

    let out_local = is_local_system(&out.system);
    let same_host = (out_local && exec_local) || (!out_local && out.system == exec_host);

    if same_host {
        // Write directly to the requested path on the exec host.
        Ok((Some(out.path.clone()), None))
    } else if !out_local && !exec_local {
        // Output on a third remote system: unsupported in the minimal design.
        Err(format!(
            "{}: {}",
            gettext("cross-system output to a third system is not supported"),
            out.system
        ))
    } else {
        // Cross-system: redirect to a temp in the work dir, deliver afterwards.
        let tmp = format!("{}/uux_output_file", work_dir);
        let path = if out_local && !out.path.starts_with('/') {
            // Local target with a relative path: resolve against the cwd.
            match std::env::current_dir() {
                Ok(cwd) => cwd.join(&out.path).to_string_lossy().to_string(),
                Err(_) => out.path.clone(),
            }
        } else {
            out.path.clone()
        };
        Ok((
            Some(tmp.clone()),
            Some(Delivery {
                fetch: out_local,
                tmp,
                system: out.system.clone(),
                path,
            }),
        ))
    }
}

/// Carry out a pending [`Delivery`] (the produced file is in `d.tmp` on the
/// exec host).
fn deliver(d: &Delivery, exec_host: &str) -> Result<(), String> {
    if d.fetch {
        // Remote exec host → local target.
        ssh_fetch_file(exec_host, &d.tmp, &d.path, true).map_err(|e| {
            format!(
                "{}: {}",
                gettext("failed to fetch output file from exec host"),
                e
            )
        })
    } else {
        // Local exec → remote target system.
        ssh_send_file(&d.system, &d.tmp, &d.path, true)
            .map_err(|e| format!("{}: {}", gettext("failed to send output file to remote"), e))
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
        // The redirect is recorded in output_file, not left in the command
        // string (execute_uux re-appends it with the right target).
        assert_eq!(result.command, "cmd");
        assert!(result.output_file.is_some());
        let out = result.output_file.unwrap();
        assert_eq!(out.system, "host"); // defaults to exec host
        assert_eq!(out.path, "output.txt");
    }

    #[test]
    fn test_parse_command_with_output_redirect_combined() {
        let result = parse_command_string("host!cmd >output.txt").unwrap();
        assert_eq!(result.exec_host, "host");
        assert_eq!(result.command, "cmd");
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
    fn test_resolve_output_same_host_writes_in_place() {
        // Output target on the same (remote) host as execution: the command
        // redirects straight to the requested path; no delivery is scheduled.
        let out = Some(FileRef {
            system: "exechost".to_string(),
            path: "/p/out".to_string(),
        });
        let (redirect, delivery) = resolve_output(&out, "exechost", false, "/tmp/wd").unwrap();
        assert_eq!(redirect.as_deref(), Some("/p/out"));
        assert!(delivery.is_none());
    }

    #[test]
    fn test_resolve_output_cross_system_uses_workdir_temp() {
        // Output local, execution remote: redirect to a temp under the work dir
        // (never the target path on the exec host), then fetch it back.
        let out = Some(FileRef {
            system: String::new(), // local
            path: "/p/out".to_string(),
        });
        let (redirect, delivery) = resolve_output(&out, "exechost", false, "/tmp/wd").unwrap();
        assert_eq!(redirect.as_deref(), Some("/tmp/wd/uux_output_file"));
        let d = delivery.expect("delivery scheduled");
        assert!(d.fetch, "exec-remote/output-local fetches");
        assert_eq!(d.tmp, "/tmp/wd/uux_output_file");
        assert_eq!(d.path, "/p/out");
    }

    #[test]
    fn test_resolve_output_third_system_is_error() {
        // Output on a system that is neither local nor the execution host is not
        // supported in the minimal SSH design — a hard error, not a silent skip.
        let out = Some(FileRef {
            system: "thirdsys".to_string(),
            path: "/p/out".to_string(),
        });
        let err = resolve_output(&out, "exechost", false, "/tmp/wd").unwrap_err();
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
