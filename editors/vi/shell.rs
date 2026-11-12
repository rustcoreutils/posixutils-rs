//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Shell command execution for vi.
//!
//! This module provides secure shell command execution for:
//! - `:!command` - Execute and display output
//! - `:[range]!command` - Filter lines through command
//! - `:r !command` - Read command output into buffer
//! - `:w !command` - Write buffer to command stdin
//!
//! Per POSIX, commands are executed via the shell specified in the
//! `shell` option, invoked with `-c` and the command as arguments.

use crate::error::{Result, ViError};
use std::io::Write;
use std::process::{Command, Stdio};

/// Maximum allowed command length to prevent DoS.
const MAX_COMMAND_LEN: usize = 8192;

/// Shell executor for vi commands.
pub struct ShellExecutor {
    /// Shell program to use.
    shell: String,
    /// Current filename (for % expansion).
    current_file: Option<String>,
    /// Alternate filename (for # expansion).
    alternate_file: Option<String>,
    /// Last shell command (for ! expansion).
    last_command: Option<String>,
}

impl ShellExecutor {
    /// Create a new shell executor.
    pub fn new(shell: &str) -> Self {
        Self {
            shell: shell.to_string(),
            current_file: None,
            alternate_file: None,
            last_command: None,
        }
    }

    /// Use `shell` as the program to run (the `shell` edit option, read
    /// when a command runs, so `:set shell=` takes effect).  Empty means
    /// `sh` (95905-95909).
    pub fn set_program(&mut self, shell: &str) {
        shell.clone_into(&mut self.shell);
        if self.shell.is_empty() {
            self.shell = "sh".to_string();
        }
    }

    /// Set the current filename for % expansion.
    pub fn set_current_file(&mut self, file: Option<String>) {
        self.current_file = file;
    }

    /// Set the alternate filename for # expansion.
    pub fn set_alternate_file(&mut self, file: Option<String>) {
        self.alternate_file = file;
    }

    /// Get the last executed command.
    pub fn last_command(&self) -> Option<&str> {
        self.last_command.as_deref()
    }

    /// `command` with `%`, `#` and `!` expanded (see [`expand`]), and
    /// whether anything was substituted.
    pub fn expand_command(&self, command: &str) -> Result<(String, bool)> {
        expand(
            command,
            self.current_file.as_deref(),
            self.alternate_file.as_deref(),
            Some(self.last_command.as_deref()),
        )
    }

    /// Validate command before execution.
    fn validate_command(command: &str) -> Result<()> {
        if command.is_empty() {
            return Err(ViError::InvalidCommand("empty command".to_string()));
        }
        if command.len() > MAX_COMMAND_LEN {
            return Err(ViError::InvalidCommand(format!(
                "command too long (max {} characters)",
                MAX_COMMAND_LEN
            )));
        }
        Ok(())
    }

    /// Execute a shell command and return its output.
    ///
    /// This is used for `:!command` - execute and display output.
    pub fn execute(&mut self, command: &str) -> Result<ShellOutput> {
        let (expanded, _) = self.expand_command(command)?;
        Self::validate_command(&expanded)?;

        // Save as last command
        self.last_command = Some(expanded.clone());

        let output = Command::new(&self.shell)
            .arg("-c")
            .arg(crate::bytes::to_os(&expanded))
            .stdin(Stdio::inherit())
            .stdout(Stdio::inherit())
            .stderr(Stdio::inherit())
            .status()
            .map_err(|e| ViError::ShellError(format!("failed to execute command: {}", e)))?;

        Ok(ShellOutput {
            success: output.success(),
            exit_code: output.code().unwrap_or(-1),
            stdout: Vec::new(), // Output went directly to terminal
        })
    }

    /// Execute a shell command and capture its output.
    ///
    /// This is used for `:r !command` - read output into buffer.
    pub fn execute_capture(&mut self, command: &str) -> Result<ShellOutput> {
        let (expanded, _) = self.expand_command(command)?;
        Self::validate_command(&expanded)?;

        // Save as last command
        self.last_command = Some(expanded.clone());

        // #X23: POSIX (ex.md §95278-95280) says the program's standard input
        // "shall be set to the standard input of the ex program when it was
        // invoked", so inherit it rather than handing the child /dev/null --
        // that is what makes `:r !cat` and `:r !sed ...` work at all.
        // 95279-95280: its standard output *and standard error* go into
        // the buffer -- one pipe for both, so they keep their order.
        let (mut reader, writer) = std::io::pipe()
            .map_err(|e| ViError::ShellError(format!("failed to make a pipe: {}", e)))?;
        let err_writer = writer
            .try_clone()
            .map_err(|e| ViError::ShellError(format!("failed to make a pipe: {}", e)))?;
        let mut command = Command::new(&self.shell);
        command
            .arg("-c")
            .arg(crate::bytes::to_os(&expanded))
            .stdin(Stdio::inherit())
            .stdout(writer)
            .stderr(err_writer);
        let mut child = command
            .spawn()
            .map_err(|e| ViError::ShellError(format!("failed to execute command: {}", e)))?;
        // The parent's copies of the write end go with `command`, so the
        // read sees end of file when the child is done.
        drop(command);
        // Read it all, then reap the child -- reaped even if the read fails.
        let mut stdout = Vec::new();
        let read = std::io::Read::read_to_end(&mut reader, &mut stdout);
        let status = child
            .wait()
            .map_err(|e| ViError::ShellError(format!("failed to wait for command: {}", e)))?;
        read.map_err(|e| ViError::ShellError(format!("failed to read command output: {}", e)))?;

        Ok(ShellOutput {
            success: status.success(),
            exit_code: status.code().unwrap_or(-1),
            stdout: fold_crlf(stdout),
        })
    }

    /// Execute a command with input and capture output (filter).
    ///
    /// This is used for `:[range]!command` - filter lines through command.
    pub fn filter(&mut self, command: &str, input: &str) -> Result<ShellOutput> {
        let (expanded, _) = self.expand_command(command)?;
        Self::validate_command(&expanded)?;

        // Save as last command
        self.last_command = Some(expanded.clone());

        // 95609-95610: standard output and standard error replace the
        // lines -- one pipe for both, so they keep their order.
        let (mut reader, writer) = std::io::pipe()
            .map_err(|e| ViError::ShellError(format!("failed to make a pipe: {}", e)))?;
        let err_writer = writer
            .try_clone()
            .map_err(|e| ViError::ShellError(format!("failed to make a pipe: {}", e)))?;
        let mut command = Command::new(&self.shell);
        command
            .arg("-c")
            .arg(crate::bytes::to_os(&expanded))
            .stdin(Stdio::piped())
            .stdout(writer)
            .stderr(err_writer);
        let mut child = command
            .spawn()
            .map_err(|e| ViError::ShellError(format!("failed to spawn command: {}", e)))?;
        drop(command);

        // Feed stdin from a separate thread.  `wait_with_output` cannot start
        // draining the child's stdout until the write finishes, so writing
        // inline deadlocks as soon as the child's output fills the pipe
        // buffer: each side waits for the other, and `:1,$!cat` on anything
        // past ~64 KB hung the editor outright.
        // Held until after the join below: a command that exits early (`!head`)
        // closes this pipe, and the feeder thread must see EPIPE rather than
        // the process taking the signal and losing the buffer.
        let _sigpipe = plib::io::SigPipeIgnored::new();

        let feeder = child.stdin.take().map(|mut stdin| {
            // The bytes the buffer stands for, escaped bytes as themselves.
            let data = crate::bytes::encode(input);
            std::thread::spawn(move || stdin.write_all(&data))
        });

        // Read it all, then reap the child.
        let mut stdout = Vec::new();
        let read = std::io::Read::read_to_end(&mut reader, &mut stdout);
        let status = child
            .wait()
            .map_err(|e| ViError::ShellError(format!("failed to wait for command: {}", e)))?;
        read.map_err(|e| ViError::ShellError(format!("failed to read command output: {}", e)))?;

        if let Some(feeder) = feeder {
            match feeder.join() {
                // A command that exits without reading all its input (`head`)
                // gives us EPIPE; that is the command's choice, not an error.
                Ok(Err(e)) if e.kind() != std::io::ErrorKind::BrokenPipe => {
                    return Err(ViError::ShellError(format!(
                        "failed to write to command: {}",
                        e
                    )));
                }
                Err(_) => {
                    return Err(ViError::ShellError(
                        "failed to write to command".to_string(),
                    ));
                }
                Ok(_) => {}
            }
        }

        Ok(ShellOutput {
            success: status.success(),
            exit_code: status.code().unwrap_or(-1),
            stdout: fold_crlf(stdout),
        })
    }

    /// Execute a command with input, discarding output.
    ///
    /// This is used for `:w !command` - write buffer to command.
    pub fn write_to(&mut self, command: &str, input: &str) -> Result<ShellOutput> {
        let (expanded, _) = self.expand_command(command)?;
        Self::validate_command(&expanded)?;

        // Save as last command
        self.last_command = Some(expanded.clone());

        let mut child = Command::new(&self.shell)
            .arg("-c")
            .arg(crate::bytes::to_os(&expanded))
            .stdin(Stdio::piped())
            .stdout(Stdio::inherit())
            .stderr(Stdio::inherit())
            .spawn()
            .map_err(|e| ViError::ShellError(format!("failed to spawn command: {}", e)))?;

        // Write input to stdin
        if let Some(mut stdin) = child.stdin.take() {
            stdin
                .write_all(&crate::bytes::encode(input))
                .map_err(|e| ViError::ShellError(format!("failed to write to command: {}", e)))?;
        }

        // Wait for completion
        let status = child
            .wait()
            .map_err(|e| ViError::ShellError(format!("failed to wait for command: {}", e)))?;

        Ok(ShellOutput {
            success: status.success(),
            exit_code: status.code().unwrap_or(-1),
            stdout: Vec::new(),
        })
    }

    /// Start an interactive shell.
    ///
    /// This is used for `:shell` - spawn interactive shell.
    pub fn interactive(&mut self) -> Result<ShellOutput> {
        // An interactive shell needs a terminal to be interactive *with*.
        // Started without one it inherits the editor's piped stdin, competes
        // for the controlling terminal and stops -- which reads as the editor
        // hanging, with no diagnostic and no way back.  Refusing is the only
        // useful answer.
        use std::io::IsTerminal;
        if !std::io::stdin().is_terminal() {
            return Err(ViError::ShellError(
                "cannot start an interactive shell without a terminal".to_string(),
            ));
        }

        // POSIX: `:shell` invokes the shell as an interactive shell (-i).
        let status = Command::new(&self.shell)
            .arg("-i")
            .stdin(Stdio::inherit())
            .stdout(Stdio::inherit())
            .stderr(Stdio::inherit())
            .status()
            .map_err(|e| ViError::ShellError(format!("failed to start shell: {}", e)))?;

        Ok(ShellOutput {
            success: status.success(),
            exit_code: status.code().unwrap_or(-1),
            stdout: Vec::new(),
        })
    }
}

/// Expand `text` per 94657-94672: unescaped `%` is the `current`
/// pathname and `#` the `alternate` one -- an error if that is not set --
/// and, when `last` is given (a shell command), `!` is the previous shell
/// command.  `\` before one of them makes it the character, and in a file
/// name (no `last`) so does `\` before `\`.
/// Returns the text and whether anything was substituted.
pub fn expand(
    text: &str,
    current: Option<&str>,
    alternate: Option<&str>,
    last: Option<Option<&str>>,
) -> Result<(String, bool)> {
    let mut out = String::with_capacity(text.len());
    let mut changed = false;
    let mut chars = text.chars().peekable();
    while let Some(c) = chars.next() {
        match c {
            '\\' => match chars.peek() {
                Some(&next @ ('%' | '#')) => {
                    out.push(next);
                    chars.next();
                }
                // In a file name `\\` is a backslash; a shell command keeps
                // it, for the shell to read.
                Some('\\') if last.is_none() => {
                    out.push('\\');
                    chars.next();
                }
                Some('!') if last.is_some() => {
                    out.push('!');
                    chars.next();
                }
                _ => out.push('\\'),
            },
            '%' => {
                out.push_str(current.ok_or(ViError::NoFileName)?);
                changed = true;
            }
            '#' => {
                out.push_str(alternate.ok_or(ViError::NoAlternateFile)?);
                changed = true;
            }
            '!' if last.is_some() => {
                let previous = last.flatten().ok_or(ViError::NoPreviousCommand)?;
                out.push_str(previous);
                changed = true;
            }
            _ => out.push(c),
        }
    }
    Ok((out, changed))
}

/// A program's output with each <carriage-return><newline> pair made one
/// <newline> (95283-95284, 95612-95614); a lone <carriage-return> stays.
fn fold_crlf(bytes: Vec<u8>) -> Vec<u8> {
    if !bytes.windows(2).any(|w| w == b"\r\n") {
        return bytes;
    }
    let mut out = Vec::with_capacity(bytes.len());
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'\r' && bytes.get(i + 1) == Some(&b'\n') {
            i += 1;
            continue;
        }
        out.push(bytes[i]);
        i += 1;
    }
    out
}

/// Output from a shell command.
#[derive(Debug)]
pub struct ShellOutput {
    /// Whether the command succeeded (exit code 0).
    pub success: bool,
    /// Exit code of the command.
    pub exit_code: i32,
    /// Captured stdout (empty if not captured).
    pub stdout: Vec<u8>,
}

impl ShellOutput {
    /// Get stdout as editor text.
    ///
    /// Bytes that are not UTF-8 are escaped, not replaced, so the output can
    /// go into the buffer and back out unchanged.
    pub fn stdout_string(&self) -> String {
        crate::bytes::decode(&self.stdout)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_expand_command_no_special() {
        let exec = ShellExecutor::new("/bin/sh");
        let result = exec.expand_command("echo hello").unwrap().0;
        assert_eq!(result, "echo hello");
    }

    #[test]
    fn test_expand_command_percent() {
        let mut exec = ShellExecutor::new("/bin/sh");
        exec.set_current_file(Some("test.txt".to_string()));
        let result = exec.expand_command("cat %").unwrap().0;
        assert_eq!(result, "cat test.txt");
    }

    #[test]
    fn test_expand_command_hash() {
        let mut exec = ShellExecutor::new("/bin/sh");
        exec.set_alternate_file(Some("other.txt".to_string()));
        let result = exec.expand_command("diff # %").unwrap_err();
        // Should fail because current file not set
        assert!(matches!(result, ViError::NoFileName));
    }

    #[test]
    fn test_expand_command_escaped() {
        let mut exec = ShellExecutor::new("/bin/sh");
        exec.set_current_file(Some("test.txt".to_string()));
        let result = exec.expand_command("echo \\%").unwrap().0;
        assert_eq!(result, "echo %");
    }

    #[test]
    fn test_expand_command_bang() {
        let mut exec = ShellExecutor::new("/bin/sh");
        exec.last_command = Some("ls -la".to_string());
        let result = exec.expand_command("!").unwrap().0;
        assert_eq!(result, "ls -la");
    }

    #[test]
    fn test_expand_backslashes_in_file_names() {
        // In a file name `\\%` is a backslash and then the name.
        assert_eq!(
            expand("d\\\\%", Some("f"), None, None).unwrap(),
            ("d\\f".to_string(), true)
        );
        assert_eq!(
            expand("d\\%", Some("f"), None, None).unwrap(),
            ("d%".to_string(), false)
        );
        // A shell command keeps `\\` for the shell.
        assert_eq!(
            expand("d\\\\x", None, None, Some(None)).unwrap().0,
            "d\\\\x"
        );
    }

    #[test]
    fn test_validate_empty_command() {
        let result = ShellExecutor::validate_command("");
        assert!(result.is_err());
    }

    #[test]
    fn test_validate_long_command() {
        let long_cmd = "a".repeat(MAX_COMMAND_LEN + 1);
        let result = ShellExecutor::validate_command(&long_cmd);
        assert!(result.is_err());
    }

    #[test]
    fn test_execute_capture_echo() {
        let mut exec = ShellExecutor::new("/bin/sh");
        let output = exec.execute_capture("echo hello").unwrap();
        assert!(output.success);
        assert_eq!(output.stdout_string().trim(), "hello");
    }

    #[test]
    fn test_filter_sort() {
        let mut exec = ShellExecutor::new("/bin/sh");
        let output = exec.filter("sort", "banana\napple\ncherry\n").unwrap();
        assert!(output.success);
        assert_eq!(output.stdout_string(), "apple\nbanana\ncherry\n");
    }

    #[test]
    fn test_filter_tr_uppercase() {
        let mut exec = ShellExecutor::new("/bin/sh");
        let output = exec.filter("tr a-z A-Z", "hello world\n").unwrap();
        assert!(output.success);
        assert_eq!(output.stdout_string(), "HELLO WORLD\n");
    }
}
