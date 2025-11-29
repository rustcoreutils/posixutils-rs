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

    /// Expand special characters in command string per POSIX.
    ///
    /// - `%` expands to current filename
    /// - `#` expands to alternate filename
    /// - `!` expands to previous command
    /// - Backslash escapes these special characters
    pub fn expand_command(&self, command: &str) -> Result<String> {
        let mut result = String::with_capacity(command.len());
        let mut chars = command.chars().peekable();

        while let Some(c) = chars.next() {
            match c {
                '\\' => {
                    // Check for escaped special chars
                    if let Some(&next @ ('%' | '#' | '!')) = chars.peek() {
                        result.push(next);
                        chars.next();
                    } else {
                        result.push('\\');
                    }
                }
                '%' => {
                    // Expand to current filename
                    let file = self.current_file.as_ref().ok_or(ViError::NoFileName)?;
                    result.push_str(file);
                }
                '#' => {
                    // Expand to alternate filename
                    let file = self
                        .alternate_file
                        .as_ref()
                        .ok_or(ViError::NoAlternateFile)?;
                    result.push_str(file);
                }
                '!' => {
                    // Expand to previous command
                    let cmd = self
                        .last_command
                        .as_ref()
                        .ok_or(ViError::NoPreviousCommand)?;
                    result.push_str(cmd);
                }
                _ => {
                    result.push(c);
                }
            }
        }

        Ok(result)
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
        let expanded = self.expand_command(command)?;
        Self::validate_command(&expanded)?;

        // Save as last command
        self.last_command = Some(expanded.clone());

        let output = Command::new(&self.shell)
            .arg("-c")
            .arg(&expanded)
            .stdin(Stdio::inherit())
            .stdout(Stdio::inherit())
            .stderr(Stdio::inherit())
            .status()
            .map_err(|e| ViError::ShellError(format!("failed to execute command: {}", e)))?;

        Ok(ShellOutput {
            success: output.success(),
            exit_code: output.code().unwrap_or(-1),
            stdout: Vec::new(), // Output went directly to terminal
            stderr: Vec::new(),
        })
    }

    /// Execute a shell command and capture its output.
    ///
    /// This is used for `:r !command` - read output into buffer.
    pub fn execute_capture(&mut self, command: &str) -> Result<ShellOutput> {
        let expanded = self.expand_command(command)?;
        Self::validate_command(&expanded)?;

        // Save as last command
        self.last_command = Some(expanded.clone());

        let output = Command::new(&self.shell)
            .arg("-c")
            .arg(&expanded)
            .stdin(Stdio::null())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .output()
            .map_err(|e| ViError::ShellError(format!("failed to execute command: {}", e)))?;

        Ok(ShellOutput {
            success: output.status.success(),
            exit_code: output.status.code().unwrap_or(-1),
            stdout: output.stdout,
            stderr: output.stderr,
        })
    }

    /// Execute a command with input and capture output (filter).
    ///
    /// This is used for `:[range]!command` - filter lines through command.
    pub fn filter(&mut self, command: &str, input: &str) -> Result<ShellOutput> {
        let expanded = self.expand_command(command)?;
        Self::validate_command(&expanded)?;

        // Save as last command
        self.last_command = Some(expanded.clone());

        let mut child = Command::new(&self.shell)
            .arg("-c")
            .arg(&expanded)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .map_err(|e| ViError::ShellError(format!("failed to spawn command: {}", e)))?;

        // Write input to stdin
        if let Some(mut stdin) = child.stdin.take() {
            stdin
                .write_all(input.as_bytes())
                .map_err(|e| ViError::ShellError(format!("failed to write to command: {}", e)))?;
        }

        // Wait for completion and get output
        let output = child
            .wait_with_output()
            .map_err(|e| ViError::ShellError(format!("failed to read command output: {}", e)))?;

        Ok(ShellOutput {
            success: output.status.success(),
            exit_code: output.status.code().unwrap_or(-1),
            stdout: output.stdout,
            stderr: output.stderr,
        })
    }

    /// Execute a command with input, discarding output.
    ///
    /// This is used for `:w !command` - write buffer to command.
    pub fn write_to(&mut self, command: &str, input: &str) -> Result<ShellOutput> {
        let expanded = self.expand_command(command)?;
        Self::validate_command(&expanded)?;

        // Save as last command
        self.last_command = Some(expanded.clone());

        let mut child = Command::new(&self.shell)
            .arg("-c")
            .arg(&expanded)
            .stdin(Stdio::piped())
            .stdout(Stdio::inherit())
            .stderr(Stdio::inherit())
            .spawn()
            .map_err(|e| ViError::ShellError(format!("failed to spawn command: {}", e)))?;

        // Write input to stdin
        if let Some(mut stdin) = child.stdin.take() {
            stdin
                .write_all(input.as_bytes())
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
            stderr: Vec::new(),
        })
    }

    /// Start an interactive shell.
    ///
    /// This is used for `:shell` - spawn interactive shell.
    pub fn interactive(&mut self) -> Result<ShellOutput> {
        let status = Command::new(&self.shell)
            .stdin(Stdio::inherit())
            .stdout(Stdio::inherit())
            .stderr(Stdio::inherit())
            .status()
            .map_err(|e| ViError::ShellError(format!("failed to start shell: {}", e)))?;

        Ok(ShellOutput {
            success: status.success(),
            exit_code: status.code().unwrap_or(-1),
            stdout: Vec::new(),
            stderr: Vec::new(),
        })
    }
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
    /// Captured stderr (empty if not captured).
    pub stderr: Vec<u8>,
}

impl ShellOutput {
    /// Get stdout as a string, lossy conversion.
    pub fn stdout_string(&self) -> String {
        String::from_utf8_lossy(&self.stdout).into_owned()
    }

    /// Get stderr as a string, lossy conversion.
    pub fn stderr_string(&self) -> String {
        String::from_utf8_lossy(&self.stderr).into_owned()
    }

    /// Get stdout as lines.
    pub fn stdout_lines(&self) -> Vec<String> {
        let s = self.stdout_string();
        s.lines().map(|l| l.to_string()).collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_expand_command_no_special() {
        let exec = ShellExecutor::new("/bin/sh");
        let result = exec.expand_command("echo hello").unwrap();
        assert_eq!(result, "echo hello");
    }

    #[test]
    fn test_expand_command_percent() {
        let mut exec = ShellExecutor::new("/bin/sh");
        exec.set_current_file(Some("test.txt".to_string()));
        let result = exec.expand_command("cat %").unwrap();
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
        let result = exec.expand_command("echo \\%").unwrap();
        assert_eq!(result, "echo %");
    }

    #[test]
    fn test_expand_command_bang() {
        let mut exec = ShellExecutor::new("/bin/sh");
        exec.last_command = Some("ls -la".to_string());
        let result = exec.expand_command("!").unwrap();
        assert_eq!(result, "ls -la");
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
