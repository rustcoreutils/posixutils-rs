//! Common functionality shared between uucp, uux, and uustat utilities.

#![allow(dead_code)]

use std::env;
use std::fs::{self, File};
use std::io::{self, Read, Write};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::time::{SystemTime, UNIX_EPOCH};

/// Default public directory for UUCP
pub const PUBDIR: &str = "/var/spool/uucppublic";

/// Escape a string for safe use in shell single quotes.
/// This handles the case where the string contains single quotes by ending the
/// quoted string, adding an escaped single quote, and starting a new quoted string.
/// Example: "it's" becomes "'it'\''s'"
pub fn shell_escape(s: &str) -> String {
    let mut result = String::with_capacity(s.len() + 2);
    result.push('\'');
    for c in s.chars() {
        if c == '\'' {
            // End single quote, add escaped single quote, start new single quote
            result.push_str("'\\''");
        } else {
            result.push(c);
        }
    }
    result.push('\'');
    result
}

/// Get the spool directory path
pub fn spool_dir() -> PathBuf {
    if let Ok(dir) = env::var("UUCP_SPOOL") {
        return PathBuf::from(dir);
    }
    // Try system spool first, fall back to user-local
    let sys_spool = PathBuf::from("/var/spool/uucp");
    if sys_spool.exists()
        && !fs::metadata(&sys_spool)
            .map(|m| m.permissions().readonly())
            .unwrap_or(true)
    {
        return sys_spool;
    }
    // User-local fallback
    if let Ok(home) = env::var("HOME") {
        return PathBuf::from(home).join(".uucp").join("spool");
    }
    sys_spool
}

/// Generate a unique job ID
pub fn generate_job_id() -> String {
    let now = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default();
    let pid = std::process::id();
    format!("{}{:04x}", now.as_secs() % 100000, pid % 0x10000)
}

/// Parse a UUCP path specification (system!path or just path)
/// Returns (system, path) where system is empty for local
pub fn parse_path_spec(spec: &str) -> (String, String) {
    if let Some(pos) = spec.find('!') {
        let system = &spec[..pos];
        let path = &spec[pos + 1..];
        (system.to_string(), path.to_string())
    } else {
        (String::new(), spec.to_string())
    }
}

/// Expand tilde in path for remote system (convert ~/ to PUBDIR)
pub fn expand_remote_path(path: &str) -> String {
    if let Some(rest) = path.strip_prefix("~/") {
        format!("{}/{}", PUBDIR, rest)
    } else {
        path.to_string()
    }
}

/// Expand tilde in path for local system
pub fn expand_local_path(path: &str) -> String {
    if let Some(rest) = path.strip_prefix("~/") {
        // Public directory
        format!("{}/{}", PUBDIR, rest)
    } else if let Some(rest) = path.strip_prefix('~') {
        // ~user/path
        if let Some(slash_pos) = rest.find('/') {
            let user = &rest[..slash_pos];
            let file_rest = &rest[slash_pos..];
            if let Some(home) = get_user_home(user) {
                format!("{}{}", home, file_rest)
            } else {
                format!("{}{}", PUBDIR, file_rest)
            }
        } else {
            // Just ~user with no path
            get_user_home(rest).unwrap_or_else(|| PUBDIR.to_string())
        }
    } else if path.starts_with('/') {
        path.to_string()
    } else {
        // Relative path - prefix with current directory
        if let Ok(cwd) = env::current_dir() {
            cwd.join(path).to_string_lossy().to_string()
        } else {
            path.to_string()
        }
    }
}

/// Get home directory for a user
fn get_user_home(user: &str) -> Option<String> {
    if user.is_empty() {
        return env::var("HOME").ok();
    }
    // Try to get user's home from passwd
    let output = Command::new("getent")
        .args(["passwd", user])
        .output()
        .ok()?;
    if output.status.success() {
        let line = String::from_utf8_lossy(&output.stdout);
        let fields: Vec<&str> = line.split(':').collect();
        if fields.len() >= 6 {
            return Some(fields[5].to_string());
        }
    }
    None
}

/// Check if a system name refers to the local system
pub fn is_local_system(system: &str) -> bool {
    system.is_empty() || system == "localhost" || {
        if let Ok(hostname) = Command::new("hostname").output() {
            let local = String::from_utf8_lossy(&hostname.stdout).trim().to_string();
            system == local
        } else {
            false
        }
    }
}

/// Job information stored in spool
#[derive(Debug)]
pub struct Job {
    pub id: String,
    pub user: String,
    pub system: String,
    pub command: String, // "uucp" or "uux"
    pub request: String, // Description of the job
}

impl Job {
    /// Create a new job
    pub fn new(system: &str, command: &str, request: &str) -> Self {
        let user = env::var("USER").unwrap_or_else(|_| "unknown".to_string());
        Job {
            id: generate_job_id(),
            user,
            system: system.to_string(),
            command: command.to_string(),
            request: request.to_string(),
        }
    }

    /// Get the job file path
    pub fn file_path(&self) -> PathBuf {
        let spool = spool_dir();
        spool.join(&self.system).join(format!("J.{}", self.id))
    }

    /// Save job to spool
    pub fn save(&self) -> io::Result<()> {
        let path = self.file_path();
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent)?;
        }
        let mut file = File::create(&path)?;
        writeln!(file, "id={}", self.id)?;
        writeln!(file, "user={}", self.user)?;
        writeln!(file, "system={}", self.system)?;
        writeln!(file, "command={}", self.command)?;
        writeln!(file, "request={}", self.request)?;
        Ok(())
    }

    /// Load job from file
    pub fn load(path: &Path) -> io::Result<Self> {
        let mut content = String::new();
        File::open(path)?.read_to_string(&mut content)?;

        let mut id = String::new();
        let mut user = String::new();
        let mut system = String::new();
        let mut command = String::new();
        let mut request = String::new();

        for line in content.lines() {
            if let Some(val) = line.strip_prefix("id=") {
                id = val.to_string();
            } else if let Some(val) = line.strip_prefix("user=") {
                user = val.to_string();
            } else if let Some(val) = line.strip_prefix("system=") {
                system = val.to_string();
            } else if let Some(val) = line.strip_prefix("command=") {
                command = val.to_string();
            } else if let Some(val) = line.strip_prefix("request=") {
                request = val.to_string();
            }
        }

        Ok(Job {
            id,
            user,
            system,
            command,
            request,
        })
    }

    /// Delete job from spool
    pub fn delete(&self) -> io::Result<()> {
        let path = self.file_path();
        if path.exists() {
            fs::remove_file(&path)?;
        }
        // Also remove any data files
        if let Some(parent) = path.parent() {
            let data_pattern = format!("D.{}", self.id);
            if let Ok(entries) = fs::read_dir(parent) {
                for entry in entries.flatten() {
                    if entry
                        .file_name()
                        .to_string_lossy()
                        .starts_with(&data_pattern)
                    {
                        let _ = fs::remove_file(entry.path());
                    }
                }
            }
        }
        Ok(())
    }

    /// Touch job files to update modification time
    pub fn rejuvenate(&self) -> io::Result<()> {
        let path = self.file_path();
        if path.exists() {
            // Touch by opening and writing same content
            let content = fs::read_to_string(&path)?;
            fs::write(&path, content)?;
        }
        Ok(())
    }
}

/// List all jobs in the spool, optionally filtered
pub fn list_jobs(system_filter: Option<&str>, user_filter: Option<&str>) -> Vec<Job> {
    let mut jobs = Vec::new();
    let spool = spool_dir();

    if !spool.exists() {
        return jobs;
    }

    let Ok(entries) = fs::read_dir(&spool) else {
        return jobs;
    };

    for entry in entries.flatten() {
        let path = entry.path();
        if !path.is_dir() {
            continue;
        }

        let system_name = path
            .file_name()
            .unwrap_or_default()
            .to_string_lossy()
            .to_string();

        // Filter by system if specified
        if let Some(filter) = system_filter {
            if system_name != filter {
                continue;
            }
        }

        let Ok(job_entries) = fs::read_dir(&path) else {
            continue;
        };

        for job_entry in job_entries.flatten() {
            let job_path = job_entry.path();
            if !job_path
                .file_name()
                .unwrap_or_default()
                .to_string_lossy()
                .starts_with("J.")
            {
                continue;
            }

            if let Ok(job) = Job::load(&job_path) {
                // Filter by user if specified
                if let Some(filter) = user_filter {
                    if job.user != filter {
                        continue;
                    }
                }
                jobs.push(job);
            }
        }
    }

    jobs
}

/// Find a job by ID
pub fn find_job(job_id: &str) -> Option<Job> {
    let spool = spool_dir();
    if !spool.exists() {
        return None;
    }

    let Ok(entries) = fs::read_dir(&spool) else {
        return None;
    };

    for entry in entries.flatten() {
        let path = entry.path();
        if !path.is_dir() {
            continue;
        }

        let job_file = path.join(format!("J.{}", job_id));
        if job_file.exists() {
            return Job::load(&job_file).ok();
        }
    }

    None
}

/// Execute SSH command to transfer file TO remote
pub fn ssh_send_file(
    host: &str,
    local_path: &str,
    remote_path: &str,
    create_dirs: bool,
) -> io::Result<()> {
    // Optionally create directory on remote
    if create_dirs {
        if let Some(parent) = Path::new(remote_path).parent() {
            let parent_escaped = shell_escape(&parent.display().to_string());
            let mkdir_cmd = format!("mkdir -p {}", parent_escaped);
            let status = Command::new("ssh")
                .args(["-T", "-o", "BatchMode=yes", host, &mkdir_cmd])
                .status()?;
            if !status.success() {
                return Err(io::Error::other("Failed to create remote directory"));
            }
        }
    }

    // Send file content via ssh cat
    let mut file = File::open(local_path)?;
    let mut content = Vec::new();
    file.read_to_end(&mut content)?;

    let remote_escaped = shell_escape(remote_path);
    let cat_cmd = format!("cat > {}", remote_escaped);
    let mut child = Command::new("ssh")
        .args(["-T", "-o", "BatchMode=yes", host, &cat_cmd])
        .stdin(Stdio::piped())
        .spawn()?;

    if let Some(ref mut stdin) = child.stdin {
        stdin.write_all(&content)?;
    }

    let status = child.wait()?;
    if !status.success() {
        return Err(io::Error::other("Failed to send file to remote"));
    }

    Ok(())
}

/// Execute SSH command to fetch file FROM remote
pub fn ssh_fetch_file(
    host: &str,
    remote_path: &str,
    local_path: &str,
    create_dirs: bool,
) -> io::Result<()> {
    // Create local directory if needed
    if create_dirs {
        if let Some(parent) = Path::new(local_path).parent() {
            fs::create_dir_all(parent)?;
        }
    }

    let remote_escaped = shell_escape(remote_path);
    let cat_cmd = format!("cat {}", remote_escaped);
    let output = Command::new("ssh")
        .args(["-T", "-o", "BatchMode=yes", host, &cat_cmd])
        .output()?;

    if !output.status.success() {
        let err = String::from_utf8_lossy(&output.stderr);
        return Err(io::Error::other(format!(
            "Failed to fetch file: {}",
            err.trim()
        )));
    }

    fs::write(local_path, &output.stdout)?;
    Ok(())
}

/// Execute remote command via SSH
pub fn ssh_exec(
    host: &str,
    command: &str,
    stdin_data: Option<&[u8]>,
) -> io::Result<(i32, Vec<u8>, Vec<u8>)> {
    let mut child = Command::new("ssh")
        .args(["-T", "-o", "BatchMode=yes", host, command])
        .stdin(if stdin_data.is_some() {
            Stdio::piped()
        } else {
            Stdio::null()
        })
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()?;

    if let Some(data) = stdin_data {
        if let Some(ref mut stdin) = child.stdin {
            stdin.write_all(data)?;
        }
    }

    let output = child.wait_with_output()?;
    let code = output.status.code().unwrap_or(1);

    Ok((code, output.stdout, output.stderr))
}

/// Send mail notification
pub fn send_mail(to: &str, subject: &str, body: &str) -> io::Result<()> {
    let mut child = Command::new("mail")
        .args(["-s", subject, to])
        .stdin(Stdio::piped())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .spawn()?;

    if let Some(ref mut stdin) = child.stdin {
        stdin.write_all(body.as_bytes())?;
    }

    let _ = child.wait();
    Ok(())
}

/// Send mail on remote system
pub fn send_remote_mail(host: &str, to: &str, subject: &str, body: &str) -> io::Result<()> {
    let body_escaped = shell_escape(body);
    let subject_escaped = shell_escape(subject);
    let to_escaped = shell_escape(to);
    let cmd = format!(
        "echo {} | mail -s {} {}",
        body_escaped, subject_escaped, to_escaped
    );
    let _ = ssh_exec(host, &cmd, None);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_shell_escape_simple() {
        assert_eq!(shell_escape("hello"), "'hello'");
    }

    #[test]
    fn test_shell_escape_with_spaces() {
        assert_eq!(shell_escape("hello world"), "'hello world'");
    }

    #[test]
    fn test_shell_escape_with_single_quote() {
        assert_eq!(shell_escape("it's"), "'it'\\''s'");
    }

    #[test]
    fn test_shell_escape_with_multiple_quotes() {
        assert_eq!(shell_escape("it's a 'test'"), "'it'\\''s a '\\''test'\\'''");
    }

    #[test]
    fn test_shell_escape_with_special_chars() {
        assert_eq!(shell_escape("$HOME"), "'$HOME'");
        assert_eq!(shell_escape("`whoami`"), "'`whoami`'");
        assert_eq!(shell_escape("$(id)"), "'$(id)'");
        assert_eq!(shell_escape("a;rm -rf /"), "'a;rm -rf /'");
    }

    #[test]
    fn test_shell_escape_empty() {
        assert_eq!(shell_escape(""), "''");
    }
}
