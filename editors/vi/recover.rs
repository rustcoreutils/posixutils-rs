//! Crash/hangup recovery for vi/ex.
//!
//! When the editor receives a hangup (SIGHUP), termination (SIGTERM), or an
//! unexpected end-of-input while the buffer has unsaved changes — or when the
//! user runs `:preserve` — a copy of the edit buffer is written to a recovery
//! file under a per-user recovery directory, and (best effort) the user is
//! mailed a notification. `vi -r` (or `:recover`) reads the saved buffer back.
//!
//! Recovery file format (text):
//! ```text
//! VI-RECOVER-1
//! path: /abs/path/to/original        (empty if the buffer was unnamed)
//! time: <unix epoch seconds>
//! user: <login name>
//! ---
//! <buffer contents>
//! ```

use std::fs::{self, File};
use std::io::{self, BufRead, BufReader, Read, Write};
use std::path::{Path, PathBuf};
use std::time::{SystemTime, UNIX_EPOCH};

const MAGIC: &str = "VI-RECOVER-1";

/// Metadata describing one recoverable edit buffer.
pub struct RecoverInfo {
    /// Path of the recovery file itself.
    pub recovery_path: PathBuf,
    /// Original file path the buffer was editing, if any.
    pub orig_path: Option<String>,
    /// Modification time (unix epoch seconds).
    pub modified: u64,
}

/// The directory used to store recovery files, created (mode 0700) if needed.
pub fn recover_dir() -> PathBuf {
    let base = std::env::var_os("TMPDIR")
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from("/tmp"));
    let dir = base.join("vi.recover");
    let _ = fs::create_dir_all(&dir);
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let _ = fs::set_permissions(&dir, fs::Permissions::from_mode(0o700));
    }
    dir
}

fn now_secs() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_secs())
        .unwrap_or(0)
}

/// Write `text` to a new recovery file for the (optional) original path.
/// Returns the path of the recovery file.
pub fn preserve(orig: Option<&Path>, text: &str) -> io::Result<PathBuf> {
    let dir = recover_dir();
    let pid = std::process::id();
    let secs = now_secs();
    let base = orig
        .and_then(|p| p.file_name())
        .and_then(|s| s.to_str())
        .unwrap_or("buffer");
    let safe: String = base
        .chars()
        .map(|c| {
            if c.is_ascii_alphanumeric() || matches!(c, '.' | '-' | '_') {
                c
            } else {
                '_'
            }
        })
        .collect();
    let path = dir.join(format!("recover.{}.{}.{}", safe, pid, secs));

    let mut f = File::create(&path)?;
    writeln!(f, "{}", MAGIC)?;
    match orig {
        Some(p) => writeln!(f, "path: {}", p.display())?,
        None => writeln!(f, "path:")?,
    }
    writeln!(f, "time: {}", secs)?;
    writeln!(f, "user: {}", std::env::var("USER").unwrap_or_default())?;
    writeln!(f, "---")?;
    f.write_all(text.as_bytes())?;
    if !text.is_empty() && !text.ends_with('\n') {
        writeln!(f)?;
    }
    f.flush()?;
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let _ = fs::set_permissions(&path, fs::Permissions::from_mode(0o600));
    }
    Ok(path)
}

/// Parse the header of a recovery file into a [`RecoverInfo`].
fn read_header(path: &Path) -> io::Result<RecoverInfo> {
    let mut r = BufReader::new(File::open(path)?);
    let mut line = String::new();
    r.read_line(&mut line)?;
    if line.trim_end() != MAGIC {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "not a recovery file",
        ));
    }
    let mut orig_path = None;
    let mut modified = 0u64;
    loop {
        line.clear();
        if r.read_line(&mut line)? == 0 {
            break;
        }
        let l = line.trim_end();
        if l == "---" {
            break;
        }
        if let Some(v) = l.strip_prefix("path:") {
            let v = v.trim();
            if !v.is_empty() {
                orig_path = Some(v.to_string());
            }
        } else if let Some(v) = l.strip_prefix("time:") {
            modified = v.trim().parse().unwrap_or(0);
        }
    }
    Ok(RecoverInfo {
        recovery_path: path.to_path_buf(),
        orig_path,
        modified,
    })
}

/// List recoverable buffers, newest first.
pub fn list() -> Vec<RecoverInfo> {
    let mut out = Vec::new();
    if let Ok(rd) = fs::read_dir(recover_dir()) {
        for ent in rd.flatten() {
            let p = ent.path();
            let is_recover = p
                .file_name()
                .and_then(|s| s.to_str())
                .map(|s| s.starts_with("recover."))
                .unwrap_or(false);
            if is_recover {
                if let Ok(info) = read_header(&p) {
                    out.push(info);
                }
            }
        }
    }
    // Newest first.
    out.sort_by_key(|i| std::cmp::Reverse(i.modified));
    out
}

/// Read the buffer text (the body) from a recovery file.
pub fn load_body(path: &Path) -> io::Result<String> {
    let mut r = BufReader::new(File::open(path)?);
    let mut line = String::new();
    loop {
        line.clear();
        if r.read_line(&mut line)? == 0 {
            break;
        }
        if line.trim_end() == "---" {
            break;
        }
    }
    let mut body = String::new();
    r.read_to_string(&mut body)?;
    Ok(body)
}

/// Find the newest recovery file for `orig` (or the newest of any if `None`).
pub fn find_for(orig: Option<&str>) -> Option<RecoverInfo> {
    let all = list();
    match orig {
        Some(want) => all
            .into_iter()
            .find(|i| i.orig_path.as_deref() == Some(want)),
        None => all.into_iter().next(),
    }
}

/// Remove a recovery file (e.g. after a successful recovery).
pub fn remove(path: &Path) {
    let _ = fs::remove_file(path);
}

/// Remove recovery files older than `max_age_secs`.
pub fn cleanup_stale(max_age_secs: u64) {
    let now = now_secs();
    for info in list() {
        if now.saturating_sub(info.modified) > max_age_secs {
            remove(&info.recovery_path);
        }
    }
}

/// Best-effort email notification that a buffer was preserved.
pub fn notify(orig: Option<&Path>) {
    use std::process::{Command, Stdio};
    let user = match std::env::var("USER") {
        Ok(u) if !u.is_empty() => u,
        _ => return,
    };
    let fname = orig
        .map(|p| p.display().to_string())
        .unwrap_or_else(|| "an unnamed buffer".to_string());
    let body = format!(
        "From: vi\nTo: {user}\nSubject: editor recovery\n\n\
         A copy of an edit session for {fname} was saved.\n\
         Run \"vi -r\" to recover it.\n"
    );
    for prog in ["/usr/sbin/sendmail", "/usr/lib/sendmail", "sendmail"] {
        if let Ok(mut child) = Command::new(prog)
            .arg("-t")
            .stdin(Stdio::piped())
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .spawn()
        {
            if let Some(mut sin) = child.stdin.take() {
                let _ = sin.write_all(body.as_bytes());
            }
            let _ = child.wait();
            return;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_preserve_and_load_roundtrip() {
        // Use an isolated TMPDIR so the test doesn't see other recovery files.
        let td = tempfile::tempdir().unwrap();
        std::env::set_var("TMPDIR", td.path());

        let orig = PathBuf::from("/home/user/notes.txt");
        let text = "line one\nline two\n";
        let rec = preserve(Some(&orig), text).unwrap();
        assert!(rec.exists());

        let info = find_for(Some("/home/user/notes.txt")).expect("recoverable found");
        assert_eq!(info.orig_path.as_deref(), Some("/home/user/notes.txt"));

        let body = load_body(&info.recovery_path).unwrap();
        assert_eq!(body, text);

        remove(&rec);
        assert!(!rec.exists());
        std::env::remove_var("TMPDIR");
    }
}
