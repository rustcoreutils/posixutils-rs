//! Crash/hangup recovery for vi/ex.
//!
//! When the editor receives a hangup (SIGHUP), termination (SIGTERM), or an
//! unexpected end-of-input while the buffer has unsaved changes — or when the
//! user runs `:preserve` — a copy of the edit buffer is written to a recovery
//! file under a per-user recovery directory, and (best effort) the user is
//! mailed a notification. `vi -r` (or `:recover`) reads the saved buffer back.
//!
//! Recovery files live in `<base>/vi.recover.<uid>` where `<base>` is the
//! `directory` option (default `$TMPDIR` or `/tmp`). The directory is created
//! mode 0700 and is verified before use to be a real directory (not a symlink)
//! owned by the current user with no group/other access — otherwise it is not
//! used (defending against symlink / shared-directory attacks in a world-
//! writable `/tmp`).
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
use std::os::unix::fs::MetadataExt;
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

/// The default base directory for recovery files (`$TMPDIR` or `/tmp`).
pub fn default_base() -> String {
    std::env::var("TMPDIR")
        .ok()
        .filter(|s| !s.is_empty())
        .unwrap_or_else(|| "/tmp".to_string())
}

/// Resolve (and create, mode 0700) the per-user recovery directory under
/// `base`, verifying it is a real directory owned by the current user with no
/// group/other access. Returns an error if the directory is missing/unsafe.
pub fn recover_dir(base: &str) -> io::Result<PathBuf> {
    use std::os::unix::fs::PermissionsExt;

    let uid = unsafe { libc::getuid() };
    let dir = Path::new(base).join(format!("vi.recover.{}", uid));

    // Create on first use; ignore "already exists" so we can re-validate below.
    if let Err(e) = fs::create_dir(&dir) {
        if e.kind() != io::ErrorKind::AlreadyExists {
            // The base may itself be missing; try to create the whole path.
            fs::create_dir_all(&dir)?;
        }
    }
    // Tighten permissions if we own it (best effort).
    let _ = fs::set_permissions(&dir, fs::Permissions::from_mode(0o700));

    // Validate using lstat semantics: must be a real directory (symlink_metadata
    // does NOT follow symlinks, so a symlink reports is_dir() == false).
    let md = fs::symlink_metadata(&dir)?;
    if !md.is_dir() {
        return Err(io::Error::new(
            io::ErrorKind::PermissionDenied,
            "recovery path is not a directory",
        ));
    }
    if md.uid() != uid {
        return Err(io::Error::new(
            io::ErrorKind::PermissionDenied,
            "recovery directory not owned by the current user",
        ));
    }
    if md.mode() & 0o077 != 0 {
        return Err(io::Error::new(
            io::ErrorKind::PermissionDenied,
            "recovery directory is group/other accessible",
        ));
    }
    Ok(dir)
}

fn now_secs() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_secs())
        .unwrap_or(0)
}

/// Write `text` to a new recovery file under `base` for the (optional) original
/// path. Returns the path of the recovery file.
pub fn preserve(orig: Option<&Path>, text: &str, base: &str) -> io::Result<PathBuf> {
    use std::os::unix::fs::PermissionsExt;

    let dir = recover_dir(base)?;
    let pid = std::process::id();
    let secs = now_secs();
    let name = orig
        .and_then(|p| p.file_name())
        .and_then(|s| s.to_str())
        .unwrap_or("buffer");
    let safe: String = name
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
    let _ = f.set_permissions(fs::Permissions::from_mode(0o600));
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

/// List recoverable buffers under `base`, newest first.
pub fn list(base: &str) -> Vec<RecoverInfo> {
    let mut out = Vec::new();
    let Ok(dir) = recover_dir(base) else {
        return out;
    };
    if let Ok(rd) = fs::read_dir(&dir) {
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

/// Find the newest recovery file under `base` for `orig` (or the newest of any).
pub fn find_for(base: &str, orig: Option<&str>) -> Option<RecoverInfo> {
    let all = list(base);
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

/// Remove recovery files under `base` older than `max_age_secs`.
pub fn cleanup_stale(base: &str, max_age_secs: u64) {
    let now = now_secs();
    for info in list(base) {
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
    // Defend against RFC-822 header injection via $USER / the file path when
    // piping to `sendmail -t`.
    if user.contains(['\n', '\r']) {
        return;
    }
    let fname = orig
        .map(|p| p.display().to_string())
        .unwrap_or_else(|| "an unnamed buffer".to_string())
        .replace(['\n', '\r'], " ");
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
        let td = tempfile::tempdir().unwrap();
        let base = td.path().to_str().unwrap();

        let orig = PathBuf::from("/home/user/notes.txt");
        let text = "line one\nline two\n";
        let rec = preserve(Some(&orig), text, base).unwrap();
        assert!(rec.exists());

        let info = find_for(base, Some("/home/user/notes.txt")).expect("recoverable found");
        assert_eq!(info.orig_path.as_deref(), Some("/home/user/notes.txt"));

        let body = load_body(&info.recovery_path).unwrap();
        assert_eq!(body, text);

        remove(&rec);
        assert!(!rec.exists());
    }

    #[test]
    fn test_recover_dir_rejects_symlink() {
        let td = tempfile::tempdir().unwrap();
        let base = td.path();
        let uid = unsafe { libc::getuid() };
        // Pre-create the recovery dir name as a symlink to elsewhere.
        let target = td.path().join("elsewhere");
        fs::create_dir(&target).unwrap();
        let link = base.join(format!("vi.recover.{}", uid));
        std::os::unix::fs::symlink(&target, &link).unwrap();

        assert!(recover_dir(base.to_str().unwrap()).is_err());
    }
}
