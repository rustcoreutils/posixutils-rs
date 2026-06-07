//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Shared at/batch spool machinery: job-directory discovery, job-id
//! sequencing, the at-job script representation, the allow/deny gate, and the
//! invoking-user lookup. Both the `at` and `batch` binaries build on this so
//! the two stay in lock-step (audit #B7).

use chrono::{DateTime, Utc};
use libc::{getpwuid, getuid, passwd};

use std::{
    collections::HashSet,
    env,
    ffi::CStr,
    fs,
    io::{self, ErrorKind, Read, Seek, Write},
    os::unix::fs::PermissionsExt,
    os::unix::io::AsRawFd,
    path::{Path, PathBuf},
    process,
};

#[cfg(target_os = "linux")]
pub const SPOOL_DIRECTORIES: &[&str] = &[
    "/var/spool/cron/atjobs/",
    "/var/spool/at/",
    "/var/spool/atjobs/",
];

#[cfg(target_os = "macos")]
pub const MACOS_DIRECTORY: &str = "/var/at/jobs/";

#[cfg(target_os = "linux")]
pub const DEFAULT_DIRECTORY: &str = "/var/spool/atjobs/";

/// Returns the path to the jobs directory, adjusted for the operating system.
/// On Linux: checks the `AT_JOB_DIR` environment variable, then predefined directories.
/// On macOS: checks or creates the `/var/at/jobs` directory.
pub fn get_job_dir() -> Result<String, String> {
    // Check `AT_JOB_DIR` environment variable
    if let Ok(env_dir) = env::var("AT_JOB_DIR") {
        if Path::new(&env_dir).exists() {
            return Ok(env_dir);
        }
    }
    #[cfg(target_os = "linux")]
    {
        // Check the predefined spool directories
        for dir in SPOOL_DIRECTORIES {
            if Path::new(dir).exists() {
                return Ok(dir.to_string());
            }
        }

        // Create the default directory if none exist
        let default_path = Path::new(DEFAULT_DIRECTORY);
        if !default_path.exists() {
            if let Err(err) = fs::create_dir_all(default_path) {
                return Err(format!(
                    "Failed to create directory {}: {}",
                    DEFAULT_DIRECTORY, err
                ));
            }
        }

        Ok(DEFAULT_DIRECTORY.to_string())
    }
    #[cfg(target_os = "macos")]
    {
        let macos_path = Path::new(MACOS_DIRECTORY);

        if !macos_path.exists() {
            if let Err(err) = fs::create_dir_all(macos_path) {
                return Err(format!(
                    "Failed to create directory {}: {}",
                    MACOS_DIRECTORY, err
                ));
            }
        }

        Ok(MACOS_DIRECTORY.to_string())
    }
}

pub fn print_err_and_exit(exit_code: i32, err: impl std::fmt::Display) -> ! {
    eprintln!("{}", err);
    process::exit(exit_code)
}

/// Submit `cmd` to the at spool for execution at `execution_time` in `queue`.
pub fn at(
    queue: Option<char>,
    execution_time: &DateTime<Utc>,
    cmd: impl Into<String>,
    mail: bool,
) -> Result<(), Box<dyn std::error::Error>> {
    let jobno = next_job_id()?;
    let job_filename = job_file_name(jobno, queue, execution_time)
        .ok_or("Failed to generate file name for job")?;

    let user = User::current().ok_or("Failed to get current user")?;
    if !is_user_allowed(&user.name) {
        return Err(format!("Access denied for user: {}", &user.name).into());
    }

    let job = Job::new(&user, std::env::current_dir()?, std::env::vars(), cmd, mail).into_script();

    let mut file_opt = std::fs::OpenOptions::new();
    file_opt.read(true).write(true).create_new(true);

    let file_path = PathBuf::from(format!("{}/{job_filename}", get_job_dir()?));

    let mut file = file_opt
        .open(&file_path)
        .map_err(|e| format!("Failed to create file with job. Reason: {e}"))?;

    file.write_all(job.as_bytes())?;

    file.set_permissions(std::fs::Permissions::from_mode(0o700))?;

    // POSIX: the submission notice "job %s at %s\n" is written to standard error,
    // with the date as `date +"%a %b %e %T %Y"` adjusted to the user's timezone
    // (audit #A3/#A14/#B2; timezone per #A5/#B4).
    eprintln!(
        "job {} at {}",
        jobno,
        execution_time
            .with_timezone(&chrono::Local)
            .format("%a %b %e %H:%M:%S %Y")
    );

    Ok(())
}

/// Structure to represent future job or script to be saved
pub struct Job {
    shell: String,
    user_uid: u32,
    user_gid: u32,
    user_name: String,
    env: std::env::Vars,
    call_place: PathBuf,
    cmd: String,
    mail: bool,
}

impl Job {
    pub fn new(
        User {
            shell,
            uid,
            gid,
            name,
        }: &User,
        call_place: PathBuf,
        env: std::env::Vars,
        cmd: impl Into<String>,
        mail: bool,
    ) -> Self {
        Self {
            shell: shell.to_owned(),
            user_uid: *uid,
            user_gid: *gid,
            user_name: name.to_owned(),
            env,
            call_place,
            cmd: cmd.into(),
            mail,
        }
    }

    pub fn into_script(self) -> String {
        let Self {
            shell,
            user_uid,
            user_gid,
            user_name,
            env,
            call_place,
            cmd,
            mail,
        } = self;

        // Environment values and the working directory are single-quoted so
        // spaces and shell metacharacters survive intact (audit #A8).
        let env = env
            .into_iter()
            .map(|(key, value)| format!("{}={}; export {}", key, sh_single_quote(&value), key))
            .collect::<Vec<_>>()
            .join("\n");

        format!(
            "#!{shell}\n# atrun uid={user_uid} gid={user_gid}\n# mail {user_name} {}\numask {:03o}\n{env}\ncd {} || {{\n\techo 'Execution directory inaccessible' >&2\n\texit 1 \n}}\n{cmd}",
            if mail { 1 } else { 0 },
            current_umask(),
            sh_single_quote(&call_place.to_string_lossy())
        )
    }
}

/// Single-quote a string for safe inclusion in a POSIX shell script.
fn sh_single_quote(s: &str) -> String {
    let mut out = String::with_capacity(s.len() + 2);
    out.push('\'');
    for c in s.chars() {
        if c == '\'' {
            out.push_str("'\\''");
        } else {
            out.push(c);
        }
    }
    out.push('\'');
    out
}

/// Read the invoking process's file-creation mask without changing it. The
/// at-job retains this umask per spec (audit #A8).
fn current_umask() -> libc::mode_t {
    // SAFETY: umask() never fails; we set it to read the old value, then restore.
    unsafe {
        let m = libc::umask(0);
        libc::umask(m);
        m & 0o777
    }
}

/// Locate the at spool directory without creating it (used by the daemon, which
/// must not fabricate the spool). Mirrors [`get_job_dir`]'s search order.
pub fn at_spool_dir_readonly() -> Option<PathBuf> {
    if let Ok(env_dir) = env::var("AT_JOB_DIR") {
        let p = PathBuf::from(env_dir);
        if p.exists() {
            return Some(p);
        }
    }

    #[cfg(target_os = "linux")]
    {
        for dir in SPOOL_DIRECTORIES {
            let p = PathBuf::from(dir);
            if p.exists() {
                return Some(p);
            }
        }
        let p = PathBuf::from(DEFAULT_DIRECTORY);
        p.exists().then_some(p)
    }
    #[cfg(target_os = "macos")]
    {
        let p = PathBuf::from(MACOS_DIRECTORY);
        p.exists().then_some(p)
    }
}

/// A parsed at-spool job filename, the inverse of [`job_file_name`].
pub struct AtJobName {
    pub queue: char,
    pub job_id: u32,
    /// Scheduled execution time in epoch-minutes.
    pub exec_minute: u64,
}

/// Parse an at-spool filename (`<queue><jobid:05x><minutes:08x>`), rejecting the
/// `.SEQ` sequence file, temporary files, and anything malformed.
pub fn parse_at_job_name(name: &str) -> Option<AtJobName> {
    if name.len() != 14 {
        return None;
    }
    let queue = name.chars().next()?;
    if !queue.is_ascii_alphabetic() {
        return None;
    }
    let job_id = u32::from_str_radix(&name[1..6], 16).ok()?;
    let exec_minute = u64::from_str_radix(&name[6..14], 16).ok()?;
    Some(AtJobName {
        queue,
        job_id,
        exec_minute,
    })
}

/// Return name for job number
///
/// None if DateTime < [DateTime::UNIX_EPOCH]
pub fn job_file_name(next_job: u32, queue: Option<char>, time: &DateTime<Utc>) -> Option<String> {
    let duration = time.signed_duration_since(DateTime::UNIX_EPOCH);
    let duration_seconds = u32::try_from(duration.num_seconds()).ok()? / 60;
    let queue = queue.unwrap_or('a');
    let result = format!("{queue}{next_job:05x}{duration_seconds:08x}");

    Some(result)
}

#[derive(Debug)]
pub enum NextJobError {
    Io(std::io::Error),
    FromStr(std::num::ParseIntError),
}

impl std::fmt::Display for NextJobError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Error reading id of next job. Reason: ")?;
        match self {
            NextJobError::Io(err) => writeln!(f, "{err}"),
            NextJobError::FromStr(err) => writeln!(f, "invalid number - {err}"),
        }
    }
}

impl std::error::Error for NextJobError {}

pub fn next_job_id() -> Result<u32, Box<dyn std::error::Error>> {
    let job_file_number = format!("{}.SEQ", get_job_dir()?);

    let mut file = std::fs::OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .truncate(false)
        .open(&job_file_number)
        .map_err(NextJobError::Io)?;

    // Hold an exclusive lock across the read-modify-write so concurrent `at`
    // invocations cannot hand out the same job id (audit #A11). The lock is
    // released when `file` is dropped.
    // SAFETY: the fd is valid for the lifetime of `file`; flock with LOCK_EX is
    // a blocking exclusive lock and we check the return value.
    if unsafe { libc::flock(file.as_raw_fd(), libc::LOCK_EX) } != 0 {
        return Err(NextJobError::Io(io::Error::last_os_error()).into());
    }

    let mut buf = String::new();
    file.read_to_string(&mut buf).map_err(NextJobError::Io)?;

    let prev = match buf.trim() {
        "" => 0,
        s => u32::from_str_radix(s, 16).map_err(NextJobError::FromStr)?,
    };

    // Limit range of jobs to 2^20 jobs
    let next_job_id = (1 + prev) % 0xfffff;

    file.rewind().map_err(NextJobError::Io)?;
    file.set_len(0).map_err(NextJobError::Io)?;
    file.write_all(format!("{next_job_id:05x}").as_bytes())
        .map_err(NextJobError::Io)?;

    Ok(next_job_id)
}

fn read_user_file(file_path: &str) -> std::io::Result<HashSet<String>> {
    let content = std::fs::read_to_string(file_path)?;
    Ok(content
        .lines()
        .map(|line| line.trim().to_string())
        .collect())
}

/// Locations of the at.allow / at.deny files. The implementation-defined
/// location is `/etc`; the `AT_ALLOW` / `AT_DENY` overrides are honored only
/// when not running set-uid (real uid == effective uid), so a set-uid `at`
/// cannot be tricked into reading attacker-chosen allow/deny files.
fn allow_deny_paths() -> (String, String) {
    // SAFETY: getuid()/geteuid() never fail.
    let overridable = unsafe { libc::getuid() == libc::geteuid() };
    let pick = |var: &str, default: &str| {
        if overridable {
            env::var(var).unwrap_or_else(|_| default.to_string())
        } else {
            default.to_string()
        }
    };
    (
        pick("AT_ALLOW", "/etc/at.allow"),
        pick("AT_DENY", "/etc/at.deny"),
    )
}

pub fn is_user_allowed(user: &str) -> bool {
    let (allow_file, deny_file) = allow_deny_paths();

    // If at.allow exists, only the users listed in it have access.
    match read_user_file(&allow_file) {
        Ok(allowed) => return allowed.contains(user),
        Err(e) if e.kind() != ErrorKind::NotFound => return false,
        Err(_) => {}
    }

    // Otherwise, if at.deny exists, everyone not listed has access (an empty
    // at.deny therefore permits all users).
    match read_user_file(&deny_file) {
        Ok(denied) => return !denied.contains(user),
        Err(e) if e.kind() != ErrorKind::NotFound => return false,
        Err(_) => {}
    }

    // Neither file exists: only a privileged process may submit (POSIX XSI).
    unsafe { getuid() == 0 }
}

/// The invoking user, resolved from the real uid via `getpwuid(getuid())`.
///
/// Identity is taken from the kernel-reported real uid, never from the
/// spoofable `$LOGNAME`/`getlogin()` (audit #X2).
pub struct User {
    pub name: String,
    pub shell: String,
    pub uid: u32,
    pub gid: u32,
}

impl User {
    pub fn current() -> Option<Self> {
        const DEFAULT_SHELL: &str = "/bin/sh";

        // SAFETY: getpwuid() is read-only; we copy every field we keep out of
        // the returned struct before it can be invalidated by another call.
        unsafe {
            let passwd {
                pw_uid,
                pw_gid,
                pw_name,
                pw_shell,
                ..
            } = *resolve_passwd()?;

            let name = CStr::from_ptr(pw_name).to_str().ok()?.to_owned();

            let shell = if pw_shell.is_null() {
                std::env::var("SHELL").unwrap_or_else(|_| DEFAULT_SHELL.to_owned())
            } else {
                CStr::from_ptr(pw_shell)
                    .to_str()
                    .unwrap_or(DEFAULT_SHELL)
                    .to_owned()
            };

            Some(Self {
                shell,
                uid: pw_uid,
                gid: pw_gid,
                name,
            })
        }
    }
}

/// `getpwuid(getuid())`, returning the raw passwd pointer or `None`.
///
/// # Safety
/// The returned pointer aliases libc's static passwd buffer and must be copied
/// from before the next passwd lookup.
unsafe fn resolve_passwd() -> Option<*const passwd> {
    let pw_ptr = getpwuid(getuid());
    if pw_ptr.is_null() {
        None
    } else {
        Some(pw_ptr)
    }
}

#[cfg(test)]
mod tests {
    use super::sh_single_quote;

    #[test]
    fn sh_quote_wraps_plain_and_spaces() {
        assert_eq!(sh_single_quote("abc"), "'abc'");
        assert_eq!(sh_single_quote("a b c"), "'a b c'");
    }

    #[test]
    fn sh_quote_neutralizes_metacharacters() {
        // Shell metacharacters are inert inside single quotes.
        assert_eq!(sh_single_quote("rm -rf /; echo x"), "'rm -rf /; echo x'");
        assert_eq!(sh_single_quote("$(id)`id`"), "'$(id)`id`'");
    }

    #[test]
    fn sh_quote_escapes_embedded_single_quote() {
        assert_eq!(sh_single_quote("a'b"), "'a'\\''b'");
    }

    #[test]
    fn at_job_name_roundtrips() {
        use super::{job_file_name, parse_at_job_name};
        use chrono::{DateTime, Utc};

        let t = DateTime::<Utc>::from_timestamp(1_700_000_040, 0).unwrap();
        let name = job_file_name(0x1a, Some('b'), &t).unwrap();
        let parsed = parse_at_job_name(&name).expect("valid name");
        assert_eq!(parsed.queue, 'b');
        assert_eq!(parsed.job_id, 0x1a);
        assert_eq!(parsed.exec_minute, 1_700_000_040 / 60);
    }

    #[test]
    fn at_job_name_rejects_seq_and_garbage() {
        use super::parse_at_job_name;
        assert!(parse_at_job_name("a00001.SEQ").is_none());
        assert!(parse_at_job_name("too-short").is_none());
        assert!(parse_at_job_name("100001041a0e81").is_none()); // non-alpha queue
    }
}
