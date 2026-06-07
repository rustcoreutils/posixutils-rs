//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use chrono::{DateTime, Local, NaiveDateTime};
use cron::job::{Database, UserInfo};
use cron::spool::{at_spool_dir_readonly, parse_at_job_name};
use cron::trust::{open_trusted, TrustPolicy};
use cron::{CRON_SPOOL_DIR, PID_FILE, SYSTEM_CRONTAB};
use gettextrs::{bind_textdomain_codeset, setlocale, textdomain, LocaleCategory};
use std::cmp::Ordering::{Greater, Less};
use std::collections::HashMap;
use std::error::Error;
use std::fmt;
use std::fs::{self, File, OpenOptions};
use std::io::{Read, Write};
use std::os::unix::fs::MetadataExt;
use std::os::unix::io::AsRawFd;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Mutex;
use std::time::SystemTime;

static CRONTAB: Mutex<Option<Database>> = Mutex::new(None);
/// Per-file modification times of every loaded crontab, so a reload happens
/// only when a file is actually added, removed, or changed (audit #D9/#D10).
static MTIMES: Mutex<Option<HashMap<PathBuf, SystemTime>>> = Mutex::new(None);

/// Atomic flag set by SIGHUP handler to signal reload needed
static RELOAD_FLAG: AtomicBool = AtomicBool::new(false);

/// Atomic flag set by SIGTERM/SIGINT handlers to signal shutdown
static SHUTDOWN_FLAG: AtomicBool = AtomicBool::new(false);

#[derive(Debug)]
enum CronError {
    Fork,
    AlreadyRunning,
    PidFile(std::io::Error),
}

impl Error for CronError {}

impl fmt::Display for CronError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Fork => write!(f, "Could not create child process"),
            Self::AlreadyRunning => write!(f, "Another crond instance is already running"),
            Self::PidFile(e) => write!(f, "Could not create PID file: {}", e),
        }
    }
}

/// Collect the modification time of every crontab file the daemon loads (each
/// spool entry plus the system crontab). Missing files are simply absent from
/// the map.
fn collect_mtimes() -> HashMap<PathBuf, SystemTime> {
    let mut mtimes = HashMap::new();

    if let Ok(entries) = fs::read_dir(CRON_SPOOL_DIR) {
        for entry in entries.flatten() {
            if let Ok(meta) = entry.metadata() {
                if let Ok(modified) = meta.modified() {
                    mtimes.insert(entry.path(), modified);
                }
            }
        }
    }

    if let Ok(meta) = fs::metadata(SYSTEM_CRONTAB) {
        if let Ok(modified) = meta.modified() {
            mtimes.insert(PathBuf::from(SYSTEM_CRONTAB), modified);
        }
    }

    mtimes
}

/// Return true if any crontab file was added, removed, or modified since the
/// last load, updating the stored snapshot (audit #D9/#D10).
fn needs_reload() -> bool {
    let current = collect_mtimes();
    let mut guard = MTIMES.lock().unwrap();
    let changed = match &*guard {
        None => true,
        Some(previous) => *previous != current,
    };
    if changed {
        *guard = Some(current);
    }
    changed
}

/// Load a crontab file after a trust check, appending its jobs to `db`. A file
/// that fails the trust check is skipped with a diagnostic (audit #D1/#D2).
fn load_trusted<F>(path: &Path, policy: &TrustPolicy, label: &str, parse: F, db: &mut Database)
where
    F: FnOnce(&str) -> Database,
{
    match open_trusted(path, policy) {
        Ok((mut file, _meta)) => {
            let mut content = String::new();
            if file.read_to_string(&mut content).is_ok() {
                let parsed = parse(&content);
                let merged = std::mem::replace(db, Database(vec![])).merge(parsed);
                *db = merged;
            }
        }
        Err(reason) => {
            // Only warn when a file is actually present (a missing optional file
            // such as /etc/crontab is not an error).
            if path.exists() {
                eprintln!("crond: ignoring {label}: {reason}");
            }
        }
    }
}

/// Update [`CRONTAB`] by loading all trusted user crontabs from the spool
/// directory plus the system crontab.
fn sync_cronfile() -> Result<(), Box<dyn Error>> {
    if (*CRONTAB.lock().unwrap()).is_none() || needs_reload() {
        let mut combined_db = Database(vec![]);

        // Load all user crontabs from the spool directory.
        if let Ok(entries) = fs::read_dir(CRON_SPOOL_DIR) {
            for entry in entries.flatten() {
                // The filename is the username.
                let username = entry.file_name().to_string_lossy().into_owned();

                // Look up user info for privilege dropping; skip unknown users.
                if let Some(user_info) = UserInfo::from_username(&username) {
                    let policy = TrustPolicy::crontab_spool(user_info.uid);
                    let label = format!("crontab for {username}");
                    load_trusted(
                        &entry.path(),
                        &policy,
                        &label,
                        |content| Database::parse_user_crontab(content, &user_info),
                        &mut combined_db,
                    );
                }
            }
        }

        // Load the system crontab (6-field format with username).
        load_trusted(
            Path::new(SYSTEM_CRONTAB),
            &TrustPolicy::system_crontab(),
            SYSTEM_CRONTAB,
            Database::parse_system_crontab,
            &mut combined_db,
        );

        *CRONTAB.lock().unwrap() = Some(combined_db);
    }
    Ok(())
}

/// Acquire PID file lock to prevent multiple daemon instances
/// Returns the file handle which must be kept open for the lock to persist
fn acquire_lock() -> Result<File, CronError> {
    // Open WITHOUT O_TRUNC so a second instance's failed lock attempt does not
    // clobber the running daemon's recorded PID (audit #D12).
    let mut file = OpenOptions::new()
        .write(true)
        .create(true)
        .truncate(false)
        .open(PID_FILE)
        .map_err(CronError::PidFile)?;

    // SAFETY: file.as_raw_fd() is valid for the lifetime of `file`; LOCK_EX|
    // LOCK_NB is an exclusive non-blocking lock and we check the return value.
    unsafe {
        if libc::flock(file.as_raw_fd(), libc::LOCK_EX | libc::LOCK_NB) != 0 {
            return Err(CronError::AlreadyRunning);
        }
    }

    // Only after the lock is held, replace the file contents with our PID.
    file.set_len(0).map_err(CronError::PidFile)?;
    writeln!(file, "{}", std::process::id()).map_err(CronError::PidFile)?;
    file.flush().map_err(CronError::PidFile)?;

    Ok(file)
}

/// Create new daemon process of crond
fn setup() -> i32 {
    // SAFETY: These libc calls implement the standard Unix daemon pattern:
    // 1. fork() creates child process; parent exits, child continues
    // 2. setsid() creates new session, detaches from controlling terminal
    // 3. chdir("/") prevents holding directory mounts open
    // 4. close(STD*_FILENO) closes inherited file descriptors
    // All calls have defined behavior and we check fork() return value.
    unsafe {
        use libc::*;

        let pid = fork();
        if pid != 0 {
            return pid;
        }

        setsid();
        chdir(b"/\0" as *const _ as *const c_char);

        close(STDIN_FILENO);
        close(STDOUT_FILENO);
        close(STDERR_FILENO);

        pid
    }
}

/// Handles SIGHUP signal to reload crontab.
/// Sets an atomic flag that the main loop checks - this is async-signal-safe.
extern "C" fn handle_sighup(_: libc::c_int) {
    RELOAD_FLAG.store(true, Ordering::SeqCst);
}

/// Handles SIGTERM/SIGINT signals for graceful shutdown.
/// Sets an atomic flag that the main loop checks - this is async-signal-safe.
extern "C" fn handle_shutdown(_: libc::c_int) {
    SHUTDOWN_FLAG.store(true, Ordering::SeqCst);
}

/// Handles SIGCHLD signal to reap zombie child processes
extern "C" fn handle_sigchld(_: libc::c_int) {
    // SAFETY: waitpid() is async-signal-safe per POSIX.
    // -1 means wait for any child, WNOHANG returns immediately if no zombie.
    // null status pointer is valid when we don't need exit status.
    // Loop reaps all available zombies without blocking.
    unsafe { while libc::waitpid(-1, std::ptr::null_mut(), libc::WNOHANG) > 0 {} }
}

/// Install `handler` for `signum` via `sigaction` (audit #D11). `SA_RESTART` is
/// intentionally omitted so a delivered signal interrupts the daemon's
/// `nanosleep` and is acted on promptly.
fn install_signal(signum: libc::c_int, handler: extern "C" fn(libc::c_int), flags: libc::c_int) {
    // SAFETY: a zeroed sigaction with a valid extern "C" handler and an emptied
    // mask is a well-formed disposition.
    unsafe {
        let mut act: libc::sigaction = std::mem::zeroed();
        act.sa_sigaction = handler as libc::sighandler_t;
        act.sa_flags = flags;
        libc::sigemptyset(&mut act.sa_mask);
        libc::sigaction(signum, &act, std::ptr::null_mut());
    }
}

/// How many missed minutes to replay after a busy tick or forward clock jump.
const CATCHUP_LIMIT: i64 = 3;

fn current_minute() -> i64 {
    Local::now().timestamp() / 60
}

/// Convert an epoch-minute back to the local wall-clock datetime the schedule
/// predicate compares against.
fn minute_to_naive(minute: i64) -> NaiveDateTime {
    DateTime::from_timestamp(minute * 60, 0)
        .map(|dt| dt.with_timezone(&Local).naive_local())
        .unwrap_or_default()
}

/// The inclusive epoch-minute range to process this tick. Catch-up is bounded to
/// `cap` minutes; a backward clock jump (or no new minute) yields an empty range
/// so nothing is re-run (audit #D6/#D14).
fn due_window(last: i64, now: i64, cap: i64) -> std::ops::RangeInclusive<i64> {
    if now <= last {
        // A computed empty range (start > end yields no values).
        return (now + 1)..=now;
    }
    std::cmp::max(last + 1, now - cap + 1)..=now
}

/// Suspend for `seconds`, returning early if interrupted by a signal so SIGTERM
/// and SIGHUP are handled promptly.
fn interruptible_sleep(seconds: i64) {
    if seconds <= 0 {
        return;
    }
    let req = libc::timespec {
        tv_sec: seconds as libc::time_t,
        tv_nsec: 0,
    };
    // SAFETY: `req` is a valid timespec; EINTR simply returns early.
    unsafe {
        libc::nanosleep(&req, std::ptr::null_mut());
    }
}

/// Sleep until just after the next minute boundary.
fn sleep_to_next_minute() {
    let secs_into = Local::now().timestamp().rem_euclid(60);
    interruptible_sleep(60 - secs_into);
}

/// Maximum 1-minute load average at which batch (queue `b`) jobs are started.
const BATCH_MAX_LOAD: f64 = 1.5;

fn loadavg_1min() -> Option<f64> {
    let mut loads = [0f64; 1];
    // SAFETY: getloadavg writes up to `nelem` (1) doubles into the array.
    let n = unsafe { libc::getloadavg(loads.as_mut_ptr(), 1) };
    (n == 1).then_some(loads[0])
}

/// Run one at-spool job (its script provided as `content`) as `owner`. Returns
/// true once the worker has been forked; the caller then unlinks the file so it
/// cannot run twice (audit #X1).
fn run_at_job(content: &str, owner: &UserInfo) -> bool {
    // SAFETY: fork() is checked; the child performs only fork-safe work.
    let pid = unsafe { libc::fork() };
    if pid < 0 {
        return false;
    }
    if pid != 0 {
        return true; // parent: caller unlinks the now-claimed job
    }

    // Child: new session, drop privileges, run the script with output mailed.
    // SAFETY: setsid() in a fresh child is always safe.
    unsafe {
        libc::setsid();
    }
    // SAFETY: called in the forked child before exec.
    if let Err(e) = unsafe { cron::job::drop_privileges(owner.uid, owner.gid, &owner.name) } {
        eprintln!("crond: cannot drop privileges for at job: {e}");
        std::process::exit(1);
    }
    if std::env::set_current_dir(&owner.home).is_err() {
        let _ = std::env::set_current_dir("/");
    }

    // The at script restores the submitter's environment itself, so start from a
    // clean base and feed the script on stdin (avoiding any unlink/path race).
    let mut child = match Command::new("/bin/sh")
        .env_clear()
        .env("PATH", "/usr/bin:/bin")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
    {
        Ok(c) => c,
        Err(_) => std::process::exit(1),
    };
    if let Some(mut stdin) = child.stdin.take() {
        let _ = stdin.write_all(content.as_bytes());
    }
    let output = match child.wait_with_output() {
        Ok(o) => o,
        Err(_) => std::process::exit(1),
    };

    let mut combined = output.stdout;
    combined.extend_from_slice(&output.stderr);
    if !combined.is_empty() {
        cron::job::mail_output(&owner.name, "at job output", &combined);
    }
    std::process::exit(output.status.code().unwrap_or(0));
}

/// Scan the at spool and run every job whose execution minute has arrived,
/// dropping to the file owner's identity. Queue `b` (batch) jobs additionally
/// wait for the load average to fall and only one is started per tick (audit
/// #X1, closing #A2/#A9/#B1/#D3).
fn run_due_at_jobs() {
    let Some(dir) = at_spool_dir_readonly() else {
        return;
    };
    let now_min = current_minute().max(0) as u64;
    let mut batch_started = false;

    let Ok(entries) = fs::read_dir(&dir) else {
        return;
    };
    for entry in entries.flatten() {
        let file_name = entry.file_name();
        let name = file_name.to_string_lossy();
        let Some(job) = parse_at_job_name(&name) else {
            continue;
        };
        if job.exec_minute > now_min {
            continue; // not due yet
        }

        if job.queue == 'b' {
            if batch_started {
                continue; // at most one batch job per tick
            }
            if loadavg_1min().map(|l| l > BATCH_MAX_LOAD).unwrap_or(false) {
                continue; // defer until the load falls
            }
        }

        let path = entry.path();
        // Trust the file (regular, single link, not group/other-writable,
        // O_NOFOLLOW) and take the run-as identity from its owner uid.
        let Ok((mut file, meta)) = open_trusted(&path, &TrustPolicy::at_spool()) else {
            continue; // untrusted; leave it in place
        };
        let Some(owner) = UserInfo::from_uid(meta.uid()) else {
            continue; // owner has no passwd entry
        };
        let mut content = String::new();
        if file.read_to_string(&mut content).is_err() {
            continue;
        }

        if run_at_job(&content, &owner) {
            let _ = fs::remove_file(&path);
            if job.queue == 'b' {
                batch_started = true;
            }
        }
    }
}

/// Daemon loop: at each minute boundary, run every job scheduled for the minutes
/// that have elapsed since the last tick (audit #D6).
fn tick_loop() -> Result<(), Box<dyn Error>> {
    // Start at the current minute so the first tick does not replay history.
    let mut last_run_minute = current_minute();

    loop {
        if SHUTDOWN_FLAG.load(Ordering::SeqCst) {
            return Ok(());
        }
        if RELOAD_FLAG.swap(false, Ordering::SeqCst) {
            *MTIMES.lock().unwrap() = None;
        }

        sleep_to_next_minute();

        if SHUTDOWN_FLAG.load(Ordering::SeqCst) {
            return Ok(());
        }
        if RELOAD_FLAG.swap(false, Ordering::SeqCst) {
            *MTIMES.lock().unwrap() = None;
        }

        sync_cronfile()?;
        let Some(db) = CRONTAB.lock().unwrap().clone() else {
            continue;
        };

        let now = current_minute();
        for minute in due_window(last_run_minute, now, CATCHUP_LIMIT) {
            let when = minute_to_naive(minute);
            for job in &db.0 {
                if job.matches_minute(&when) {
                    let _ = job.run_job(); // errors are reported in the child
                }
            }
        }
        last_run_minute = now;

        // Also run any due one-shot at/batch jobs (audit #X1).
        run_due_at_jobs();
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let pid = setup();

    match pid.cmp(&0) {
        Less => return Err(Box::new(CronError::Fork)),
        Greater => return Ok(()),
        _ => {}
    }

    // Acquire PID file lock (keep handle alive for the daemon's lifetime)
    let _pid_lock = acquire_lock()?;

    // Install handlers via sigaction (audit #D11). Handlers are async-signal-safe
    // (atomic stores or waitpid). SIGCHLD uses SA_NOCLDSTOP to avoid stop/cont
    // notifications.
    install_signal(libc::SIGHUP, handle_sighup, 0);
    install_signal(libc::SIGCHLD, handle_sigchld, libc::SA_NOCLDSTOP);
    install_signal(libc::SIGTERM, handle_shutdown, 0);
    install_signal(libc::SIGINT, handle_shutdown, 0);

    // Run @reboot jobs at startup, and flush any already-due at/batch jobs.
    run_reboot_jobs()?;
    run_due_at_jobs();

    tick_loop()
}

/// Execute all @reboot jobs once at daemon startup
fn run_reboot_jobs() -> Result<(), Box<dyn Error>> {
    sync_cronfile()?;

    let db_guard = CRONTAB.lock().unwrap();
    if let Some(ref db) = *db_guard {
        for job in db.reboot_jobs() {
            let _ = job.run_job();
        }
    }
    drop(db_guard);

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::due_window;

    #[test]
    fn window_single_new_minute() {
        assert_eq!(due_window(100, 101, 3).collect::<Vec<_>>(), vec![101]);
    }

    #[test]
    fn window_bounded_catchup() {
        // A 10-minute gap is clamped to the last CATCHUP_LIMIT minutes.
        assert_eq!(
            due_window(100, 110, 3).collect::<Vec<_>>(),
            vec![108, 109, 110]
        );
    }

    #[test]
    fn window_same_or_backward_is_empty() {
        assert!(due_window(100, 100, 3).collect::<Vec<_>>().is_empty());
        assert!(due_window(100, 95, 3).collect::<Vec<_>>().is_empty());
    }
}
