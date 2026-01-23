//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use chrono::Local;
use cron::job::{Database, UserInfo};
use cron::{CRON_SPOOL_DIR, PID_FILE, SYSTEM_CRONTAB};
use gettextrs::{bind_textdomain_codeset, setlocale, textdomain, LocaleCategory};
use std::cmp::Ordering::{Greater, Less};
use std::error::Error;
use std::fmt;
use std::fs::{self, File, OpenOptions};
use std::io::Write;
use std::os::unix::io::AsRawFd;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Mutex;
use std::time::UNIX_EPOCH;

static CRONTAB: Mutex<Option<Database>> = Mutex::new(None);
static LAST_MODIFIED: Mutex<Option<u64>> = Mutex::new(None);

/// Atomic flag set by SIGHUP handler to signal reload needed
static RELOAD_FLAG: AtomicBool = AtomicBool::new(false);

/// Atomic flag set by SIGTERM/SIGINT handlers to signal shutdown
static SHUTDOWN_FLAG: AtomicBool = AtomicBool::new(false);

#[derive(Debug)]
enum CronError {
    Fork,
    NoCrontab,
    AlreadyRunning,
    PidFile(std::io::Error),
}

impl Error for CronError {}

impl fmt::Display for CronError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::NoCrontab => write!(f, "Could not format database"),
            Self::Fork => write!(f, "Could not create child process"),
            Self::AlreadyRunning => write!(f, "Another crond instance is already running"),
            Self::PidFile(e) => write!(f, "Could not create PID file: {}", e),
        }
    }
}

/// Check if logname file is changed
fn is_file_changed(filepath: &str) -> Result<bool, Box<dyn Error>> {
    let last_modified = fs::metadata(filepath)?
        .modified()?
        .duration_since(UNIX_EPOCH)?
        .as_secs();

    let Some(last_checked) = *LAST_MODIFIED.lock().unwrap() else {
        *LAST_MODIFIED.lock().unwrap() = Some(last_modified);
        return Ok(true);
    };
    if last_checked <= last_modified {
        *LAST_MODIFIED.lock().unwrap() = Some(last_modified);
        Ok(true)
    } else {
        Ok(false)
    }
}

/// Update [`CRONTAB`] by loading all user crontabs from spool directory
fn sync_cronfile() -> Result<(), Box<dyn Error>> {
    // Check if directory has changed (use directory mtime)
    let dir_changed = is_file_changed(CRON_SPOOL_DIR).unwrap_or(true);

    if (*CRONTAB.lock().unwrap()).is_none() || dir_changed {
        let mut combined_db = Database(vec![]);

        // Load all user crontabs from spool directory
        if let Ok(entries) = fs::read_dir(CRON_SPOOL_DIR) {
            for entry in entries.flatten() {
                // The filename is the username
                let username = entry.file_name().to_string_lossy().into_owned();

                // Look up user info for privilege dropping
                if let Some(user_info) = UserInfo::from_username(&username) {
                    if let Ok(content) = fs::read_to_string(entry.path()) {
                        let user_db = Database::parse_user_crontab(&content, &user_info);
                        combined_db = combined_db.merge(user_db);
                    }
                }
                // If user doesn't exist in passwd, skip their crontab
            }
        }

        // Also load system crontab if exists (6-field format with username)
        if let Ok(content) = fs::read_to_string(SYSTEM_CRONTAB) {
            let sys_db = Database::parse_system_crontab(&content);
            combined_db = combined_db.merge(sys_db);
        }

        *CRONTAB.lock().unwrap() = Some(combined_db);
    }
    Ok(())
}

/// Acquire PID file lock to prevent multiple daemon instances
/// Returns the file handle which must be kept open for the lock to persist
fn acquire_lock() -> Result<File, CronError> {
    let mut file = OpenOptions::new()
        .write(true)
        .create(true)
        .truncate(true)
        .open(PID_FILE)
        .map_err(CronError::PidFile)?;

    // SAFETY: flock() is safe to call here because:
    // 1. file.as_raw_fd() returns a valid file descriptor from an open File
    // 2. LOCK_EX | LOCK_NB are valid flags for exclusive non-blocking lock
    // 3. We check the return value and handle errors appropriately
    unsafe {
        if libc::flock(file.as_raw_fd(), libc::LOCK_EX | libc::LOCK_NB) != 0 {
            return Err(CronError::AlreadyRunning);
        }
    }

    // Write our PID to the file
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

/// Daemon loop
fn daemon_loop() -> Result<(), Box<dyn Error>> {
    loop {
        // Check for shutdown signal
        if SHUTDOWN_FLAG.load(Ordering::SeqCst) {
            return Ok(());
        }

        // Check for reload signal (from SIGHUP)
        if RELOAD_FLAG.swap(false, Ordering::SeqCst) {
            // Force reload by clearing last modified time
            *LAST_MODIFIED.lock().unwrap() = None;
        }

        sync_cronfile()?;
        let Some(db) = CRONTAB.lock().unwrap().clone() else {
            return Err(Box::new(CronError::NoCrontab));
        };
        let Some(x) = db.nearest_job() else {
            sleep(60);
            continue;
        };
        let Some(next_exec) = x.next_execution(&Local::now().naive_local()) else {
            sleep(60);
            continue;
        };
        let now = Local::now();
        let diff = next_exec - now.naive_local();
        let sleep_time = diff.num_seconds().max(0) as u64;

        if sleep_time < 60 {
            sleep(sleep_time as u32);
            let _ = x.run_job(); // Errors logged in child process
        } else {
            sleep(60);
        }
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

    // SAFETY: signal() is safe to call with valid signal numbers and
    // extern "C" function handlers. All handlers are async-signal-safe
    // (they only perform atomic stores or call async-signal-safe functions).
    unsafe {
        libc::signal(libc::SIGHUP, handle_sighup as *const () as usize);
        libc::signal(libc::SIGCHLD, handle_sigchld as *const () as usize);
        libc::signal(libc::SIGTERM, handle_shutdown as *const () as usize);
        libc::signal(libc::SIGINT, handle_shutdown as *const () as usize);
    }

    // Run @reboot jobs at startup
    run_reboot_jobs()?;

    daemon_loop()
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

fn sleep(target: u32) {
    // SAFETY: libc::sleep() is safe with any u32 value; it simply
    // suspends execution for the specified number of seconds.
    unsafe { libc::sleep(target) };
}
