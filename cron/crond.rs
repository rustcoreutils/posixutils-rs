//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use chrono::Local;
use cron::job::Database;
use gettextrs::{bind_textdomain_codeset, setlocale, textdomain, LocaleCategory};
use std::cmp::Ordering::{Greater, Less};
use std::env;
use std::error::Error;
use std::fmt;
use std::fs;
use std::str::FromStr;
use std::sync::Mutex;
use std::time::UNIX_EPOCH;

static CRONTAB: Mutex<Option<Database>> = Mutex::new(None);
static LAST_MODIFIED: Mutex<Option<u64>> = Mutex::new(None);

#[derive(Debug)]
enum CronError {
    Fork,
    NoLogname,
    NoCrontab,
}

impl Error for CronError {}

impl fmt::Display for CronError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::NoLogname => write!(f, "Could not obtain the user's logname"),
            Self::NoCrontab => write!(f, "Could not format database"),
            Self::Fork => write!(f, "Could not create child process"),
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

/// Update [`CRONTAB`] if logname file is changed
fn sync_cronfile() -> Result<(), Box<dyn Error>> {
    let Ok(logname) = env::var("LOGNAME") else {
        return Err(Box::new(CronError::NoLogname));
    };
    #[cfg(target_os = "linux")]
    let file = format!("/var/spool/cron/{logname}");
    #[cfg(target_os = "macos")]
    let file = format!("/var/at/tabs/{logname}");
    if (*CRONTAB.lock().unwrap()).is_none() || is_file_changed(&file)? {
        let s = fs::read_to_string(&file)?;
        let crontab = s
            .lines()
            .filter_map(|x| Database::from_str(x).ok())
            .fold(Database(vec![]), |acc, next| acc.merge(next));
        *CRONTAB.lock().unwrap() = Some(crontab);
    }
    Ok(())
}

/// Create new daemon process of crond
fn setup() -> i32 {
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

/// Handles incoming signals
fn handle_signals(signal_code: libc::c_int) {
    if signal_code == libc::SIGHUP {
        if let Err(err) = sync_cronfile() {
            eprintln!("{err}");
            std::process::exit(1);
        }
    }
}

/// Daemon loop
fn daemon_loop() -> Result<(), Box<dyn Error>> {
    loop {
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
        let diff = now.naive_local() - next_exec;
        let sleep_time = diff.num_seconds();

        if sleep_time < 60 {
            sleep(sleep_time as u32);
            x.run_job()?;
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

    unsafe {
        libc::signal(libc::SIGHUP, handle_signals as usize);
    }

    daemon_loop()
}

fn sleep(target: u32) {
    unsafe { libc::sleep(target) };
}
