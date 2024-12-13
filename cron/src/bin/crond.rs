//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use chrono::Local;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use posixutils_cron::job::Database;
use std::env;
use std::error::Error;
use std::fs;
use std::str::FromStr;

fn parse_cronfile(username: &str) -> Result<Database, Box<dyn Error>> {
    #[cfg(target_os = "linux")]
    let file = format!("/var/spool/cron/{username}");
    #[cfg(target_os = "macos")]
    let file = format!("/var/at/tabs/{username}");
    let s = fs::read_to_string(&file)?;
    Ok(s.lines()
        .filter_map(|x| Database::from_str(x).ok())
        .fold(Database(vec![]), |acc, next| acc.merge(next)))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let Ok(logname) = env::var("LOGNAME") else {
        panic!("Could not obtain the user's logname.")
    };

    // Daemon setup
    unsafe {
        use libc::*;

        let pid = fork();
        if pid > 0 {
            return Ok(());
        }

        setsid();
        chdir(b"/\0" as *const _ as *const c_char);

        close(STDIN_FILENO);
        close(STDOUT_FILENO);
        close(STDERR_FILENO);
    }

    // Daemon code

    loop {
        let db = parse_cronfile(&logname)?;
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

fn sleep(target: u32) {
    unsafe { libc::sleep(target) };
}
