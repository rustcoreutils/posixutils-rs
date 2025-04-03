//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::collections::HashSet;
use std::ffi::{CStr, CString};
use std::fs::{self};
use std::io::{BufRead, Read, Seek, Write};
use std::os::unix::fs::PermissionsExt;
use std::path::{Path, PathBuf};
use std::{env, process};

use chrono::{DateTime, Local, TimeZone, Utc};
use gettextrs::{bind_textdomain_codeset, setlocale, textdomain, LocaleCategory};
use libc::{getlogin, getpwnam, passwd};

#[cfg(target_os = "linux")]
const SPOOL_DIRECTORIES: &[&str] = &[
    "/var/spool/cron/atjobs/",
    "/var/spool/at/",
    "/var/spool/atjobs/",
];

#[cfg(target_os = "macos")]
const MACOS_DIRECTORY: &str = "/var/at/jobs/";

#[cfg(target_os = "linux")]
const DEFAULT_DIRECTORY: &str = "/var/spool/atjobs/";

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let time = {
        let datetime_utc = Utc::now();
        let datetime_local = datetime_utc.with_timezone(&Local);
        Utc.from_local_datetime(&datetime_local.naive_local())
            .unwrap()
    };

    let cmd = {
        let stdout = std::io::stdout();
        let mut stdout_lock = stdout.lock();

        writeln!(&mut stdout_lock, "at {}", time.to_rfc2822())?;
        write!(&mut stdout_lock, "at> ")?;
        stdout_lock.flush()?;

        let stdin = std::io::stdin();
        let mut stdin_lock = stdin.lock();

        let mut result = Vec::new();
        let mut buf = String::new();

        while stdin_lock.read_line(&mut buf)? != 0 {
            write!(&mut stdout_lock, "at> ")?;
            stdout_lock.flush()?;

            result.push(buf.to_owned());
        }

        writeln!(&mut stdout_lock, "<EOT>")?;
        stdout_lock.flush()?;

        result.join("\n")
    };

    let _ = at(Some('b'), &time, cmd, true).inspect_err(|err| print_err_and_exit(1, err));

    Ok(())
}

/// Returns the path to the jobs directory, adjusted for the operating system.
/// On Linux: checks the `AT_JOB_DIR` environment variable, then predefined directories.
/// On macOS: checks or creates the `/var/at/jobs` directory.
fn get_job_dir() -> Result<String, String> {
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

fn print_err_and_exit(exit_code: i32, err: impl std::fmt::Display) -> ! {
    eprintln!("{}", err);
    process::exit(exit_code)
}

fn at(
    queue: Option<char>,
    execution_time: &DateTime<Utc>,
    cmd: impl Into<String>,
    mail: bool,
) -> Result<(), Box<dyn std::error::Error>> {
    let jobno = next_job_id()?;
    let job_filename = job_file_name(jobno, queue, execution_time)
        .ok_or("Failed to generate file name for job")?;

    let user = User::new().ok_or("Failed to get current user")?;
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

    println!(
        "job {} at {}",
        jobno,
        execution_time.format("%a %b %d %H:%M:%S %Y")
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

        let env = env
            .into_iter()
            .map(|(key, value)| format!("{}={}; export {}", key, value, key))
            .collect::<Vec<_>>()
            .join("\n");

        format!(
            "#!{shell}\n# atrun uid={user_uid} gid={user_gid}\n# mail {user_name} {}\numask 22\n{env}\ncd {} || {{\n\techo 'Execution directory inaccessible' >&2\n\texit 1 \n}}\n{cmd}",
            if mail {1} else {0},
            call_place.to_string_lossy()
        )
    }
}

/// Return name for job number
///
/// None if DateTime < [DateTime::UNIX_EPOCH]
fn job_file_name(next_job: u32, queue: Option<char>, time: &DateTime<Utc>) -> Option<String> {
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

fn next_job_id() -> Result<u32, Box<dyn std::error::Error>> {
    let mut file_opt = std::fs::OpenOptions::new();
    file_opt.read(true).write(true);

    let mut buf = String::new();
    let job_file_number = format!("{}.SEQ", get_job_dir()?);

    let (next_job_id, mut file) = match file_opt.open(&job_file_number) {
        Ok(mut file) => {
            file.read_to_string(&mut buf)
                .map_err(|e| NextJobError::Io(e))?;
            file.rewind().map_err(|e| NextJobError::Io(e))?;

            (
                u32::from_str_radix(buf.trim_end_matches("\n"), 16)
                    .map_err(|e| NextJobError::FromStr(e))?,
                file,
            )
        }
        Err(err) => match err.kind() {
            std::io::ErrorKind::NotFound => (
                0,
                std::fs::File::create_new(job_file_number).map_err(NextJobError::Io)?,
            ),

            _ => Err(NextJobError::Io(err))?,
        },
    };

    // Limit range of jobs to 2^20 jobs
    let next_job_id = (1 + next_job_id) % 0xfffff;

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

fn is_user_allowed(user: &str) -> bool {
    let allow_file = "/etc/at.allow";
    let deny_file = "/etc/at.deny";

    if let Ok(allowed_users) = read_user_file(allow_file) {
        // If at.allow exists, only users from this file have access
        return allowed_users.contains(user);
    }

    if let Ok(denied_users) = read_user_file(deny_file) {
        // If there is no at.allow, but there is at.deny, check if the user is blacklisted
        return !denied_users.contains(user);
    }

    // If there are no files, access is allowed to all
    true
}

fn login_name() -> Option<String> {
    // Try to get the login name using getlogin
    unsafe {
        let login_ptr = getlogin();
        if !login_ptr.is_null() {
            if let Ok(c_str) = CStr::from_ptr(login_ptr).to_str() {
                return Some(c_str.to_string());
            }
        }
    }

    // Fall back to checking the LOGNAME environment variable
    env::var("LOGNAME").ok()
}

pub struct User {
    pub name: String,
    pub shell: String,
    pub uid: u32,
    pub gid: u32,
}

impl User {
    pub fn new() -> Option<Self> {
        const DEFAULT_SHELL: &str = "/bin/sh";

        let login_name = login_name()?;

        let passwd {
            pw_uid,
            pw_gid,
            pw_shell,
            ..
        } = user_info_by_name(&login_name)?;

        let pw_shell = match pw_shell.is_null() {
            true => std::env::var("SHELL")
                .ok()
                .unwrap_or(DEFAULT_SHELL.to_owned()),
            false => unsafe {
                CStr::from_ptr(pw_shell)
                    .to_str()
                    .ok()
                    .unwrap_or(DEFAULT_SHELL)
                    .to_owned()
            },
        };

        Some(Self {
            shell: pw_shell,
            uid: pw_uid,
            gid: pw_gid,
            name: login_name,
        })
    }
}

fn user_info_by_name(name: &str) -> Option<passwd> {
    let c_name = CString::new(name).unwrap();
    let pw_ptr = unsafe { getpwnam(c_name.as_ptr()) };
    if pw_ptr.is_null() {
        None
    } else {
        Some(unsafe { *pw_ptr })
    }
}
