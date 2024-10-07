//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use plib::PROJECT_NAME;
use std::{
    error::Error,
    os::unix::{
        fs::PermissionsExt,
        process::{CommandExt, ExitStatusExt},
    },
    path::Path,
    process::{Command, ExitStatus},
    sync::{
        atomic::{AtomicBool, AtomicI32, Ordering},
        Mutex,
    },
    time::Duration,
};

#[cfg(target_os = "macos")]
const SIGLIST: [(&str, i32); 31] = [
    ("HUP", 1),
    ("INT", 2),
    ("QUIT", 3),
    ("ILL", 4),
    ("TRAP", 5),
    ("ABRT", 6),
    ("EMT", 7),
    ("FPE", 8),
    ("KILL", 9),
    ("BUS", 10),
    ("SEGV", 11),
    ("SYS", 12),
    ("PIPE", 13),
    ("ALRM", 14),
    ("TERM", 15),
    ("URG", 16),
    ("STOP", 17),
    ("TSTP", 18),
    ("CONT", 19),
    ("CHLD", 20),
    ("TTIN", 21),
    ("TTOU", 22),
    ("IO", 23),
    ("XCPU", 24),
    ("XFSZ", 25),
    ("VTALRM", 26),
    ("PROF", 27),
    ("WINCH", 28),
    ("INFO", 29),
    ("USR1", 30),
    ("USR2", 31),
];

#[cfg(target_os = "linux")]
const SIGLIST: [(&str, i32); 32] = [
    ("HUP", 1),
    ("INT", 2),
    ("QUIT", 3),
    ("ILL", 4),
    ("TRAP", 5),
    ("ABRT", 6),
    ("IOT", 6),
    ("BUS", 7),
    ("FPE", 8),
    ("KILL", 9),
    ("USR1", 10),
    ("SEGV", 11),
    ("USR2", 12),
    ("PIPE", 13),
    ("ALRM", 14),
    ("TERM", 15),
    ("STKFLT", 16),
    ("CHLD", 17),
    ("CONT", 18),
    ("STOP", 19),
    ("TSTP", 20),
    ("TTIN", 21),
    ("TTOU", 22),
    ("URG", 23),
    ("XCPU", 24),
    ("XFSZ", 25),
    ("VTALRM", 26),
    ("PROF", 27),
    ("WINCH", 28),
    ("IO", 29),
    ("PWR", 30),
    ("SYS", 31),
];

static FOREGROUND: AtomicBool = AtomicBool::new(false);
static FIRST_SIGNAL: AtomicI32 = AtomicI32::new(libc::SIGTERM);
static KILL_AFTER: Mutex<Option<Duration>> = Mutex::new(None);
static MONITORED_PID: AtomicI32 = AtomicI32::new(0);
static TIMED_OUT: AtomicBool = AtomicBool::new(false);

#[derive(Parser)]
#[command(version, about = gettext("timeout — execute a utility with a time limit"))]
struct Args {
    #[arg(short = 'f', long, help=gettext("Only time out the utility itself, not its descendants."))]
    foreground: bool,

    #[arg(short = 'p', long, help=gettext("Preserve the exit status of the utility."))]
    preserve_status: bool,

    #[arg(short = 'k', long, value_parser = parse_duration, help=gettext("Send a SIGKILL signal if the child process has not terminated after the time period."))]
    kill_after: Option<Duration>,

    #[arg(short = 's', long, default_value = "TERM", value_parser = parse_signal, help=gettext("Specify the signal to send when the time limit is reached."))]
    signal_name: i32,

    #[arg(name = "DURATION", value_parser = parse_duration, help=gettext("The maximum amount of time to allow the utility to run, specified as a decimal number with an optional decimal fraction and an optional suffix."))]
    duration: Duration,

    #[arg(name = "UTILITY", help=gettext("The utility to execute."))]
    utility: String,

    #[arg(name = "ARGUMENT", trailing_var_arg = true, help=gettext("Arguments to pass to the utility."))]
    arguments: Vec<String>,
}

/// Parses string slice into [Duration].
///
/// # Arguments
///
/// * `s` - [str] that represents duration.
///
/// # Returns
///
/// Returns the parsed [Duration] value.
///
/// # Errors
///
/// Returns a [String] error if passed invalid duration string.
fn parse_duration(s: &str) -> Result<Duration, String> {
    let (value, suffix) = s.split_at(
        s.find(|c: char| !c.is_ascii_digit() && c != '.')
            .unwrap_or(s.len()),
    );

    let value: f64 = value
        .parse()
        .map_err(|_| format!("invalid duration format '{s}'"))?;

    let multiplier = match suffix {
        "s" | "" => 1.0,
        "m" => 60.0,
        "h" => 3600.0,
        "d" => 86400.0,
        _ => return Err(format!("invalid duration format '{s}'")),
    };

    Ok(Duration::from_secs_f64(value * multiplier))
}

/// Parses [str] into [Signal].
///
/// # Arguments
///
/// * `s` - [str] that represents the signal name.
///
/// # Returns
///
/// Returns the parsed [Signal] value.
///
/// # Errors
///
/// Returns an [String] error if passed invalid signal name.
fn parse_signal(s: &str) -> Result<i32, String> {
    let normalized = s.trim().to_uppercase();
    let normalized = normalized.strip_prefix("SIG").unwrap_or(&normalized);

    for (name, num) in SIGLIST.iter() {
        if name == &normalized {
            return Ok(*num);
        }
    }
    Err(format!("invalid signal name '{s}'"))
}

/// Starts the timeout after which [libc::SIGALRM] will be send.
///
/// # Arguments
///
/// * `duration` - [Duration] value of time until alarm.
fn set_timeout(duration: Duration) {
    if !duration.is_zero() {
        unsafe { libc::alarm(duration.as_secs() as libc::c_uint) };
    }
}

/// Sends a signal to the process or process group.
///
/// # Arguments:
///
/// * `pid` - [i32] value of PID.
/// * `signal` - [i32] value of signal tat must be sent.
fn send_signal(pid: i32, signal: i32) {
    if pid == 0 {
        unsafe { libc::signal(signal, libc::SIG_IGN) };
    }
    unsafe {
        libc::kill(pid, signal);
    }
}

/// Signal [libc::SIGCHLD] handler.
extern "C" fn chld_handler(_signal: i32) {}

/// Timeout signal handler.
///
/// # Arguments
///
/// * `signal` - integer value of incoming signal.
extern "C" fn handler(mut signal: i32) {
    // When timeout receives [libc::SIGALRM], this will be considered as timeout reached and
    // timeout will send prepared signal.
    if signal == libc::SIGALRM {
        TIMED_OUT.store(true, Ordering::SeqCst);
        signal = FIRST_SIGNAL.load(Ordering::SeqCst);
    }
    match MONITORED_PID.load(Ordering::SeqCst).cmp(&0) {
        std::cmp::Ordering::Less => {}
        std::cmp::Ordering::Equal => std::process::exit(128 + signal),
        std::cmp::Ordering::Greater => {
            let mut kill_after = KILL_AFTER.lock().unwrap();
            if let Some(duration) = *kill_after {
                FIRST_SIGNAL.store(libc::SIGKILL, Ordering::SeqCst);
                set_timeout(duration);
                *kill_after = None;
            }

            // Propagating incoming signal
            send_signal(MONITORED_PID.load(Ordering::SeqCst), signal);

            if !FOREGROUND.load(Ordering::SeqCst) {
                send_signal(0, signal);
                if signal != libc::SIGKILL && signal != libc::SIGCONT {
                    send_signal(MONITORED_PID.load(Ordering::SeqCst), libc::SIGCONT);
                    send_signal(0, libc::SIGCONT);
                }
            }
        }
    }
}

/// Returns empty set of signals.
///
/// # Returns:
///
/// Returns [libc::sigset_t] empty set of signal.
fn get_empty_sig_set() -> libc::sigset_t {
    let mut sig_set = std::mem::MaybeUninit::uninit();
    let _ = unsafe { libc::sigemptyset(sig_set.as_mut_ptr()) };
    unsafe { sig_set.assume_init() }
}

/// Unblocks incoming signal by adding it to empty signals mask.
///
/// # Arguments
///
/// `signal` - [i32] value of signal that needs to be unblocked.
fn unblock_signal(signal: i32) {
    unsafe {
        let mut sig_set = get_empty_sig_set();

        libc::sigaddset(&mut sig_set, signal);
        if libc::sigprocmask(
            libc::SIG_UNBLOCK,
            &sig_set,
            std::ptr::null_mut::<libc::sigset_t>(),
        ) != 0
        {
            eprintln!("timeout: failed to set unblock signals mask");
            std::process::exit(125)
        }
    }
}

/// Installs handler for [libc::SIGCHLD] signal to receive child's exit status code from parent (timeout).
fn set_chld() {
    unsafe {
        let mut sig_action = std::mem::MaybeUninit::<libc::sigaction>::uninit();
        let p_sa = sig_action.as_mut_ptr();
        (*p_sa).sa_sigaction = chld_handler as *const extern "C" fn(libc::c_int) as usize;
        (*p_sa).sa_flags = libc::SA_RESTART;
        libc::sigemptyset(&mut (*p_sa).sa_mask);
        let sig_action = sig_action.assume_init();

        libc::sigaction(
            libc::SIGCHLD,
            &sig_action,
            std::ptr::null_mut::<libc::sigaction>(),
        );
    }

    unblock_signal(libc::SIGCHLD);
}

/// Installs handler ([handler]) for incoming signal and other signals.
///
/// # Arguments
///
/// `signal` - [i32] value of signal that needs to be handled.
fn set_handler(signal: i32) {
    unsafe {
        let mut sig_action = std::mem::MaybeUninit::<libc::sigaction>::uninit();
        let p_sa = sig_action.as_mut_ptr();
        (*p_sa).sa_sigaction = handler as *const extern "C" fn(libc::c_int) as usize;
        (*p_sa).sa_flags = libc::SA_RESTART;
        libc::sigemptyset(&mut (*p_sa).sa_mask);
        let sig_action = sig_action.assume_init();

        libc::sigaction(
            libc::SIGALRM,
            &sig_action,
            std::ptr::null_mut::<libc::sigaction>(),
        );
        libc::sigaction(
            libc::SIGINT,
            &sig_action,
            std::ptr::null_mut::<libc::sigaction>(),
        );
        libc::sigaction(
            libc::SIGQUIT,
            &sig_action,
            std::ptr::null_mut::<libc::sigaction>(),
        );
        libc::sigaction(
            libc::SIGHUP,
            &sig_action,
            std::ptr::null_mut::<libc::sigaction>(),
        );
        libc::sigaction(
            libc::SIGTERM,
            &sig_action,
            std::ptr::null_mut::<libc::sigaction>(),
        );
        libc::sigaction(signal, &sig_action, std::ptr::null_mut::<libc::sigaction>());
    }
}

/// Blocks incoming signal and stores previous signals mask.
///
/// # Arguments
///
/// `signal` - [i32] value of signal that needs to be handled.
/// `old_set` - [libc::sigset_t] mutable reference to set of signals into which will be placed previous mask.
fn block_handler_and_chld(signal: i32, old_set: &mut libc::sigset_t) {
    unsafe {
        let mut block_set = get_empty_sig_set();

        libc::sigaddset(&mut block_set, libc::SIGALRM);
        libc::sigaddset(&mut block_set, libc::SIGINT);
        libc::sigaddset(&mut block_set, libc::SIGQUIT);
        libc::sigaddset(&mut block_set, libc::SIGHUP);
        libc::sigaddset(&mut block_set, libc::SIGTERM);
        libc::sigaddset(&mut block_set, signal);

        libc::sigaddset(&mut block_set, libc::SIGCHLD);

        if libc::sigprocmask(libc::SIG_BLOCK, &block_set, old_set) != 0 {
            eprintln!("timeout: failed to set block signals mask");
            std::process::exit(125)
        }
    }
}

/// Tries to disable core dumps for current process.
///
/// # Returns
///
/// `true` is successfull, `false` otherwise.
fn disable_core_dumps() -> bool {
    #[cfg(target_os = "linux")]
    if unsafe { libc::prctl(libc::PR_SET_DUMPABLE, 0) } == 0 {
        return true;
    }
    let rlim = libc::rlimit {
        rlim_cur: 0,
        rlim_max: 0,
    };
    (unsafe { libc::setrlimit(libc::RLIMIT_CORE, &rlim) } == 0)
}

/// Searches for the executable utility in the directories specified by the `PATH` environment variable.
///
/// # Arguments
///
/// * `utility` - name of the utility to search for.
///
/// # Returns
///
/// `Option<String>` - full path to the utility if found, or `None` if not found.
fn search_in_path(utility: &str) -> Option<String> {
    if let Ok(paths) = std::env::var("PATH") {
        for path in paths.split(':') {
            let full_path = std::path::Path::new(path).join(utility);
            if full_path.is_file() {
                if let Ok(metadata) = std::fs::metadata(&full_path) {
                    // Check if the file is executable
                    if metadata.permissions().mode() & 0o111 != 0 {
                        return Some(full_path.to_string_lossy().into_owned());
                    }
                }
            }
        }
    }
    None
}

/// Main timeout function that creates child and processes its return exit status.
///
/// # Arguments
///
/// `args` - [Args] structure of timeout options and operands.
///
/// # Returns
///
/// [i32] - exit status code of timeout utility.
fn timeout(args: Args) -> i32 {
    let Args {
        foreground,
        mut preserve_status,
        kill_after,
        signal_name,
        duration,
        utility,
        arguments,
    } = args;

    let utility_path = if Path::new(&utility).is_file() {
        utility.clone()
    } else {
        match search_in_path(&utility) {
            Some(path) => path,
            None => {
                eprintln!("timeout: utility '{utility}' not found");
                return 127;
            }
        }
    };

    FOREGROUND.store(foreground, Ordering::SeqCst);
    FIRST_SIGNAL.store(signal_name, Ordering::SeqCst);
    *KILL_AFTER.lock().unwrap() = kill_after;

    // Ensures, this process is process leader so all subprocesses can be killed.s
    if !foreground {
        unsafe { libc::setpgid(0, 0) };
    }

    // Setup handlers before to catch signals before fork()
    set_handler(signal_name);
    unsafe {
        libc::signal(libc::SIGTTIN, libc::SIG_IGN);
        libc::signal(libc::SIGTTOU, libc::SIG_IGN);
    }
    set_chld();

    // To be able to handle SIGALRM (will be send after timeout)
    unblock_signal(libc::SIGALRM);

    let mut original_set = get_empty_sig_set();
    block_handler_and_chld(signal_name, &mut original_set);

    let spawn_result = unsafe {
        Command::new(&utility_path)
            .args(arguments)
            .pre_exec(move || {
                libc::sigprocmask(
                    libc::SIG_SETMASK,
                    &original_set,
                    std::ptr::null_mut::<libc::sigset_t>(),
                );

                libc::signal(libc::SIGTTIN, libc::SIG_DFL);
                libc::signal(libc::SIGTTOU, libc::SIG_DFL);
                Ok(())
            })
            .spawn()
    };
    let child = match spawn_result {
        Ok(child) => child,
        Err(err) => match err.kind() {
            std::io::ErrorKind::NotFound => {
                eprintln!("timeout: utility '{utility}' not found");
                return 127;
            }
            std::io::ErrorKind::PermissionDenied => {
                eprintln!("timeout: unable to run the utility '{utility}'");
                return 126;
            }
            _ => return 125,
        },
    };

    MONITORED_PID.store(child.id() as i32, Ordering::SeqCst);

    set_timeout(duration);

    let mut wait_status;
    let mut status = 0;
    let options = libc::WNOHANG | libc::WCONTINUED | libc::WUNTRACED;

    loop {
        wait_status =
            unsafe { libc::waitpid(MONITORED_PID.load(Ordering::SeqCst), &mut status, options) };

        let es = ExitStatus::from_raw(status);
        if wait_status == 0 || es.continued() {
            unsafe { libc::sigsuspend(&original_set) };
        } else if es.stopped_signal().is_some() {
            send_signal(MONITORED_PID.load(Ordering::SeqCst), libc::SIGCONT);
            TIMED_OUT.store(true, Ordering::SeqCst);
        } else {
            break;
        }
    }

    if wait_status < 0 {
        eprintln!("timeout: failed to wait for child");
        125
    } else {
        status = if libc::WIFEXITED(status) {
            libc::WEXITSTATUS(status)
        } else if libc::WIFSIGNALED(status) {
            let signal = libc::WTERMSIG(status);
            if libc::WCOREDUMP(status) {
                eprintln!("timeout: monitored command dumped core");
                return 125;
            }
            if !TIMED_OUT.load(Ordering::SeqCst) && disable_core_dumps() {
                unsafe { libc::signal(signal, libc::SIG_DFL) };
                unblock_signal(signal);
                unsafe { libc::raise(signal) };
            }
            if TIMED_OUT.load(Ordering::SeqCst) && signal as i32 == libc::SIGKILL {
                preserve_status = true;
            }
            128 + signal
        } else {
            eprintln!("timeout: unknown status from commnad: {status}");
            return 125;
        };

        if TIMED_OUT.load(Ordering::SeqCst) && !preserve_status {
            124
        } else {
            status
        }
    }
}

/// Exit code:
///     124 - Process timed out.
///     125 - An error other than the two described below occurred.
///     126 - The utility specified by utility was found but could not be executed.
///     127 - The utility specified by utility could not be found.
fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Parse command line arguments
    let args = Args::try_parse().unwrap_or_else(|err| match err.kind() {
        clap::error::ErrorKind::DisplayHelp | clap::error::ErrorKind::DisplayVersion => {
            print!("{err}");
            std::process::exit(0);
        }
        _ => {
            eprintln!(
                "timeout: {}",
                err.source()
                    .map_or_else(|| err.kind().to_string(), |err| err.to_string())
            );
            std::process::exit(125);
        }
    });

    setlocale(LocaleCategory::LcAll, "");
    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    let exit_code = timeout(args);
    std::process::exit(exit_code);
}
