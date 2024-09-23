//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, setlocale, textdomain, LocaleCategory};
use nix::{
    errno::Errno,
    sys::{
        resource::{setrlimit, Resource},
        signal::{
            raise, sigaction, signal, sigprocmask, SaFlags, SigAction, SigHandler, SigSet,
            SigmaskHow,
            Signal::{self, SIGKILL, SIGTERM},
        },
        wait::{waitpid, WaitPidFlag, WaitStatus},
    },
    unistd::{execvp, fork, ForkResult},
};
use plib::PROJECT_NAME;
use std::{
    error::Error,
    ffi::CString,
    os::unix::fs::PermissionsExt,
    path::Path,
    str::FromStr,
    sync::{
        atomic::{AtomicBool, AtomicI32, Ordering},
        Mutex,
    },
    time::Duration,
};

static FOREGROUND: AtomicBool = AtomicBool::new(false);
static FIRST_SIGNAL: AtomicI32 = AtomicI32::new(SIGTERM as i32);
static KILL_AFTER: Mutex<Option<Duration>> = Mutex::new(None);
static MONITORED_PID: AtomicI32 = AtomicI32::new(0);
static TIMED_OUT: AtomicBool = AtomicBool::new(false);

/// timeout — execute a utility with a time limit
#[derive(Parser, Debug)]
#[command(author, version, about, long_about)]
struct Args {
    /// Only time out the utility itself, not its descendants.
    #[arg(short = 'f', long)]
    foreground: bool,

    /// Always preserve (mimic) the wait status of the executed utility, even if the time limit was reached.
    #[arg(short = 'p', long)]
    preserve_status: bool,

    /// Send a SIGKILL signal if the child process created to execute the utility has not terminated after the time period
    /// specified by time has elapsed since the first signal was sent. The value of time shall be interpreted as specified for
    /// the duration operand.
    #[arg(short = 'k', long, value_parser = parse_duration)]
    kill_after: Option<Duration>,

    /// Specify the signal to send when the time limit is reached, using one of the symbolic names defined in the <signal.h> header.
    /// Values of signal shall be recognized in a case-independent fashion, without the SIG prefix. By default, SIGTERM shall be sent.
    #[arg(short = 's', long, default_value = "TERM", value_parser = parse_signal)]
    signal_name: Signal,

    /// The maximum amount of time to allow the utility to run, specified as a decimal number with an optional decimal fraction and an optional suffix.
    #[arg(name = "DURATION", value_parser = parse_duration)]
    duration: Duration,

    /// The name of a utility that is to be executed.
    #[arg(name = "UTILITY")]
    utility: String,

    /// Any string to be supplied as an argument when executing the utility named by the utility operand.
    #[arg(name = "ARGUMENT", trailing_var_arg = true)]
    arguments: Vec<String>,
}

/// Parses string slice into [Duration].
///
/// # Arguments
///
/// * `s` - [str] that represents duration.
///
/// # Errors
///
/// Returns an error if passed invalid input.
///
/// # Returns
///
/// Returns the parsed [Duration] value.
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
/// # Errors
///
/// Returns an error if passed invalid input.
///
/// # Returns
///
/// Returns the parsed [Signal] value.
fn parse_signal(s: &str) -> Result<Signal, String> {
    let s = s.to_uppercase();
    let signal_name = if s.starts_with("SIG") {
        s.to_string()
    } else {
        format!("SIG{s}")
    };

    Signal::from_str(&signal_name).map_err(|_| format!("invalid signal name '{s}'"))
}

/// Starts the timeout after which [Signal::SIGALRM] will be send.
///
/// # Arguments
///
/// * `duration` - [Duration] value of
fn set_timeout(duration: Duration) {
    if !duration.is_zero() {
        unsafe { libc::alarm(duration.as_secs() as libc::c_uint) };
    }
}

/// Sends a signal to the process or process group.
fn send_signal(pid: i32, signal: i32) {
    if pid == 0 {
        unsafe { libc::signal(signal, libc::SIG_IGN) };
    }
    unsafe {
        libc::kill(pid, signal);
    }
}

/// Signal [Signal::SIGCHLD] handler.
extern "C" fn chld_handler(_signal: i32) {}

/// Timeout signal handler.
///
/// # Arguments
///
/// * `signal` - integer value of incoming signal.
extern "C" fn handler(mut signal: i32) {
    // When timeout receives [libc::SIGALRM], this will be considered as timeout reached and
    // timeout will send prepared signal
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

/// Unblocks incoming signal by adding it to empty signals mask.
///
/// # Arguments
///
/// `signal` - signal of type [Signal] that needs to be unblocked.
fn unblock_signal(signal: Signal) {
    let mut sig_set = SigSet::empty();
    sig_set.add(signal);
    if sigprocmask(SigmaskHow::SIG_UNBLOCK, Some(&sig_set), None).is_err() {
        eprintln!("timeout: failed to set unblock signals mask");
        std::process::exit(125)
    }
}

/// Installs handler for [Signal::SIGCHLD] signal to receive child's exit status code from parent (timeout).
fn set_chld() {
    let handler = SigHandler::Handler(chld_handler);
    let flags = SaFlags::SA_RESTART;
    let mask = SigSet::empty();
    let sa = SigAction::new(handler, flags, mask);

    unsafe {
        let _ = sigaction(Signal::SIGCHLD, &sa);
    };

    unblock_signal(Signal::SIGCHLD);
}

/// Installs handler ([handler]) for incoming [Signal] and other signals.
///
/// # Arguments
///
/// `signal` - signal of type [Signal] that needs to be handled.
fn set_handler(signal: Signal) {
    let handler = SigHandler::Handler(handler);
    let flags = SaFlags::SA_RESTART;
    let mask = SigSet::empty();
    let sa = SigAction::new(handler, flags, mask);

    unsafe {
        let _ = sigaction(Signal::SIGALRM, &sa);
        let _ = sigaction(Signal::SIGINT, &sa);
        let _ = sigaction(Signal::SIGQUIT, &sa);
        let _ = sigaction(Signal::SIGHUP, &sa);
        let _ = sigaction(Signal::SIGTERM, &sa);
        let _ = sigaction(signal, &sa);
    }
}

/// Blocks incoming signal and stores previous signals mask.
///
/// # Arguments
///
/// `signal` - signal of type [Signal] that needs to be handled.
///
/// `old_set` - mutable reference to set of gidnals of type [SigSet] into which will be placed previous mask.
fn block_handler_and_chld(signal: Signal, old_set: &mut SigSet) {
    let mut block_set = SigSet::empty();

    block_set.add(Signal::SIGALRM);
    block_set.add(Signal::SIGINT);
    block_set.add(Signal::SIGQUIT);
    block_set.add(Signal::SIGHUP);
    block_set.add(Signal::SIGTERM);
    block_set.add(signal);

    block_set.add(Signal::SIGCHLD);

    if sigprocmask(SigmaskHow::SIG_BLOCK, Some(&block_set), Some(old_set)).is_err() {
        eprintln!("timeout: failed to set block signals mask");
        std::process::exit(125)
    }
}

/// Tries to disable core dumps for current process.
///
/// # Returns
///
/// `true` is successfull, `false` otherwise.
fn disable_core_dumps() -> bool {
    #[cfg(target_os = "linux")]
    if nix::sys::prctl::set_dumpable(false).is_ok() {
        return true;
    }
    if setrlimit(Resource::RLIMIT_CORE, 0, 0).is_ok() {
        return true;
    }
    false
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
/// `args` - structure of timeout options and operands.
///
/// # Return
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
        mut arguments,
    } = args;

    FOREGROUND.store(foreground, Ordering::SeqCst);
    FIRST_SIGNAL.store(signal_name as i32, Ordering::SeqCst);
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
    unblock_signal(Signal::SIGALRM);

    let mut sig_set = SigSet::empty();
    block_handler_and_chld(signal_name, &mut sig_set);

    match unsafe { fork() } {
        Ok(ForkResult::Child) => {
            // Restore original mask for child.= process.
            let _ = sigprocmask(SigmaskHow::SIG_SETMASK, Some(&sig_set), None);

            unsafe {
                libc::signal(libc::SIGTTIN, libc::SIG_DFL);
                libc::signal(libc::SIGTTOU, libc::SIG_DFL);
            }

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

            let utility_c = CString::new(utility_path.clone()).unwrap();
            let mut arguments_c: Vec<CString> = arguments
                .drain(..)
                .map(|arg| CString::new(arg).unwrap())
                .collect();
            arguments_c.insert(0, utility_c.clone());
            match execvp(&utility_c, &arguments_c) {
                Ok(_) => 0,
                Err(Errno::ENOENT) => {
                    eprintln!("timeout: utility '{utility}' not found");
                    127
                }
                Err(_) => {
                    eprintln!("timeout: unable to run the utility '{utility}'");
                    126
                }
            }
        }
        Ok(ForkResult::Parent { child }) => {
            MONITORED_PID.store(child.as_raw(), Ordering::SeqCst);

            set_timeout(duration);

            let mut wait_status: WaitStatus;
            loop {
                match waitpid(
                    child,
                    Some(WaitPidFlag::WNOHANG | WaitPidFlag::WCONTINUED | WaitPidFlag::WUNTRACED),
                ) {
                    Ok(ws) => wait_status = ws,
                    Err(_) => {
                        eprintln!("timeout: failed to wait for child");
                        return 125;
                    }
                }
                match wait_status {
                    WaitStatus::StillAlive | WaitStatus::Continued(_) => {
                        let _ = sig_set.suspend();
                    }
                    WaitStatus::Stopped(_, _s) => {
                        send_signal(MONITORED_PID.load(Ordering::SeqCst), libc::SIGCONT);
                        TIMED_OUT.store(true, Ordering::SeqCst);
                    }
                    _ => {
                        break;
                    }
                }
            }
            let status = match wait_status {
                WaitStatus::Exited(_, status) => status,
                WaitStatus::Signaled(_, rec_signal, _) => {
                    if !TIMED_OUT.load(Ordering::SeqCst) && disable_core_dumps() {
                        let _ = unsafe { signal(rec_signal, SigHandler::SigDfl) };
                        unblock_signal(rec_signal);
                        let _ = raise(rec_signal);
                    }
                    if TIMED_OUT.load(Ordering::SeqCst) && rec_signal == SIGKILL {
                        preserve_status = true;
                    }
                    128 + rec_signal as i32
                }
                _ => 125,
            };

            if TIMED_OUT.load(Ordering::SeqCst) && !preserve_status {
                124
            } else {
                status
            }
        }
        Err(_) => 125,
    }
}

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
