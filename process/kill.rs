//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use gettextrs::{bind_textdomain_codeset, setlocale, textdomain, LocaleCategory};
use posixutils_process::signal::{list_signals, lookup_signum, signum_to_name};

enum ConfigMode {
    Signal(i32),
    List,
    ListExit(i32),
}

struct Config {
    mode: ConfigMode,
    pids: Vec<i32>,
}

fn parse_cmdline() -> Result<Config, &'static str> {
    let mut pids = Vec::new();
    let mut mode = ConfigMode::Signal(libc::SIGTERM);
    let mut in_args = true;
    let mut in_s_arg = false;
    let mut in_l_arg = false;
    for arg in std::env::args().skip(1) {
        if in_args {
            if in_s_arg {
                let sig_no = lookup_signum(&arg)?;
                mode = ConfigMode::Signal(sig_no);
                in_s_arg = false;
            } else if in_l_arg {
                // -l with exit_status argument
                match arg.parse::<i32>() {
                    Ok(exit_status) => {
                        mode = ConfigMode::ListExit(exit_status);
                        in_l_arg = false;
                    }
                    Err(_) => {
                        return Err("Invalid exit status");
                    }
                }
            } else if arg == "-s" || arg == "--signal" {
                in_s_arg = true;
            } else if arg == "-l" || arg == "--list" {
                mode = ConfigMode::List;
                in_l_arg = true; // Check if next arg is exit_status
            } else if arg == "--" {
                in_args = false;
                in_l_arg = false;
            } else if let Some(st) = arg.strip_prefix("-") {
                in_l_arg = false;
                let sig_no: i32 = match st.parse() {
                    Ok(signo) => signo,
                    Err(_) => lookup_signum(st)?,
                };
                mode = ConfigMode::Signal(sig_no);
            } else {
                in_args = false;
                in_l_arg = false;
            }

            if in_args || arg == "--" {
                continue;
            }

            // fall through to process non-option arguments
        }

        match arg.parse::<i32>() {
            Ok(pid) => pids.push(pid),
            Err(_) => {
                return Err("Invalid PID");
            }
        }
    }

    // Validate: signal mode requires at least one PID
    if let ConfigMode::Signal(_) = mode {
        if pids.is_empty() {
            return Err("No process ID specified");
        }
    }

    Ok(Config { mode, pids })
}

fn send_signal(prog_cfg: &Config, sig_no: i32) -> u32 {
    let mut exit_code = 0;

    for pid in &prog_cfg.pids {
        let res = unsafe { libc::kill(*pid as libc::pid_t, sig_no) };
        if res != 0 {
            let err = std::io::Error::last_os_error();
            eprintln!("kill: {}: {}", pid, err);
            exit_code = 1;
        }
    }

    exit_code
}

fn list_exit_status(exit_status: i32) -> u32 {
    match signum_to_name(exit_status) {
        Some(name) => {
            println!("{}", name);
            0
        }
        None => {
            eprintln!("kill: {}: invalid signal number", exit_status);
            1
        }
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let prog_cfg = parse_cmdline()?;

    let exit_code = match prog_cfg.mode {
        ConfigMode::List => {
            list_signals();
            0
        }
        ConfigMode::ListExit(exit_status) => list_exit_status(exit_status),
        ConfigMode::Signal(sig_no) => send_signal(&prog_cfg, sig_no),
    };

    std::process::exit(exit_code as i32)
}
