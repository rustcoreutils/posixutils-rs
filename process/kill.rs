//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

mod signal;

use gettextrs::{bind_textdomain_codeset, setlocale, textdomain, LocaleCategory};

use crate::signal::{list_signals, lookup_signum};

enum ConfigMode {
    Signal(i32),
    List,
}

struct Config {
    mode: ConfigMode,
    pids: Vec<u32>,
}

fn parse_cmdline() -> Result<Config, &'static str> {
    let mut pids = Vec::new();
    let mut mode = ConfigMode::Signal(libc::SIGTERM);
    let mut in_args = true;
    let mut in_s_arg = false;
    for arg in std::env::args().skip(1) {
        if in_args {
            if in_s_arg {
                let sig_no = lookup_signum(&arg)?;
                mode = ConfigMode::Signal(sig_no);
                in_s_arg = false;
            } else if arg == "-s" || arg == "--signal" {
                in_s_arg = true;
            } else if arg == "-l" || arg == "--list" {
                mode = ConfigMode::List;
            } else if arg == "--" {
                in_args = false;
            } else if let Some(st) = arg.strip_prefix("-") {
                let sig_no: i32 = match st.parse() {
                    Ok(signo) => signo,
                    Err(_) => lookup_signum(st)?,
                };
                mode = ConfigMode::Signal(sig_no);
            } else {
                in_args = false;
            }

            if in_args || arg == "--" {
                continue;
            }

            // fall through to process non-option arguments
        }

        match arg.parse::<u32>() {
            Ok(pid) => pids.push(pid),
            Err(_) => {
                return Err("Invalid PID");
            }
        }
    }

    Ok(Config { mode, pids })
}

fn send_signal(prog_cfg: &Config, sig_no: i32) -> u32 {
    let mut exit_code = 0;

    for pid in &prog_cfg.pids {
        let res = unsafe { libc::kill(*pid as libc::pid_t, sig_no as i32) };
        if res != 0 {
            let err = std::io::Error::last_os_error();
            eprintln!("kill pid {}: {}", pid, err);
            exit_code = 1;
        }
    }

    exit_code
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
        ConfigMode::Signal(sig_no) => send_signal(&prog_cfg, sig_no),
    };

    std::process::exit(exit_code as i32)
}
