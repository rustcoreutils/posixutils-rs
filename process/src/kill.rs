//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

extern crate clap;
extern crate libc;
extern crate plib;

use gettextrs::{bind_textdomain_codeset, setlocale, textdomain, LocaleCategory};
use plib::PROJECT_NAME;
use regex::Regex;

#[cfg(target_os = "macos")]
const SIGLIST: [(&str, u32); 31] = [
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
const SIGLIST: [(&str, u32); 32] = [
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

fn siglist_get(name: &str) -> Option<u32> {
    for (signame, signo) in SIGLIST.iter() {
        if *signame == name {
            return Some(*signo);
        }
    }

    None
}

fn lookup_signum(signame: &str) -> Result<u32, &'static str> {
    if signame == "0" {
        Ok(0)
    } else {
        match siglist_get(signame.to_uppercase().as_str()) {
            Some(sig_no) => Ok(sig_no),
            None => Err("Unknown signal name"),
        }
    }
}

enum ConfigMode {
    Signal(u32),
    List,
}

struct Config {
    mode: ConfigMode,
    pids: Vec<u32>,
}

fn parse_cmdline() -> Result<Config, &'static str> {
    let signame_re = Regex::new(r"^-(\w+)$").unwrap();
    let signum_re = Regex::new(r"^-(\d+)$").unwrap();

    let mut pids = Vec::new();
    let mut mode = ConfigMode::Signal(libc::SIGTERM as u32);
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
            } else if let Some(caps) = signum_re.captures(&arg) {
                let numstr = caps.get(1).unwrap().as_str();
                let sig_no = numstr.parse::<u32>().unwrap();
                mode = ConfigMode::Signal(sig_no);
            } else if let Some(caps) = signame_re.captures(&arg) {
                let namestr = caps.get(1).unwrap().as_str();
                let sig_no = lookup_signum(namestr)?;
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

fn list_signals() -> u32 {
    let mut output = String::new();
    for (name, _) in SIGLIST.iter() {
        output.push_str(name);
        output.push(' ');
    }

    println!("{}", output);

    0
}

fn send_signal(prog_cfg: &Config, sig_no: u32) -> u32 {
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
    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    let prog_cfg = parse_cmdline()?;

    let exit_code = match prog_cfg.mode {
        ConfigMode::List => list_signals(),
        ConfigMode::Signal(sig_no) => send_signal(&prog_cfg, sig_no),
    };

    std::process::exit(exit_code as i32)
}
