//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

#[cfg(target_os = "macos")]
mod psmacos;

#[cfg(target_os = "linux")]
mod pslinux;

use clap::Parser;

#[cfg(target_os = "macos")]
mod platform {
    pub use crate::psmacos::*;
}

#[cfg(target_os = "linux")]
mod platform {
    pub use crate::pslinux::*;
}

#[derive(Parser)]
#[command(name = "ps")]
#[command(about = "Report process status", version = "1.0")]
struct Args {
    /// List all processes
    #[arg(short = 'A', long)]
    all: bool,

    /// List all processes (alias for -A)
    #[arg(short = 'e')]
    all2: bool,

    /// List all processes associated with terminals
    #[arg(short = 'a', long)]
    terminal_processes: bool,

    /// Exclude session leaders
    #[arg(short = 'd', long)]
    exclude_session_leaders: bool,
}

fn main() {
    let mut args = Args::parse();
    if args.all2 {
        args.all = true;
    }

    let processes = match platform::list_processes() {
        Ok(processes) => processes,
        Err(e) => {
            eprintln!("Error: {}", e);
            return;
        }
    };

    // Filter processes based on arguments
    let filtered_processes = if args.terminal_processes && args.exclude_session_leaders {
        processes
            .into_iter()
            .filter(|p| p.tty.is_some() && p.pid != p.sid) // -a and -d logic
            .collect::<Vec<_>>()
    } else if args.terminal_processes {
        processes
            .into_iter()
            .filter(|p| p.tty.is_some()) // -a logic
            .collect::<Vec<_>>()
    } else if args.exclude_session_leaders {
        processes
            .into_iter()
            .filter(|p| p.pid != p.sid) // -d logic
            .collect::<Vec<_>>()
    } else {
        processes
    };

    println!(
        "{:<5} {:<5} {:<5} {:<5} COMMAND",
        "PID", "PPID", "UID", "GID",
    );
    for proc in filtered_processes {
        println!(
            "{:<5} {:<5} {:<5} {:<5} {}",
            proc.pid, proc.ppid, proc.uid, proc.gid, proc.path
        );
    }
}
