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
use std::collections::HashMap;

#[cfg(target_os = "macos")]
mod platform {
    pub use crate::psmacos::*;
}

#[cfg(target_os = "linux")]
mod platform {
    pub use crate::pslinux::*;
}

/// ps - report process status
#[derive(Parser)]
#[command(version, about)]
struct Args {
    /// List all processes
    #[arg(short = 'A', long)]
    all: bool,

    /// List all processes (alias for -A)
    #[arg(short = 'e')]
    all2: bool,

    /// List all processes associated with terminals
    #[arg(short = 'a')]
    terminal_processes: bool,

    /// Exclude session leaders
    #[arg(short = 'd')]
    exclude_session_leaders: bool,

    /// Full output format (-f)
    #[arg(short = 'f', long = "full")]
    full_format: bool,

    /// Long output format (-l)
    #[arg(short = 'l', long = "long")]
    long_format: bool,

    /// Custom output format (-o)
    #[arg(short = 'o', value_parser = clap::builder::NonEmptyStringValueParser::new())]
    output_format: Option<String>,
}

// Parse the -o format option into a list of fields
fn parse_output_format<'a>(
    format: &'a str,
    posix_fields: &'a HashMap<&'a str, &'a str>,
) -> Vec<&'a str> {
    format
        .split(|c| c == ' ' || c == ',')
        .map(|s| {
            let field = s.split('=').next().unwrap_or("").trim();
            if posix_fields.contains_key(field) {
                field
            } else {
                panic!("Invalid field specified in -o option: {}", field);
            }
        })
        .collect()
}

// Lookup table for POSIX-compliant output fields
fn posix_field_map() -> HashMap<&'static str, &'static str> {
    HashMap::from([
        ("ruser", "RUSER"),
        ("user", "USER"),
        ("rgroup", "RGROUP"),
        ("group", "GROUP"),
        ("pid", "PID"),
        ("ppid", "PPID"),
        ("pgid", "PGID"),
        ("pcpu", "%CPU"),
        ("vsz", "VSZ"),
        ("nice", "NI"),
        ("etime", "ELAPSED"),
        ("time", "TIME"),
        ("tty", "TTY"),
        ("comm", "COMMAND"),
        ("args", "COMMAND"),
    ])
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

    // Define a lookup table for POSIX-compliant fields
    let posix_fields = posix_field_map();

    // Build output based on -o, -f, -l, or default
    let output_fields = if let Some(ref format) = args.output_format {
        parse_output_format(format, &posix_fields)
    } else if args.full_format {
        vec!["uid", "pid", "ppid", "C", "time", "comm"]
    } else if args.long_format {
        vec!["nice", "vsz", "WCHAN", "tty", "comm"]
    } else {
        vec!["pid", "ppid", "tty", "time", "comm"] // Default format
    };

    // Print the header
    for field in &output_fields {
        let header = posix_fields.get(*field).unwrap_or(&field);
        print!("{:<10} ", header);
    }
    println!();

    // Print each process
    for proc in filtered_processes {
        for field in &output_fields {
            match *field {
                "pid" => print!("{:<10} ", proc.pid),
                "ppid" => print!("{:<10} ", proc.ppid),
                "group" => print!("{:<10} ", proc.gid),
                "tty" => print!("{:<10} ", proc.tty.as_deref().unwrap_or("-")),
                // "time" => print!("{:<10} ", proc.time),
                "comm" => print!("{:<10} ", proc.path),
                "user" => print!("{:<10} ", proc.uid), // Example for user field, would need to resolve UID -> username
                // Add cases for more fields as needed...
                _ => print!("{:<10} ", "-"),
            }
        }
        println!();
    }
}
