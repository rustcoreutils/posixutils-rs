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

use std::collections::HashMap;

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};

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
#[command(version, about = gettext("ps - report process status"))]
struct Args {
    #[arg(short = 'A', long, help = gettext("List all processes"))]
    all: bool,

    #[arg(short = 'e', help = gettext("List all processes (alias for -A)"))]
    all2: bool,

    #[arg(short = 'a', help = gettext("List all processes associated with terminals"))]
    terminal_processes: bool,

    #[arg(short = 'd', help = gettext("Exclude session leaders"))]
    exclude_session_leaders: bool,

    #[arg(short = 'f', long = "full", help = gettext("Full output format (-f)"))]
    full_format: bool,

    #[arg(short = 'l', long = "long", help = gettext("Long output format (-l)"))]
    long_format: bool,

    #[arg(short = 'o', value_parser = clap::builder::NonEmptyStringValueParser::new(), help = gettext("Custom output format (-o)"))]
    output_format: Option<String>,
}

// Parse the -o format option into a list of fields
fn parse_output_format<'a>(
    format: &'a str,
    posix_fields: &'a HashMap<&'a str, &'a str>,
) -> Vec<&'a str> {
    format
        .split([' ', ','])
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

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let mut args = Args::parse();

    if args.all2 {
        args.all = true;
    }

    let processes = match platform::list_processes() {
        Ok(processes) => processes,
        Err(e) => {
            eprintln!("Error: {}", e);
            return Ok(());
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
        let header = posix_fields.get(*field).unwrap_or(field);
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

    Ok(())
}
