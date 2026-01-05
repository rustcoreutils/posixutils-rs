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

use std::collections::{HashMap, HashSet};
use std::ffi::CStr;
use std::io::{self, Write};
use std::process::ExitCode;

use clap::Parser;
use gettextrs::{LocaleCategory, bind_textdomain_codeset, gettext, setlocale, textdomain};
use libc::{STDIN_FILENO, geteuid, getgrgid, getgrnam, getpwnam, getpwuid, isatty, ttyname};

#[cfg(target_os = "macos")]
mod platform {
    pub use crate::psmacos::*;
}

#[cfg(target_os = "linux")]
mod platform {
    pub use crate::pslinux::*;
}

/// Convert UID to username
fn uid_to_name(uid: u32) -> Option<String> {
    unsafe {
        let pw = getpwuid(uid);
        if pw.is_null() {
            return None;
        }
        let name = (*pw).pw_name;
        if name.is_null() {
            return None;
        }
        Some(CStr::from_ptr(name).to_string_lossy().into_owned())
    }
}

/// Convert username to UID
fn name_to_uid(name: &str) -> Option<u32> {
    let c_name = std::ffi::CString::new(name).ok()?;
    unsafe {
        let pw = getpwnam(c_name.as_ptr());
        if pw.is_null() {
            return None;
        }
        Some((*pw).pw_uid)
    }
}

/// Convert GID to group name
fn gid_to_name(gid: u32) -> Option<String> {
    unsafe {
        let gr = getgrgid(gid);
        if gr.is_null() {
            return None;
        }
        let name = (*gr).gr_name;
        if name.is_null() {
            return None;
        }
        Some(CStr::from_ptr(name).to_string_lossy().into_owned())
    }
}

/// Convert group name to GID
fn groupname_to_gid(name: &str) -> Option<u32> {
    let c_name = std::ffi::CString::new(name).ok()?;
    unsafe {
        let gr = getgrnam(c_name.as_ptr());
        if gr.is_null() {
            return None;
        }
        Some((*gr).gr_gid)
    }
}

/// ps - report process status
#[derive(Parser)]
#[command(version, about = gettext("ps - report process status"))]
struct Args {
    /// Write information for all processes
    #[arg(short = 'A', help = gettext("Write information for all processes"))]
    all: bool,

    /// Write information for all processes (alias for -A)
    #[arg(short = 'e', help = gettext("Write information for all processes (alias for -A)"))]
    all2: bool,

    /// Write information for all processes associated with terminals
    #[arg(short = 'a', help = gettext("Write information for all processes associated with terminals"))]
    terminal_processes: bool,

    /// Write information for all processes except session leaders
    #[arg(short = 'd', help = gettext("Write information for all processes except session leaders"))]
    exclude_session_leaders: bool,

    /// Generate a full listing
    #[arg(short = 'f', help = gettext("Generate a full listing"))]
    full_format: bool,

    /// Generate a long listing
    #[arg(short = 'l', help = gettext("Generate a long listing"))]
    long_format: bool,

    /// Write information for processes whose session leaders are in grouplist
    #[arg(short = 'g', value_name = "grouplist", help = gettext("Write information for processes whose session leaders are in grouplist"))]
    session_leaders: Option<String>,

    /// Write information for processes whose real group ID is in grouplist
    #[arg(short = 'G', value_name = "grouplist", help = gettext("Write information for processes whose real group ID is in grouplist"))]
    real_group: Option<String>,

    /// Write information for processes whose process ID is in proclist
    #[arg(short = 'p', value_name = "proclist", help = gettext("Write information for processes in proclist"))]
    pid_list: Option<String>,

    /// Write information for processes associated with terminals in termlist
    #[arg(short = 't', value_name = "termlist", help = gettext("Write information for processes on terminals in termlist"))]
    term_list: Option<String>,

    /// Write information for processes whose effective user ID is in userlist
    #[arg(short = 'u', value_name = "userlist", help = gettext("Write information for processes whose user ID is in userlist"))]
    user_list: Option<String>,

    /// Write information for processes whose real user ID is in userlist
    #[arg(short = 'U', value_name = "userlist", help = gettext("Write information for processes whose real user ID is in userlist"))]
    real_user: Option<String>,

    /// Custom output format
    #[arg(short = 'o', value_name = "format", action = clap::ArgAction::Append, help = gettext("Specify output format"))]
    output_format: Vec<String>,
}

/// Field definition for -o option
#[derive(Clone)]
struct OutputField {
    name: &'static str,
    header: String,
    width: usize,
}

/// POSIX-defined output fields
fn get_posix_fields() -> HashMap<&'static str, (&'static str, usize)> {
    HashMap::from([
        ("ruser", ("RUSER", 8)),
        ("user", ("USER", 8)),
        ("rgroup", ("RGROUP", 8)),
        ("group", ("GROUP", 8)),
        ("pid", ("PID", 5)),
        ("ppid", ("PPID", 5)),
        ("pgid", ("PGID", 5)),
        ("pcpu", ("%CPU", 4)),
        ("vsz", ("VSZ", 6)),
        ("nice", ("NI", 3)),
        ("etime", ("ELAPSED", 11)),
        ("time", ("TIME", 8)),
        ("tty", ("TT", 8)),
        ("comm", ("COMMAND", 16)),
        ("args", ("COMMAND", 32)),
        // Additional common fields
        ("uid", ("UID", 5)),
        ("gid", ("GID", 5)),
        ("sid", ("SID", 5)),
        ("state", ("S", 1)),
        ("pri", ("PRI", 3)),
        ("addr", ("ADDR", 8)),
        ("sz", ("SZ", 6)),
        ("wchan", ("WCHAN", 8)),
        ("stime", ("STIME", 8)),
        ("c", ("C", 2)),
        ("f", ("F", 2)),
    ])
}

/// Parse comma/space-separated list into a HashSet
fn parse_list(list: &str) -> HashSet<String> {
    list.split([',', ' '])
        .map(|s| s.trim().to_string())
        .filter(|s| !s.is_empty())
        .collect()
}

/// Parse numeric list (for PIDs, UIDs, GIDs)
fn parse_numeric_list(list: &str) -> Result<HashSet<i32>, String> {
    let mut result = HashSet::new();
    for item in list.split([',', ' ']) {
        let item = item.trim();
        if item.is_empty() {
            continue;
        }
        match item.parse::<i32>() {
            Ok(n) => {
                result.insert(n);
            }
            Err(_) => {
                return Err(format!("invalid number: {}", item));
            }
        }
    }
    Ok(result)
}

/// Parse user list (can be names or numeric UIDs)
fn parse_user_list(list: &str) -> HashSet<u32> {
    let mut result = HashSet::new();
    for item in list.split([',', ' ']) {
        let item = item.trim();
        if item.is_empty() {
            continue;
        }
        if let Ok(uid) = item.parse::<u32>() {
            result.insert(uid);
        } else {
            // Try to look up username
            if let Some(uid) = name_to_uid(item) {
                result.insert(uid);
            }
        }
    }
    result
}

/// Parse group list (can be names or numeric GIDs)
fn parse_group_list(list: &str) -> HashSet<u32> {
    let mut result = HashSet::new();
    for item in list.split([',', ' ']) {
        let item = item.trim();
        if item.is_empty() {
            continue;
        }
        if let Ok(gid) = item.parse::<u32>() {
            result.insert(gid);
        } else {
            // Try to look up group name
            if let Some(gid) = groupname_to_gid(item) {
                result.insert(gid);
            }
        }
    }
    result
}

/// Parse -o format specification
fn parse_output_format(format_specs: &[String]) -> Result<Vec<OutputField>, String> {
    let posix_fields = get_posix_fields();
    let mut fields = Vec::new();

    for spec in format_specs {
        for field_spec in spec.split([',', ' ']) {
            let field_spec = field_spec.trim();
            if field_spec.is_empty() {
                continue;
            }

            let (name, custom_header) = if let Some(eq_pos) = field_spec.find('=') {
                let name = &field_spec[..eq_pos];
                let header = &field_spec[eq_pos + 1..];
                (name, Some(header.to_string()))
            } else {
                (field_spec, None)
            };

            let name_lower = name.to_lowercase();
            if let Some((default_header, width)) = posix_fields.get(name_lower.as_str()) {
                let header = custom_header.unwrap_or_else(|| default_header.to_string());
                let width = if header.is_empty() {
                    *width
                } else {
                    header.len().max(*width)
                };
                fields.push(OutputField {
                    name: match name_lower.as_str() {
                        "ruser" => "ruser",
                        "user" => "user",
                        "rgroup" => "rgroup",
                        "group" => "group",
                        "pid" => "pid",
                        "ppid" => "ppid",
                        "pgid" => "pgid",
                        "pcpu" => "pcpu",
                        "vsz" => "vsz",
                        "nice" => "nice",
                        "etime" => "etime",
                        "time" => "time",
                        "tty" => "tty",
                        "comm" => "comm",
                        "args" => "args",
                        "uid" => "uid",
                        "gid" => "gid",
                        "sid" => "sid",
                        "state" => "state",
                        "pri" => "pri",
                        "addr" => "addr",
                        "sz" => "sz",
                        "wchan" => "wchan",
                        "stime" => "stime",
                        "c" => "c",
                        "f" => "f",
                        _ => return Err(format!("unknown format specifier: {}", name)),
                    },
                    header,
                    width,
                });
            } else {
                return Err(format!("unknown format specifier: {}", name));
            }
        }
    }

    Ok(fields)
}

/// Get default output fields
fn get_default_fields() -> Vec<OutputField> {
    vec![
        OutputField {
            name: "pid",
            header: "PID".to_string(),
            width: 5,
        },
        OutputField {
            name: "tty",
            header: "TTY".to_string(),
            width: 8,
        },
        OutputField {
            name: "time",
            header: "TIME".to_string(),
            width: 8,
        },
        OutputField {
            name: "comm",
            header: "CMD".to_string(),
            width: 16,
        },
    ]
}

/// Get full format fields (-f)
fn get_full_fields() -> Vec<OutputField> {
    vec![
        OutputField {
            name: "uid",
            header: "UID".to_string(),
            width: 8,
        },
        OutputField {
            name: "pid",
            header: "PID".to_string(),
            width: 5,
        },
        OutputField {
            name: "ppid",
            header: "PPID".to_string(),
            width: 5,
        },
        OutputField {
            name: "c",
            header: "C".to_string(),
            width: 2,
        },
        OutputField {
            name: "stime",
            header: "STIME".to_string(),
            width: 8,
        },
        OutputField {
            name: "tty",
            header: "TTY".to_string(),
            width: 8,
        },
        OutputField {
            name: "time",
            header: "TIME".to_string(),
            width: 8,
        },
        OutputField {
            name: "args",
            header: "CMD".to_string(),
            width: 32,
        },
    ]
}

/// Get long format fields (-l)
fn get_long_fields() -> Vec<OutputField> {
    vec![
        OutputField {
            name: "f",
            header: "F".to_string(),
            width: 2,
        },
        OutputField {
            name: "state",
            header: "S".to_string(),
            width: 1,
        },
        OutputField {
            name: "uid",
            header: "UID".to_string(),
            width: 5,
        },
        OutputField {
            name: "pid",
            header: "PID".to_string(),
            width: 5,
        },
        OutputField {
            name: "ppid",
            header: "PPID".to_string(),
            width: 5,
        },
        OutputField {
            name: "c",
            header: "C".to_string(),
            width: 2,
        },
        OutputField {
            name: "pri",
            header: "PRI".to_string(),
            width: 3,
        },
        OutputField {
            name: "nice",
            header: "NI".to_string(),
            width: 3,
        },
        OutputField {
            name: "addr",
            header: "ADDR".to_string(),
            width: 8,
        },
        OutputField {
            name: "sz",
            header: "SZ".to_string(),
            width: 6,
        },
        OutputField {
            name: "wchan",
            header: "WCHAN".to_string(),
            width: 8,
        },
        OutputField {
            name: "tty",
            header: "TTY".to_string(),
            width: 8,
        },
        OutputField {
            name: "time",
            header: "TIME".to_string(),
            width: 8,
        },
        OutputField {
            name: "comm",
            header: "CMD".to_string(),
            width: 16,
        },
    ]
}

/// Format time value (CPU time) as [dd-]hh:mm:ss
fn format_time(ticks: u64) -> String {
    // Convert ticks to seconds (assuming 100 ticks per second)
    let total_secs = ticks / 100;
    let hours = total_secs / 3600;
    let mins = (total_secs % 3600) / 60;
    let secs = total_secs % 60;

    if hours >= 24 {
        let days = hours / 24;
        let hours = hours % 24;
        format!("{}-{:02}:{:02}:{:02}", days, hours, mins, secs)
    } else {
        format!("{:02}:{:02}:{:02}", hours, mins, secs)
    }
}

/// Format elapsed time as [[dd-]hh:]mm:ss
fn format_etime(start_time: u64) -> String {
    // This is a simplified version - proper implementation would need boot time
    // For now, just format as mm:ss
    let now = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_secs())
        .unwrap_or(0);

    let elapsed = if start_time > 0 && now > start_time {
        now - start_time
    } else {
        0
    };

    let days = elapsed / 86400;
    let hours = (elapsed % 86400) / 3600;
    let mins = (elapsed % 3600) / 60;
    let secs = elapsed % 60;

    if days > 0 {
        format!("{}-{:02}:{:02}:{:02}", days, hours, mins, secs)
    } else if hours > 0 {
        format!("{:02}:{:02}:{:02}", hours, mins, secs)
    } else {
        format!("{:02}:{:02}", mins, secs)
    }
}

/// Get current terminal name
fn get_current_tty() -> Option<String> {
    unsafe {
        if isatty(STDIN_FILENO) == 0 {
            return None;
        }
        let name = ttyname(STDIN_FILENO);
        if name.is_null() {
            return None;
        }
        let cstr = std::ffi::CStr::from_ptr(name);
        let name_str = cstr.to_string_lossy().to_string();
        // Extract just the device name (e.g., "pts/0" from "/dev/pts/0")
        if let Some(stripped) = name_str.strip_prefix("/dev/") {
            Some(stripped.to_string())
        } else {
            Some(name_str)
        }
    }
}

/// Get field value for a process
fn get_field_value(proc: &platform::ProcessInfo, field: &str, full_format: bool) -> String {
    match field {
        "pid" => proc.pid.to_string(),
        "ppid" => proc.ppid.to_string(),
        "pgid" => proc.pgid.to_string(),
        "sid" => proc.sid.to_string(),
        "uid" => {
            if full_format {
                uid_to_name(proc.uid).unwrap_or_else(|| proc.uid.to_string())
            } else {
                proc.uid.to_string()
            }
        }
        "gid" => proc.gid.to_string(),
        "user" => uid_to_name(proc.uid).unwrap_or_else(|| proc.uid.to_string()),
        "ruser" => uid_to_name(proc.ruid).unwrap_or_else(|| proc.ruid.to_string()),
        "group" => gid_to_name(proc.gid).unwrap_or_else(|| proc.gid.to_string()),
        "rgroup" => gid_to_name(proc.rgid).unwrap_or_else(|| proc.rgid.to_string()),
        "tty" => proc.tty.as_deref().unwrap_or("?").to_string(),
        "comm" => proc.comm.clone(),
        "args" => proc.args.clone(),
        "time" => format_time(proc.time),
        "etime" => format_etime(proc.start_time),
        "nice" => proc.nice.to_string(),
        "pri" => proc.priority.to_string(),
        "vsz" => proc.vsz.to_string(),
        "sz" => (proc.vsz / 4).to_string(), // Convert KB to blocks (4KB pages)
        "state" => proc.state.to_string(),
        "f" => format!("{:x}", proc.flags & 0xf),
        "c" => "-".to_string(),    // CPU utilization - difficult to calculate
        "addr" => "-".to_string(), // Memory address - implementation specific
        "wchan" => "-".to_string(), // Wait channel - implementation specific
        "stime" => "-".to_string(), // Start time - would need proper formatting
        "pcpu" => "-".to_string(), // CPU percentage - needs interval measurement
        _ => "-".to_string(),
    }
}

fn main() -> ExitCode {
    setlocale(LocaleCategory::LcAll, "");
    if let Err(e) = textdomain("posixutils-rs") {
        eprintln!("Warning: {}", e);
    }
    let _ = bind_textdomain_codeset("posixutils-rs", "UTF-8");

    let args = Args::parse();

    // Get processes
    let processes = match platform::list_processes() {
        Ok(p) => p,
        Err(e) => {
            eprintln!("ps: {}", e);
            return ExitCode::from(1);
        }
    };

    // Determine if any selection options were specified
    let any_selection = args.all
        || args.all2
        || args.terminal_processes
        || args.exclude_session_leaders
        || args.session_leaders.is_some()
        || args.real_group.is_some()
        || args.pid_list.is_some()
        || args.term_list.is_some()
        || args.user_list.is_some()
        || args.real_user.is_some();

    // Parse filter options
    let session_filter: Option<HashSet<i32>> = args.session_leaders.as_ref().and_then(|s| {
        parse_numeric_list(s)
            .map_err(|e| eprintln!("ps: -g: {}", e))
            .ok()
    });

    let real_group_filter: Option<HashSet<u32>> =
        args.real_group.as_ref().map(|s| parse_group_list(s));

    let pid_filter: Option<HashSet<i32>> = args.pid_list.as_ref().and_then(|s| {
        parse_numeric_list(s)
            .map_err(|e| eprintln!("ps: -p: {}", e))
            .ok()
    });

    let term_filter: Option<HashSet<String>> = args.term_list.as_ref().map(|s| parse_list(s));

    let user_filter: Option<HashSet<u32>> = args.user_list.as_ref().map(|s| parse_user_list(s));

    let real_user_filter: Option<HashSet<u32>> =
        args.real_user.as_ref().map(|s| parse_user_list(s));

    // Get current user and terminal for default filtering
    let current_uid = unsafe { geteuid() };
    let current_tty = get_current_tty();

    // Filter processes
    let filtered: Vec<_> = processes
        .into_iter()
        .filter(|p| {
            // If -A or -e, include all
            if args.all || args.all2 {
                return true;
            }

            // Apply selection filters (OR logic between different options)
            if any_selection {
                let mut matches = false;

                // -a: processes with terminals
                if args.terminal_processes && p.tty.is_some() {
                    matches = true;
                }

                // -g: session leader filter
                if let Some(ref sids) = session_filter {
                    if sids.contains(&p.sid) {
                        matches = true;
                    }
                }

                // -G: real group filter
                if let Some(ref gids) = real_group_filter {
                    if gids.contains(&p.rgid) {
                        matches = true;
                    }
                }

                // -p: PID filter
                if let Some(ref pids) = pid_filter {
                    if pids.contains(&p.pid) {
                        matches = true;
                    }
                }

                // -t: terminal filter
                if let Some(ref terms) = term_filter {
                    if let Some(ref tty) = p.tty {
                        for term in terms {
                            if tty.contains(term) || tty == term {
                                matches = true;
                                break;
                            }
                        }
                    }
                }

                // -u: effective user filter
                if let Some(ref uids) = user_filter {
                    if uids.contains(&p.uid) {
                        matches = true;
                    }
                }

                // -U: real user filter
                if let Some(ref uids) = real_user_filter {
                    if uids.contains(&p.ruid) {
                        matches = true;
                    }
                }

                // -d: exclude session leaders
                if args.exclude_session_leaders && p.pid == p.sid {
                    return false;
                }

                return matches;
            }

            // Default: same effective UID and same controlling terminal
            if p.uid != current_uid {
                return false;
            }
            match (&p.tty, &current_tty) {
                (Some(ptty), Some(ctty)) => ptty.contains(ctty) || ctty.contains(ptty),
                (None, None) => true,
                _ => false,
            }
        })
        .collect();

    // Determine output fields
    let output_fields = if !args.output_format.is_empty() {
        match parse_output_format(&args.output_format) {
            Ok(fields) => fields,
            Err(e) => {
                eprintln!("ps: {}", e);
                return ExitCode::from(1);
            }
        }
    } else if args.long_format {
        get_long_fields()
    } else if args.full_format {
        get_full_fields()
    } else {
        get_default_fields()
    };

    // Check if we should print header (all headers non-empty)
    let print_header = output_fields.iter().any(|f| !f.header.is_empty());

    let stdout = io::stdout();
    let mut out = stdout.lock();

    // Print header
    if print_header {
        for (i, field) in output_fields.iter().enumerate() {
            if i > 0 {
                let _ = write!(out, " ");
            }
            let _ = write!(out, "{:>width$}", field.header, width = field.width);
        }
        let _ = writeln!(out);
    }

    // Print processes
    for proc in filtered {
        for (i, field) in output_fields.iter().enumerate() {
            if i > 0 {
                let _ = write!(out, " ");
            }
            let value = get_field_value(&proc, field.name, args.full_format);
            // Right-align numeric fields, left-align text
            if matches!(
                field.name,
                "pid" | "ppid" | "pgid" | "sid" | "uid" | "gid" | "nice" | "pri" | "vsz" | "sz"
            ) {
                let _ = write!(out, "{:>width$}", value, width = field.width);
            } else {
                let _ = write!(out, "{:<width$}", value, width = field.width);
            }
        }
        let _ = writeln!(out);
    }

    ExitCode::SUCCESS
}
