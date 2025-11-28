//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
//! uucp - system-to-system copy
//!
//! POSIX-compliant implementation using SSH for transport.

mod common;

use clap::Parser;
use common::{
    expand_local_path, expand_remote_path, is_local_system, parse_path_spec, send_mail,
    send_remote_mail, ssh_fetch_file, ssh_send_file, Job,
};
use gettextrs::{bind_textdomain_codeset, setlocale, textdomain, LocaleCategory};
use std::env;
use std::fs;
use std::path::Path;
use std::process::ExitCode;

/// uucp - system-to-system copy
#[derive(Parser)]
#[command(
    version,
    about = "Copy files between systems",
    override_usage = "uucp [-cCdfjmr] [-n user] source-file... destination-file"
)]
struct Args {
    /// Do not copy local file to spool directory (default)
    #[arg(short = 'c', overrides_with = "copy_to_spool")]
    no_copy_to_spool: bool,

    /// Force copy of local file to spool directory
    #[arg(short = 'C')]
    copy_to_spool: bool,

    /// Create intermediate directories as needed (default)
    #[arg(short = 'd', overrides_with = "no_create_dirs")]
    create_dirs: bool,

    /// Do not create intermediate directories
    #[arg(short = 'f')]
    no_create_dirs: bool,

    /// Write job ID to stdout
    #[arg(short = 'j')]
    print_job_id: bool,

    /// Send mail to requester when copy completes
    #[arg(short = 'm')]
    mail_requester: bool,

    /// Notify user on remote system when copy completes
    #[arg(short = 'n', value_name = "USER")]
    notify_user: Option<String>,

    /// Queue job only, do not start transfer
    #[arg(short = 'r')]
    queue_only: bool,

    /// Source files and destination (last argument is destination)
    #[arg(required = true, num_args = 2..)]
    files: Vec<String>,
}

fn main() -> ExitCode {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs").unwrap();
    bind_textdomain_codeset("posixutils-rs", "UTF-8").unwrap();

    let args = Args::parse();

    // -d is the default, -f overrides it
    let create_dirs = !args.no_create_dirs;

    // Split files into sources and destination
    let mut files = args.files.clone();
    let dest_spec = files.pop().unwrap();
    let source_specs = files;

    // Parse destination
    let (dest_system, dest_path) = parse_path_spec(&dest_spec);
    let dest_is_local = is_local_system(&dest_system);

    // Check for unsupported routing (system!system!path)
    if dest_path.contains('!') {
        eprintln!("uucp: route specification not supported");
        return ExitCode::from(1);
    }

    // Determine if destination is a directory
    let is_dir = source_specs.len() > 1 || dest_path.ends_with('/');

    // Expand destination path for local
    let dest_path_expanded = if dest_is_local {
        expand_local_path(&dest_path)
    } else {
        expand_remote_path(&dest_path)
    };

    // Process each source
    let mut success = true;
    let mut job: Option<Job> = None;

    for source_spec in &source_specs {
        let (src_system, src_path) = parse_path_spec(source_spec);

        // Check for unsupported routing
        if src_path.contains('!') {
            eprintln!("uucp: route specification not supported");
            return ExitCode::from(1);
        }

        let src_is_local = is_local_system(&src_system);

        // Expand source path
        let src_path_expanded = if src_is_local {
            expand_local_path(&src_path)
        } else {
            expand_remote_path(&src_path)
        };

        // Determine the actual destination filename
        let final_dest = if is_dir {
            let basename = Path::new(&src_path_expanded)
                .file_name()
                .map(|s| s.to_string_lossy().to_string())
                .unwrap_or_else(|| src_path_expanded.clone());
            let mut d = dest_path_expanded.clone();
            if !d.ends_with('/') {
                d.push('/');
            }
            d.push_str(&basename);
            d
        } else {
            dest_path_expanded.clone()
        };

        if args.queue_only {
            // Queue the job instead of executing
            let system = if !src_is_local {
                &src_system
            } else if !dest_is_local {
                &dest_system
            } else {
                "local"
            };

            if job.is_none() {
                let request = format!("{} -> {}", source_spec, dest_spec);
                job = Some(Job::new(system, "uucp", &request));
            }
        } else {
            // Execute transfer immediately
            let result = if src_is_local && dest_is_local {
                // Local to local copy
                copy_local(&src_path_expanded, &final_dest, create_dirs)
            } else if src_is_local && !dest_is_local {
                // Local to remote
                ssh_send_file(&dest_system, &src_path_expanded, &final_dest, create_dirs)
            } else if !src_is_local && dest_is_local {
                // Remote to local
                ssh_fetch_file(&src_system, &src_path_expanded, &final_dest, create_dirs)
            } else {
                // Remote to remote (via local)
                copy_remote_to_remote(
                    &src_system,
                    &src_path_expanded,
                    &dest_system,
                    &final_dest,
                    create_dirs,
                )
            };

            if let Err(e) = result {
                eprintln!("uucp: {}", e);
                success = false;
                break;
            }

            // Send notification to remote user if -n specified
            if let Some(ref user) = args.notify_user {
                if !dest_is_local {
                    let current_user = env::var("USER").unwrap_or_else(|_| "unknown".to_string());
                    let msg = format!("File {} sent from {}", final_dest, current_user);
                    let _ = send_remote_mail(&dest_system, user, "uucp file received", &msg);
                }
            }
        }
    }

    // Handle queued job
    if let Some(j) = job {
        if let Err(e) = j.save() {
            eprintln!("uucp: failed to queue job: {}", e);
            return ExitCode::from(1);
        }
        if args.print_job_id {
            println!("{}", j.id);
        }
        return ExitCode::SUCCESS;
    }

    if !success {
        return ExitCode::from(1);
    }

    // Print job ID if requested (for immediate execution, generate one)
    if args.print_job_id {
        println!("{}", common::generate_job_id());
    }

    // Send mail to requester if -m specified and successful
    if args.mail_requester {
        let current_user = env::var("USER").unwrap_or_else(|_| "unknown".to_string());
        let _ = send_mail(
            &current_user,
            "uucp complete",
            "Your uucp request has completed.",
        );
    }

    ExitCode::SUCCESS
}

/// Copy file locally
fn copy_local(src: &str, dest: &str, create_dirs: bool) -> std::io::Result<()> {
    if create_dirs {
        if let Some(parent) = Path::new(dest).parent() {
            fs::create_dir_all(parent)?;
        }
    }
    fs::copy(src, dest)?;
    Ok(())
}

/// Copy from one remote to another via local
fn copy_remote_to_remote(
    src_host: &str,
    src_path: &str,
    dest_host: &str,
    dest_path: &str,
    create_dirs: bool,
) -> std::io::Result<()> {
    // Create temp file
    let temp_dir = std::env::temp_dir();
    let temp_file = temp_dir.join(format!("uucp_{}", std::process::id()));
    let temp_path = temp_file.to_string_lossy().to_string();

    // Fetch from source
    ssh_fetch_file(src_host, src_path, &temp_path, true)?;

    // Send to destination
    let result = ssh_send_file(dest_host, &temp_path, dest_path, create_dirs);

    // Clean up temp file
    let _ = fs::remove_file(&temp_file);

    result
}
