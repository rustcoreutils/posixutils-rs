//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
//! uustat - UUCP status enquiry and job control
//!
//! POSIX-compliant implementation.

mod common;

use clap::Parser;
use common::{find_job, list_jobs, spool_dir};
use gettextrs::{bind_textdomain_codeset, setlocale, textdomain, LocaleCategory};
use std::collections::HashMap;
use std::env;
use std::process::ExitCode;

/// uustat - UUCP status enquiry and job control
#[derive(Parser)]
#[command(
    version,
    about = "UUCP status enquiry and job control",
    override_usage = "uustat [-q|-k jobid|-r jobid]\n       uustat [-s system] [-u user]"
)]
struct Args {
    /// Report queue summary (jobs per system)
    #[arg(short = 'q', conflicts_with_all = ["kill_job", "rejuvenate_job", "system", "user"])]
    queue_summary: bool,

    /// Kill (remove) the specified job
    #[arg(short = 'k', value_name = "JOBID", conflicts_with_all = ["queue_summary", "rejuvenate_job", "system", "user"])]
    kill_job: Option<String>,

    /// Rejuvenate (touch) the specified job
    #[arg(short = 'r', value_name = "JOBID", conflicts_with_all = ["queue_summary", "kill_job", "system", "user"])]
    rejuvenate_job: Option<String>,

    /// Filter jobs by system name
    #[arg(short = 's', value_name = "SYSTEM", conflicts_with_all = ["queue_summary", "kill_job", "rejuvenate_job"])]
    system: Option<String>,

    /// Filter jobs by user name
    #[arg(short = 'u', value_name = "USER", conflicts_with_all = ["queue_summary", "kill_job", "rejuvenate_job"])]
    user: Option<String>,
}

fn main() -> ExitCode {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs").unwrap();
    bind_textdomain_codeset("posixutils-rs", "UTF-8").unwrap();

    let args = Args::parse();

    // Check if spool is accessible
    let spool = spool_dir();
    if !spool.exists() {
        // Not an error if empty - just no jobs
    }

    if args.queue_summary {
        // Show queue summary per system
        let jobs = list_jobs(None, None);
        let mut counts: HashMap<String, usize> = HashMap::new();
        for job in jobs {
            *counts.entry(job.system.clone()).or_insert(0) += 1;
        }
        for (system, count) in counts {
            println!("{}: {} job(s) queued", system, count);
        }
    } else if let Some(jid) = args.kill_job {
        match find_job(&jid) {
            Some(job) => {
                // Check ownership
                let current_user = env::var("USER").unwrap_or_default();
                let is_root = current_user == "root";

                if job.user != current_user && !is_root {
                    eprintln!("uustat: permission denied to kill job {}", jid);
                    return ExitCode::from(1);
                }

                if let Err(e) = job.delete() {
                    eprintln!("uustat: failed to delete job {}: {}", jid, e);
                    return ExitCode::from(1);
                }
            }
            None => {
                eprintln!("uustat: job {} not found", jid);
                return ExitCode::from(1);
            }
        }
    } else if let Some(jid) = args.rejuvenate_job {
        match find_job(&jid) {
            Some(job) => {
                // Check ownership
                let current_user = env::var("USER").unwrap_or_default();
                let is_root = current_user == "root";

                if job.user != current_user && !is_root {
                    eprintln!("uustat: permission denied to rejuvenate job {}", jid);
                    return ExitCode::from(1);
                }

                if let Err(e) = job.rejuvenate() {
                    eprintln!("uustat: failed to rejuvenate job {}: {}", jid, e);
                    return ExitCode::from(1);
                }
            }
            None => {
                eprintln!("uustat: job {} not found", jid);
                return ExitCode::from(1);
            }
        }
    } else {
        // Default: list current user's jobs, or filter by -s/-u
        let current_user = env::var("USER").unwrap_or_default();
        let user = args.user.as_deref().unwrap_or_else(|| {
            if args.system.is_none() {
                &current_user
            } else {
                "" // No user filter if -s specified alone
            }
        });

        let user_opt = if user.is_empty() { None } else { Some(user) };
        let jobs = list_jobs(args.system.as_deref(), user_opt);

        for job in jobs {
            // Output format: jobid user system
            println!("{}\t{}\t{}", job.id, job.user, job.system);
        }
    }

    ExitCode::SUCCESS
}
