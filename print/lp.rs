//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::env;
use std::fs::File;
use std::io::{self, Cursor, Read};
use std::path::PathBuf;
use std::process::ExitCode;

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use ipp::prelude::*;
use num_traits::ToPrimitive;

/// lp - send files to a printer
#[derive(Parser)]
#[command(version, about = gettext("lp - send files to a printer"))]
struct Args {
    /// Exit only after file access is no longer required (copy mode)
    #[arg(short = 'c')]
    copy: bool,

    /// Printer destination (IPP URI, e.g., ipp://host:631/ipp/print)
    #[arg(short = 'd')]
    dest: Option<String>,

    /// Number of copies to print
    #[arg(short = 'n', default_value = "1", value_parser = clap::value_parser!(u32).range(1..))]
    copies: u32,

    /// Send mail after printing (not implemented)
    #[arg(short = 'm')]
    mail: bool,

    /// Suppress messages (no request ID output)
    #[arg(short = 's')]
    silent: bool,

    /// Write to terminal after printing (not implemented)
    #[arg(short = 'w')]
    write: bool,

    /// Printer-dependent options (can be specified multiple times)
    #[arg(short = 'o', action = clap::ArgAction::Append)]
    options: Vec<String>,

    /// Title for the banner page
    #[arg(short = 't')]
    title: Option<String>,

    /// Files to print (use '-' for stdin)
    #[arg()]
    files: Vec<PathBuf>,
}

/// Get the printer destination from args or environment
fn get_destination(args: &Args) -> Result<String, String> {
    // Priority: -d > LPDEST > PRINTER
    if let Some(ref dest) = args.dest {
        return Ok(dest.clone());
    }

    if let Ok(dest) = env::var("LPDEST") {
        if !dest.is_empty() {
            return Ok(dest);
        }
    }

    if let Ok(dest) = env::var("PRINTER") {
        if !dest.is_empty() {
            return Ok(dest);
        }
    }

    Err(gettext("no destination specified"))
}

/// Validate that the destination is a valid IPP URI
fn validate_uri(dest: &str) -> Result<Uri, String> {
    // Must start with ipp://
    if !dest.starts_with("ipp://") {
        return Err(gettext("invalid destination URI (must be ipp://...)"));
    }

    dest.parse::<Uri>()
        .map_err(|_| gettext("invalid destination URI"))
}

/// Read input data from a file or stdin
fn read_input(path: &PathBuf, copy_mode: bool) -> Result<Vec<u8>, io::Error> {
    let path_str = path.to_string_lossy();
    if path_str == "-" {
        // Read from stdin
        let mut data = Vec::new();
        io::stdin().read_to_end(&mut data)?;
        Ok(data)
    } else if copy_mode {
        // Copy mode: read entire file into memory
        let mut file = File::open(path)?;
        let mut data = Vec::new();
        file.read_to_end(&mut data)?;
        Ok(data)
    } else {
        // Non-copy mode: still read the file, but we could potentially
        // stream it. For simplicity, we'll read it all since IPP needs
        // the data to be available for the request.
        let mut file = File::open(path)?;
        let mut data = Vec::new();
        file.read_to_end(&mut data)?;
        Ok(data)
    }
}

/// Get the current username for the requesting-user-name attribute
fn get_username() -> String {
    env::var("USER")
        .or_else(|_| env::var("LOGNAME"))
        .unwrap_or_else(|_| String::from("anonymous"))
}

/// Format request ID output
fn format_request_id(dest: &str, job_id: i32) -> String {
    // Format: request id is <dest>-<job_id>
    format!("request id is {}-{}\n", dest, job_id)
}

/// Send a print job to the printer
fn send_print_job(
    uri: &Uri,
    data: Vec<u8>,
    args: &Args,
    file_name: Option<&str>,
) -> Result<i32, String> {
    let payload = IppPayload::new(Cursor::new(data));

    // Build the print job operation
    let mut builder =
        IppOperationBuilder::print_job(uri.clone(), payload).user_name(get_username());

    // Set job title
    if let Some(ref title) = args.title {
        builder = builder.job_title(title);
    } else if let Some(name) = file_name {
        builder = builder.job_title(name);
    }

    // Set copies if more than 1
    if args.copies > 1 {
        builder = builder.attribute(IppAttribute::new(
            "copies",
            IppValue::Integer(args.copies as i32),
        ));
    }

    // Apply -o options as IPP attributes
    // Format expected: name=value
    for opt in &args.options {
        if let Some((name, value)) = opt.split_once('=') {
            // Try to parse as integer, otherwise use as text
            let ipp_value = if let Ok(int_val) = value.parse::<i32>() {
                IppValue::Integer(int_val)
            } else if value.eq_ignore_ascii_case("true") {
                IppValue::Boolean(true)
            } else if value.eq_ignore_ascii_case("false") {
                IppValue::Boolean(false)
            } else {
                IppValue::Keyword(value.to_string())
            };
            builder = builder.attribute(IppAttribute::new(name, ipp_value));
        }
        // Ignore options without '=' as they're not valid IPP attribute format
    }

    let operation = builder.build();

    // Create client and send
    let client = IppClient::new(uri.clone());
    let response = client
        .send(operation)
        .map_err(|e| format!("{}: {}", gettext("printer error"), e))?;

    // Check response status
    let status = response.header().status_code();
    if !status.is_success() {
        return Err(format!(
            "{}: {} (0x{:04x})",
            gettext("printer error"),
            status,
            status.to_u16().unwrap_or(0)
        ));
    }

    // Extract job-id from response
    let job_id = response
        .attributes()
        .groups_of(DelimiterTag::JobAttributes)
        .flat_map(|g| g.attributes().get("job-id"))
        .flat_map(|attr| attr.value().as_integer().copied())
        .next()
        .unwrap_or(0);

    Ok(job_id)
}

fn do_lp(args: Args) -> Result<(), String> {
    // Check for unsupported options first
    if args.mail {
        return Err(gettext("-m option not supported"));
    }
    if args.write {
        return Err(gettext("-w option not supported"));
    }

    // Get and validate destination
    let dest = get_destination(&args)?;
    let uri = validate_uri(&dest)?;

    // Determine input sources
    let files: Vec<PathBuf> = if args.files.is_empty() {
        vec![PathBuf::from("-")]
    } else {
        args.files.clone()
    };

    // Process each file
    for file in &files {
        let file_name = if file.to_string_lossy() == "-" {
            None
        } else {
            file.file_name().and_then(|s| s.to_str())
        };

        // Read input data
        let data = read_input(file, args.copy)
            .map_err(|e| format!("{} '{}': {}", gettext("cannot open"), file.display(), e))?;

        // Send print job
        let job_id = send_print_job(&uri, data, &args, file_name)?;

        // Output request ID unless silent
        if !args.silent {
            print!("{}", format_request_id(&dest, job_id));
        }
    }

    Ok(())
}

fn main() -> ExitCode {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs").ok();
    bind_textdomain_codeset("posixutils-rs", "UTF-8").ok();

    let args = Args::parse();

    match do_lp(args) {
        Ok(()) => ExitCode::SUCCESS,
        Err(e) => {
            eprintln!("lp: {}", e);
            ExitCode::FAILURE
        }
    }
}
