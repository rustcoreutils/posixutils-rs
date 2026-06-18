//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::env;
use std::fs::{File, OpenOptions};
use std::io::{self, Cursor, Read, Write};
use std::path::{Path, PathBuf};
use std::process::{Command, ExitCode, Stdio};
use std::time::{Duration, Instant};

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use ipp::prelude::*;
use num_traits::ToPrimitive;

/// lp - send files to a printer
#[derive(Parser)]
#[command(version, about = gettext("lp - send files to a printer"))]
struct Args {
    #[arg(short = 'c', help = gettext("Copy files to spool directory before printing"))]
    _copy: bool,

    #[arg(short = 'd', help = gettext("Printer destination (IPP URI)"))]
    dest: Option<String>,

    // Upper-bounded to i32::MAX: the IPP `copies` attribute is a signed 32-bit
    // integer, so a larger value would wrap negative when sent.
    #[arg(short = 'n', default_value = "1", value_parser = clap::value_parser!(u32).range(1..=i64::from(i32::MAX)), help = gettext("Number of copies to print"))]
    copies: u32,

    #[arg(short = 'm', help = gettext("Send mail after printing"))]
    mail: bool,

    #[arg(short = 's', help = gettext("Suppress messages (no request ID output)"))]
    silent: bool,

    #[arg(short = 'w', help = gettext("Write to terminal after printing"))]
    write_terminal: bool,

    #[arg(short = 'o', action = clap::ArgAction::Append, help = gettext("Printer-dependent options"))]
    options: Vec<String>,

    #[arg(short = 't', help = gettext("Title for the banner page"))]
    title: Option<String>,

    #[arg(help = gettext("Files to print (use '-' for stdin)"))]
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

/// Resolve a destination string into an IPP URI.
///
/// A value beginning with `ipp://` is used verbatim. Any other value is treated
/// as a bare printer name (the historical System V / BSD `LPDEST`/`PRINTER`
/// form) and resolved against the local IPP server as
/// `ipp://localhost/printers/<name>`. Note: TLS (`ipps://`) is intentionally
/// not supported (minimal dependencies).
fn resolve_uri(dest: &str) -> Result<Uri, String> {
    if dest.starts_with("ipp://") {
        return dest
            .parse::<Uri>()
            .map_err(|_| gettext("invalid destination URI"));
    }

    // Bare printer name: reject characters that would corrupt the URI path
    // (control characters, whitespace, or a path separator).
    if dest.is_empty()
        || dest
            .chars()
            .any(|c| c.is_control() || c.is_whitespace() || c == '/')
    {
        return Err(format!("{}: {}", gettext("invalid destination name"), dest));
    }

    format!("ipp://localhost/printers/{}", dest)
        .parse::<Uri>()
        .map_err(|_| format!("{}: {}", gettext("invalid destination name"), dest))
}

/// Read input data from a file or stdin.
/// Note: Copy mode (-c) is effectively always-on since IPP requires full data upload.
fn read_input(path: &Path) -> Result<Vec<u8>, io::Error> {
    let path_str = path.to_string_lossy();
    if path_str == "-" {
        let mut data = Vec::new();
        io::stdin().read_to_end(&mut data)?;
        Ok(data)
    } else {
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
        } else {
            // Not in IPP `name=value` attribute format: warn and skip.
            eprintln!(
                "lp: {}: {}",
                gettext("ignoring malformed -o option (expected name=value)"),
                opt
            );
        }
    }

    let operation = builder.build();

    // Create client and send
    let client = IppClient::new(uri.clone());
    let response = client
        .send(operation)
        .map_err(|e| format!("{} ({}): {}", gettext("printer error"), uri, e))?;

    // Check response status
    let status = response.header().status_code();
    if !status.is_success() {
        // Include the destination URI, consistent with the transport-error path
        // above. (Without it, this diagnostic varied by platform: a host with a
        // live IPP server — e.g. macOS CUPS — reaches here, while a host without
        // one fails at `send` above.)
        return Err(format!(
            "{} ({}): {} (0x{:04x})",
            gettext("printer error"),
            uri,
            status,
            status.to_u16().unwrap_or(0)
        ));
    }

    // Extract job-id from response. POSIX 103065 mandates a unique request ID,
    // so a response lacking job-id is an error rather than a fabricated "-0".
    let job_id = response
        .attributes()
        .groups_of(DelimiterTag::JobAttributes)
        .flat_map(|g| g.attributes().get("job-id"))
        .flat_map(|attr| attr.value().as_integer().copied())
        .next();

    job_id.ok_or_else(|| gettext("printer response missing job-id"))
}

/// Maximum time to wait for a submitted job to reach a terminal state.
const POLL_TIMEOUT: Duration = Duration::from_secs(120);
/// Interval between job-state polls.
const POLL_INTERVAL: Duration = Duration::from_secs(1);

/// Terminal outcome of waiting for a print job.
enum JobOutcome {
    Completed,
    Canceled,
    Aborted,
    /// Did not reach a terminal state within the timeout, or could not be queried.
    Unknown,
}

fn outcome_text(outcome: &JobOutcome) -> String {
    match outcome {
        JobOutcome::Completed => gettext("completed"),
        JobOutcome::Canceled => gettext("canceled"),
        JobOutcome::Aborted => gettext("aborted"),
        JobOutcome::Unknown => gettext("status unknown"),
    }
}

/// Poll a submitted job until it reaches a terminal state or the timeout
/// elapses. Honors the "after the files have been printed" wording for -m/-w.
fn wait_for_job(client: &IppClient, uri: &Uri, job_id: i32) -> JobOutcome {
    let deadline = Instant::now() + POLL_TIMEOUT;
    loop {
        let op = IppOperationBuilder::get_job_attributes(uri.clone(), job_id)
            .user_name(get_username())
            .build();
        let state = match client.send(op) {
            Ok(resp) => resp
                .attributes()
                .groups_of(DelimiterTag::JobAttributes)
                .flat_map(|g| g.attributes().get(IppAttribute::JOB_STATE))
                .flat_map(|attr| attr.value().as_enum().copied())
                .next(),
            // Cannot query the job; give up rather than spin.
            Err(_) => return JobOutcome::Unknown,
        };

        match state {
            Some(s) if s == JobState::Completed as i32 => return JobOutcome::Completed,
            Some(s) if s == JobState::Canceled as i32 => return JobOutcome::Canceled,
            Some(s) if s == JobState::Aborted as i32 => return JobOutcome::Aborted,
            _ => {}
        }

        if Instant::now() >= deadline {
            return JobOutcome::Unknown;
        }
        std::thread::sleep(POLL_INTERVAL);
    }
}

/// Reject recipient names with characters that could inject extra mail headers
/// or sendmail arguments.
fn recipient_is_safe(recipient: &str) -> bool {
    !recipient.is_empty()
        && recipient
            .chars()
            .all(|c| !c.is_control() && !c.is_whitespace() && c != '<' && c != '>')
}

/// Best-effort delivery of a completion mail via the local sendmail program.
fn send_mail(recipient: &str, subject: &str, body: &str) -> bool {
    if !recipient_is_safe(recipient) {
        return false;
    }
    let sendmail = [
        "/usr/sbin/sendmail",
        "/usr/lib/sendmail",
        "/usr/bin/sendmail",
    ]
    .into_iter()
    .find(|p| Path::new(p).exists());
    let sendmail = match sendmail {
        Some(p) => p,
        None => return false,
    };
    let mut child = match Command::new(sendmail)
        .args(["-t", "-oi"])
        .stdin(Stdio::piped())
        // Silence MTA diagnostics so they don't leak into lp's stderr.
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .spawn()
    {
        Ok(c) => c,
        Err(_) => return false,
    };
    if let Some(mut stdin) = child.stdin.take() {
        let message = format!("To: {recipient}\nSubject: {subject}\n\n{body}\n");
        let _ = stdin.write_all(message.as_bytes());
    }
    child.wait().map(|s| s.success()).unwrap_or(false)
}

/// Best-effort write of a message to the controlling terminal.
fn write_to_terminal(message: &str) {
    if let Ok(mut tty) = OpenOptions::new().write(true).open("/dev/tty") {
        let _ = tty.write_all(message.as_bytes());
    }
}

/// Wait for all submitted jobs to finish, then deliver -m / -w notifications.
fn notify_after_print(uri: &Uri, args: &Args, jobs: &[(i32, String)]) {
    let client = IppClient::new(uri.clone());
    let mut lines = Vec::with_capacity(jobs.len());
    for (job_id, request_id) in jobs {
        let outcome = wait_for_job(&client, uri, *job_id);
        lines.push(format!("{}: {}", request_id, outcome_text(&outcome)));
    }
    let body = lines.join("\n");

    if args.mail {
        let _ = send_mail(&get_username(), &gettext("lp: print job completed"), &body);
    }
    if args.write_terminal {
        let message = format!("lp: {}\n{}\n", gettext("print job completed"), body);
        write_to_terminal(&message);
    }
}

/// Process all operands. Returns `Ok(true)` if any file failed (so the caller
/// exits non-zero), `Ok(false)` if all succeeded, or `Err` for a fatal setup
/// error (bad destination) that aborts before any file is processed.
fn do_lp(mut args: Args) -> Result<bool, String> {
    // Get and validate destination (fatal — aborts before processing files).
    let dest = get_destination(&args)?;
    let uri = resolve_uri(&dest)?;

    // Determine input sources
    let files: Vec<PathBuf> = if args.files.is_empty() {
        vec![PathBuf::from("-")]
    } else {
        std::mem::take(&mut args.files)
    };

    // Per-file errors are reported but do not abort the remaining operands
    // (CONSEQUENCES OF ERRORS is Default; each file is independent).
    let mut had_error = false;
    // Successfully submitted jobs: (job-id, request-id) for -m/-w notification.
    let mut submitted: Vec<(i32, String)> = Vec::new();

    for file in &files {
        let file_name = if file.to_string_lossy() == "-" {
            None
        } else {
            file.file_name().and_then(|s| s.to_str())
        };

        // Read input data
        let data = match read_input(file) {
            Ok(data) => data,
            Err(e) => {
                eprintln!("lp: {} '{}': {}", gettext("cannot open"), file.display(), e);
                had_error = true;
                continue;
            }
        };

        // Send print job
        let job_id = match send_print_job(&uri, data, &args, file_name) {
            Ok(id) => id,
            Err(e) => {
                eprintln!("lp: {}", e);
                had_error = true;
                continue;
            }
        };

        // Output request ID unless silent
        if !args.silent {
            print!("{}", format_request_id(&dest, job_id));
        }
        submitted.push((job_id, format!("{}-{}", dest, job_id)));
    }

    // -m / -w: wait for the submitted jobs to finish printing, then notify.
    if (args.mail || args.write_terminal) && !submitted.is_empty() {
        notify_after_print(&uri, &args, &submitted);
    }

    Ok(had_error)
}

fn main() -> ExitCode {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs").ok();
    bind_textdomain_codeset("posixutils-rs", "UTF-8").ok();

    let args = Args::parse();

    match do_lp(args) {
        Ok(false) => ExitCode::SUCCESS,
        // Per-file errors were already reported inside do_lp.
        Ok(true) => ExitCode::FAILURE,
        Err(e) => {
            eprintln!("lp: {}", e);
            ExitCode::FAILURE
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn resolve_uri_bare_name() {
        let uri = resolve_uri("myprinter").unwrap();
        assert_eq!(uri.to_string(), "ipp://localhost/printers/myprinter");
    }

    #[test]
    fn resolve_uri_full_uri_passthrough() {
        let uri = resolve_uri("ipp://host/ipp/print").unwrap();
        assert_eq!(uri.to_string(), "ipp://host/ipp/print");
    }

    #[test]
    fn resolve_uri_rejects_bad_name() {
        assert!(resolve_uri("bad/name").is_err());
        assert!(resolve_uri("has space").is_err());
        assert!(resolve_uri("").is_err());
    }

    #[test]
    fn recipient_is_safe_accepts_plain_names() {
        assert!(recipient_is_safe("alice"));
        assert!(recipient_is_safe("user.name-1"));
    }

    #[test]
    fn recipient_is_safe_rejects_injection() {
        assert!(!recipient_is_safe(""));
        assert!(!recipient_is_safe("a b"));
        assert!(!recipient_is_safe("a\nb"));
        assert!(!recipient_is_safe("a<b"));
        assert!(!recipient_is_safe("a>b"));
    }
}
