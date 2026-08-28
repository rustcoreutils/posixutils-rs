//
// Copyright (c) 2025 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Send mode implementation for mailx

use std::env;
use std::fs::{self, File};
use std::io::{self, BufRead, IsTerminal, Write};
use std::process::{Command, Stdio};

use crate::args::Args;
use crate::message::Message;
use crate::variables::Variables;

/// A message being composed
#[derive(Debug, Default)]
pub struct ComposedMessage {
    pub to: Vec<String>,
    pub cc: Vec<String>,
    pub bcc: Vec<String>,
    pub subject: String,
    pub body: String,
    pub headers: Vec<(String, String)>,
}

impl ComposedMessage {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_to(&mut self, addr: &str) {
        let addr = addr.trim();
        if !addr.is_empty() && !self.to.contains(&addr.to_string()) {
            self.to.push(addr.to_string());
        }
    }

    pub fn add_cc(&mut self, addr: &str) {
        let addr = addr.trim();
        if !addr.is_empty() && !self.cc.contains(&addr.to_string()) {
            self.cc.push(addr.to_string());
        }
    }

    pub fn add_bcc(&mut self, addr: &str) {
        let addr = addr.trim();
        if !addr.is_empty() && !self.bcc.contains(&addr.to_string()) {
            self.bcc.push(addr.to_string());
        }
    }

    /// Format the message for sending
    pub fn format(&self) -> String {
        let mut msg = String::new();

        if !self.to.is_empty() {
            msg.push_str(&format!("To: {}\n", self.to.join(", ")));
        }
        if !self.cc.is_empty() {
            msg.push_str(&format!("Cc: {}\n", self.cc.join(", ")));
        }
        if !self.subject.is_empty() {
            msg.push_str(&format!("Subject: {}\n", self.subject));
        }

        for (name, value) in &self.headers {
            msg.push_str(&format!("{}: {}\n", name, value));
        }

        msg.push('\n');
        msg.push_str(&self.body);

        msg
    }

    /// Get all recipients
    pub fn all_recipients(&self) -> Vec<&str> {
        let mut recipients: Vec<&str> = self.to.iter().map(|s| s.as_str()).collect();
        recipients.extend(self.cc.iter().map(|s| s.as_str()));
        recipients.extend(self.bcc.iter().map(|s| s.as_str()));
        recipients
    }
}

/// Prompt for the header fields the `asksub`, `askcc`, and `askbcc` variables
/// request (spec 104582-104583, and the askcc/askbcc entries).
pub(crate) fn prompt_optional_headers(
    msg: &mut ComposedMessage,
    vars: &Variables,
) -> Result<(), String> {
    if msg.subject.is_empty() && vars.get_bool("asksub") {
        if let Some(v) = crate::util::prompt_field("Subject", "")? {
            msg.subject = v;
        }
    }
    if vars.get_bool("askcc") {
        if let Some(v) = crate::util::prompt_field("Cc", "")? {
            for addr in crate::util::addresses(&v) {
                msg.add_cc(addr);
            }
        }
    }
    if vars.get_bool("askbcc") {
        if let Some(v) = crate::util::prompt_field("Bcc", "")? {
            for addr in crate::util::addresses(&v) {
                msg.add_bcc(addr);
            }
        }
    }
    Ok(())
}

/// Run send mode
pub fn send_mode(args: &Args, vars: &mut Variables) -> Result<(), String> {
    let is_tty = io::stdin().is_terminal();

    let mut msg = ComposedMessage::new();

    // Add addresses from command line
    for addr in &args.addresses {
        // Expand aliases
        let expanded = vars.expand_alias(addr);
        if expanded.is_empty() {
            msg.add_to(addr);
        } else {
            for a in expanded {
                msg.add_to(&a);
            }
        }
    }

    // A -s subject supplies the Subject header; otherwise `asksub` may prompt
    // for it. `askcc`/`askbcc` are asked either way.
    if let Some(ref subject) = args.subject {
        msg.subject = subject.clone();
    }
    if is_tty {
        prompt_optional_headers(&mut msg, vars)?;
    }

    // Read message body
    let stdin = io::stdin();

    if is_tty {
        crate::commands::compose_body(&mut msg, None, vars)?;
    } else {
        // Non-interactive - just read stdin
        for line in stdin.lock().lines() {
            let line = line.map_err(|e| e.to_string())?;
            msg.body.push_str(&line);
            msg.body.push('\n');
        }
    }

    // -E: discard a message with an empty body without sending it.
    if args.discard_empty && msg.body.trim().is_empty() {
        return Ok(());
    }

    // Send the message
    send_message(&msg, vars, args.record_to_recipient)?;

    Ok(())
}

/// Send a composed message
pub fn send_message(
    msg: &ComposedMessage,
    vars: &Variables,
    record_to_recipient: bool,
) -> Result<(), String> {
    let recipients = msg.all_recipients();
    if recipients.is_empty() {
        return Err("No recipients".to_string());
    }

    // Debug mode - don't actually send, just print diagnostics
    if vars.get_bool("debug") {
        eprintln!("--- Debug mode: message not sent ---");
        // The envelope is what the delivery software is handed; the headers are
        // what the recipients see. Blind recipients appear in the first and not
        // the second, so the two are reported separately.
        eprintln!(
            "Envelope: {}",
            msg.all_recipients()
                .into_iter()
                .map(crate::message::extract_address)
                .collect::<Vec<_>>()
                .join(", ")
        );
        eprintln!("To: {}", msg.to.join(", "));
        if !msg.cc.is_empty() {
            eprintln!("Cc: {}", msg.cc.join(", "));
        }
        if !msg.bcc.is_empty() {
            eprintln!("Bcc(envelope only): {}", msg.bcc.join(", "));
        }
        eprintln!("Subject: {}", msg.subject);
        eprintln!("Body: {} bytes", msg.body.len());
        eprintln!("---");
        return Ok(());
    }

    // Try to use sendmail
    let sendmail_paths = [
        "/usr/sbin/sendmail",
        "/usr/lib/sendmail",
        "/usr/bin/sendmail",
    ];

    let sendmail = sendmail_paths
        .iter()
        .find(|p| std::path::Path::new(p).exists())
        .ok_or("Cannot find sendmail")?;

    // Name the recipients on the command line rather than asking sendmail to
    // read them from the headers with `-t`. `format()` deliberately emits no
    // `Bcc:` header, so under `-t` the blind recipients were simply never
    // delivered to; naming them here also means a blind address cannot leak
    // into the delivered message however the mail system is configured.
    // Envelope operands are bare addresses. `compose_reply` fills the recipient
    // lists from header values, so they carry display names -- and a display
    // name containing a comma or a quote is not something to hand an MTA as a
    // recipient.
    let recipients: Vec<&str> = msg
        .all_recipients()
        .into_iter()
        .map(crate::message::extract_address)
        .filter(|a| !a.is_empty())
        .collect();
    if recipients.is_empty() {
        return Err("No recipients specified".to_string());
    }

    let mut cmd = Command::new(sendmail);
    cmd.arg("-oi") // Don't treat . as end of message
        .arg("--")
        .args(&recipients)
        .stdin(Stdio::piped())
        .stdout(Stdio::null())
        .stderr(Stdio::piped());

    let mut child = cmd
        .spawn()
        .map_err(|e| format!("Failed to run sendmail: {}", e))?;

    {
        let stdin = child.stdin.as_mut().ok_or("Failed to open stdin")?;
        stdin
            .write_all(msg.format().as_bytes())
            .map_err(|e| format!("Failed to write message: {}", e))?;
    }

    // Wait for sendmail if sendwait is set
    if vars.get_bool("sendwait") {
        let output = child
            .wait_with_output()
            .map_err(|e| format!("Failed to wait for sendmail: {}", e))?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(format!("sendmail failed: {}", stderr));
        }
    }

    // Record the message if requested
    if record_to_recipient {
        if let Some(first_to) = msg.to.first() {
            let filename = crate::message::author_filename(first_to)?;
            record_message(msg, filename, vars)?;
        }
    } else if let Some(record_file) = vars.get("record") {
        record_message(msg, record_file, vars)?;
    }

    Ok(())
}

/// Record a sent message to a file
/// Append an outgoing message to a record file, in mbox format.
///
/// `record`, and the author-named file `followup` writes, are ordinary
/// mailboxes that get read back with `mailx -f`. They are written through the
/// same `From `-quoting and separator rules as any other mailbox: writing the
/// body verbatim meant a composed line beginning `From ` split the entry in two
/// on the next read, and a missing trailing empty line ran consecutive records
/// together.
pub(crate) fn record_message(
    msg: &ComposedMessage,
    filename: &str,
    vars: &Variables,
) -> Result<(), String> {
    let path = if vars.get_bool("outfolder") && !filename.starts_with('/') {
        if let Some(folder) = vars.get("folder") {
            let folder = expand_folder(folder);
            format!("{}/{}", folder, filename)
        } else {
            filename.to_string()
        }
    } else {
        filename.to_string()
    };

    // Create "From " line
    let user = crate::user_login();
    let user = if user.is_empty() { "unknown" } else { &user };
    let date = chrono::Local::now().format("%a %b %e %H:%M:%S %Y");

    let mut file = fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(&path)
        .map_err(|e| format!("Cannot open {}: {}", path, e))?;

    let write = || -> io::Result<()> {
        let mut file = &file;
        writeln!(file, "From {} {}", user, date)?;
        let formatted = msg.format();
        let (headers, body) = match formatted.split_once("\n\n") {
            Some((h, b)) => (h, b),
            None => (formatted.as_str(), ""),
        };
        writeln!(file, "{}", headers)?;
        writeln!(file)?;
        for line in body.lines() {
            if crate::mailbox::needs_from_quoting(line) {
                write!(file, ">")?;
            }
            writeln!(file, "{}", line)?;
        }
        writeln!(file)?;
        Ok(())
    };
    write().map_err(|e| format!("{}: {}", path, e))?;
    file.flush().map_err(|e| format!("{}: {}", path, e))?;

    Ok(())
}

/// Save message to dead letter file
pub fn save_dead_letter(msg: &ComposedMessage, vars: &Variables) {
    let dead_path = crate::util::dead_letter_path(vars);

    if let Ok(mut file) = File::create(&dead_path) {
        let _ = write!(file, "{}", msg.format());
        // Informative notice -> stdout (spec STDOUT, 104412-104414).
        println!("Message saved to {}", dead_path);
    }
}

fn expand_folder(folder: &str) -> String {
    if folder.starts_with('/') {
        folder.to_string()
    } else {
        let home = env::var("HOME").unwrap_or_else(|_| ".".to_string());
        format!("{}/{}", home, folder)
    }
}

/// Compose a reply message
pub fn compose_reply(original: &Message, reply_all: bool, vars: &Variables) -> ComposedMessage {
    let mut msg = ComposedMessage::new();

    // Set subject
    let original_subject = original.subject();
    if original_subject.to_lowercase().starts_with("re:") {
        msg.subject = original_subject.to_string();
    } else {
        msg.subject = format!("Re: {}", original_subject);
    }

    // Add recipients
    if reply_all {
        // Reply to all recipients.  The sender portion comes from Reply-To when
        // present, otherwise from From (spec 104911-104916: in the lowercase
        // form, From/To/Cc are used only when there is no Reply-To).
        if let Some(reply_to) = original.get_header("reply-to") {
            for addr in reply_to.split(',') {
                msg.add_to(addr);
            }
        } else {
            msg.add_to(original.from());
        }

        // Add all To: recipients.  Unless metoo is set, the user's own login
        // and any declared alternates come out of the list (spec 104926-104928).
        let include_self = vars.get_bool("metoo");

        for addr in original.to().split(',') {
            let addr = addr.trim();
            if addr.is_empty() {
                continue;
            }
            if !include_self && vars.is_me(addr) {
                continue;
            }
            msg.add_to(addr);
        }

        // Add Cc: recipients
        if let Some(cc) = original.get_header("cc") {
            for addr in cc.split(',') {
                let addr = addr.trim();
                if addr.is_empty() {
                    continue;
                }
                if !include_self && vars.is_me(addr) {
                    continue;
                }
                msg.add_cc(addr);
            }
        }
    } else {
        // Reply only to sender
        // Check Reply-To first
        if let Some(reply_to) = original.get_header("reply-to") {
            msg.add_to(reply_to);
        } else {
            msg.add_to(original.from());
        }
    }

    msg
}
