//
// Copyright (c) 2025 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Send mode implementation for mailx

use std::io::{self, IsTerminal, Read, Write};
use std::process::{Command, Stdio};

use crate::args::Args;
use crate::commands::Interp;
use crate::error::{IoContext, Result};
use crate::message::Message;
use crate::variables::Variables;

/// A message being composed
#[derive(Debug, Default)]
pub struct ComposedMessage {
    pub to: Vec<String>,
    pub cc: Vec<String>,
    pub bcc: Vec<String>,
    pub subject: String,
    /// The body as entered or read, which need not be UTF-8.
    pub body: Vec<u8>,
    pub headers: Vec<(String, String)>,
}

impl ComposedMessage {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_to(&mut self, addr: &str) {
        push_address(&mut self.to, addr);
    }

    pub fn add_cc(&mut self, addr: &str) {
        push_address(&mut self.cc, addr);
    }

    pub fn add_bcc(&mut self, addr: &str) {
        push_address(&mut self.bcc, addr);
    }

    /// The header lines, in the order they are written.
    ///
    /// `Bcc:` is shown only to the user (`~p`): blind recipients are named
    /// on the envelope, never in the message itself.
    pub fn header_lines(&self, include_bcc: bool) -> Vec<String> {
        let mut lines = Vec::new();
        let mut add = |name: &str, value: &str| {
            if !value.is_empty() {
                lines.push(format!("{}: {}", name, value));
            }
        };
        add("To", &self.to.join(", "));
        add("Cc", &self.cc.join(", "));
        if include_bcc {
            add("Bcc", &self.bcc.join(", "));
        }
        add("Subject", &self.subject);
        for (name, value) in &self.headers {
            lines.push(format!("{}: {}", name, value));
        }
        lines
    }

    /// The message as handed to the delivery agent: headers, an empty line,
    /// and the body.
    pub fn format(&self) -> Vec<u8> {
        let mut out = Vec::new();
        for line in self.header_lines(false) {
            out.extend_from_slice(line.as_bytes());
            out.push(b'\n');
        }
        out.push(b'\n');
        out.extend_from_slice(&self.body);
        out
    }

    /// The message as a mailbox entry under the envelope line `from_line`.
    pub fn to_message(&self, from_line: String) -> Message {
        let mut msg = Message::new();
        msg.from_line = from_line.into_bytes();
        msg.header_lines = self
            .header_lines(false)
            .into_iter()
            .map(String::into_bytes)
            .collect();
        msg.body = self.body.clone();
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

/// Add `addr` to a recipient list unless it is empty or already there.
///
/// An address becomes a header line, so a line break in it is folded to a
/// space: see [`single_line`].
pub(crate) fn push_address(list: &mut Vec<String>, addr: &str) {
    let addr = single_line(addr.trim());
    if !addr.is_empty() && !list.contains(&addr) {
        list.push(addr);
    }
}

/// `s` with every CR and LF replaced by a space.
///
/// A header field value is one line. Subjects and addresses given as
/// command-line operands are copied into the header block verbatim, so a
/// script passing `-s "$subject"` from an untrusted source let a line break in
/// it add arbitrary header fields, end the header block early, or -- in the
/// `record` file -- start a forged mbox message with a `From ` line.
fn single_line(s: &str) -> String {
    s.replace(['\r', '\n'], " ")
}

/// Prompt for the header fields the `asksub`, `askcc`, and `askbcc` variables
/// request (spec 104582-104583, and the askcc/askbcc entries).
pub(crate) fn prompt_optional_headers(msg: &mut ComposedMessage, vars: &Variables) -> Result<()> {
    if msg.subject.is_empty() && vars.is_set("asksub") {
        if let Some(v) = crate::util::prompt_field("Subject", "")? {
            msg.subject = v;
        }
    }
    if vars.is_set("askcc") {
        if let Some(v) = crate::util::prompt_field("Cc", "")? {
            for addr in crate::util::addresses(&v) {
                msg.add_cc(addr);
            }
        }
    }
    if vars.is_set("askbcc") {
        if let Some(v) = crate::util::prompt_field("Bcc", "")? {
            for addr in crate::util::addresses(&v) {
                msg.add_bcc(addr);
            }
        }
    }
    Ok(())
}

/// Run send mode
pub fn send_mode(args: &Args, vars: &mut Variables, interp: &mut Interp) -> Result<()> {
    let is_tty = io::stdin().is_terminal();

    let mut msg = ComposedMessage::new();

    for addr in args.addresses.iter().flat_map(|a| vars.recipients(a)) {
        msg.add_to(&addr);
    }

    // A -s subject supplies the Subject header; otherwise `asksub` may prompt
    // for it. `askcc`/`askbcc` are asked either way.
    if let Some(ref subject) = args.subject {
        msg.subject = single_line(subject);
    }
    if is_tty {
        prompt_optional_headers(&mut msg, vars)?;
    }

    // Read message body
    let stdin = io::stdin();

    if is_tty {
        crate::compose::compose_body(&mut msg, None, vars, interp)?;
    } else {
        // Non-interactive - just read stdin, as bytes: mail need not be UTF-8,
        // and reading it as text refused any body that was not.
        let mut input = Vec::new();
        stdin
            .lock()
            .read_to_end(&mut input)
            .map_err(|e| e.to_string())?;
        for line in crate::util::byte_lines(&input) {
            msg.body.extend_from_slice(line);
            msg.body.push(b'\n');
        }
    }

    // -E: discard a message with an empty body without sending it.
    if args.discard_empty && msg.body.trim_ascii().is_empty() {
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
) -> Result<()> {
    let recipients = msg.all_recipients();
    if recipients.is_empty() {
        return Err("No recipients".into());
    }

    // Debug mode - don't actually send, just print diagnostics
    if vars.is_set("debug") {
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
    let recipients: Vec<String> = msg
        .all_recipients()
        .into_iter()
        .map(crate::message::extract_address)
        .filter(|a| !a.is_empty())
        .collect();
    if recipients.is_empty() {
        return Err("No recipients specified".into());
    }

    let mut cmd = Command::new(sendmail);
    cmd.arg("-oi") // Don't treat . as end of message
        .arg("--")
        .args(&recipients)
        .stdin(Stdio::piped())
        .stdout(Stdio::null())
        .stderr(Stdio::piped());

    let mut child = cmd.spawn().context("Failed to run sendmail")?;

    {
        let stdin = child.stdin.as_mut().ok_or("Failed to open stdin")?;
        stdin
            .write_all(&msg.format())
            .context("Failed to write message")?;
    }

    // Wait for sendmail if sendwait is set
    if vars.is_set("sendwait") {
        let output = child
            .wait_with_output()
            .context("Failed to wait for sendmail")?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(format!("sendmail failed: {}", stderr).into());
        }
    }

    // Record the message if requested
    if record_to_recipient {
        if let Some(first_to) = msg.to.first() {
            let filename = crate::message::author_filename(first_to)?;
            record_message(msg, &filename, vars)?;
        }
    } else if let Some(record_file) = vars.get_nonempty("record") {
        record_message(msg, record_file, vars)?;
    }

    Ok(())
}

/// Append an outgoing message to a record file, in mbox format.
///
/// `record`, and the author-named file `followup` writes, are ordinary
/// mailboxes that get read back with `mailx -f`, so they are written by the
/// same writer, with the same locking, as any other mailbox.
pub(crate) fn record_message(
    msg: &ComposedMessage,
    filename: &str,
    vars: &Variables,
) -> Result<()> {
    let in_folder = vars.is_set("outfolder") && !filename.starts_with('/');
    let path = in_folder
        .then(|| crate::filename::in_folder(filename, vars))
        .flatten()
        .unwrap_or_else(|| filename.to_string());

    let user = crate::util::user_login_or_unknown();
    let date = chrono::Local::now().format("%a %b %e %H:%M:%S %Y");
    let entry = msg.to_message(format!("From {} {}", user, date));

    crate::mailbox::append_messages_to_file(&path, &[&entry], true).context(&path)?;
    Ok(())
}

/// Save message to dead letter file
pub fn save_dead_letter(msg: &ComposedMessage, vars: &Variables) {
    let dead_path = crate::util::dead_letter_path(vars);

    let created = crate::util::private_open()
        .write(true)
        .create(true)
        .truncate(true)
        .open(&dead_path);
    if let Ok(mut file) = created {
        let _ = file.write_all(&msg.format());
        // Informative notice -> stdout (spec STDOUT, 104412-104414).
        println!("Message saved to {}", dead_path);
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
        // Lists are split with `util::addresses`: a bare `split(',')` broke
        // `"Smith, John" <j@x>` into two recipients, neither of them valid.
        if let Some(reply_to) = original.get_header("reply-to") {
            for addr in crate::util::addresses(reply_to) {
                msg.add_to(addr);
            }
        } else {
            msg.add_to(original.from());
        }

        // Add all To: recipients.  Unless metoo is set, the user's own login
        // and any declared alternates come out of the list (spec 104926-104928).
        let include_self = vars.is_set("metoo");

        for addr in crate::util::addresses(original.to()) {
            if !include_self && vars.is_me(addr) {
                continue;
            }
            msg.add_to(addr);
        }

        // Add Cc: recipients
        if let Some(cc) = original.get_header("cc") {
            for addr in crate::util::addresses(cc) {
                if !include_self && vars.is_me(addr) {
                    continue;
                }
                msg.add_cc(addr);
            }
        }
    } else {
        // Reply only to sender
        // Check Reply-To first
        // Reply-To may name several addresses; each is a recipient.
        if let Some(reply_to) = original.get_header("reply-to") {
            for addr in crate::util::addresses(reply_to) {
                msg.add_to(addr);
            }
        } else {
            msg.add_to(original.from());
        }
    }

    msg
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A record entry is an mbox message: envelope, headers without `Bcc:`,
    /// an empty line, the `From `-quoted body with each line terminated,
    /// and the separating empty line.
    #[test]
    fn record_writes_an_mbox_entry() {
        let dir = plib::tmp::TempDir::new().expect("temp dir");
        let path = dir.path().join("record");
        let mut msg = ComposedMessage::new();
        msg.add_to("to@example.com");
        msg.add_bcc("hidden@example.com");
        msg.subject = "Subj".to_string();
        msg.body = b"From the start\n>From quoted\nline\r\nlast".to_vec();

        record_message(&msg, path.to_str().unwrap(), &Variables::new()).expect("record");

        let written = std::fs::read(&path).expect("read record");
        let (envelope, rest) = written.split_at(written.iter().position(|&b| b == b'\n').unwrap());
        assert!(envelope.starts_with(b"From "), "{:?}", envelope);
        assert_eq!(
            String::from_utf8_lossy(rest),
            "\nTo: to@example.com\nSubject: Subj\n\n>From the start\n>>From quoted\nline\nlast\n\n"
        );
    }
}
