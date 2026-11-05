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
use crate::message::Headers;
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

    /// Every recipient: To, Cc and Bcc.
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

    let stdin = io::stdin();

    if is_tty {
        crate::compose::compose_body(&mut msg, None, vars, interp)?;
    } else {
        // Not a terminal: the body is all of standard input, as bytes, since
        // mail need not be UTF-8.
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

    send_message(&msg, vars, args.record_to_recipient)?;

    Ok(())
}

/// Send a composed message, then record it as `-F` or `record` asks.
pub fn send_message(
    msg: &ComposedMessage,
    vars: &Variables,
    record_to_recipient: bool,
) -> Result<()> {
    if msg.all_recipients().is_empty() {
        return Err("No recipients".into());
    }
    if vars.is_set("debug") {
        report_unsent(msg);
        return Ok(());
    }

    deliver(msg, vars)?;

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

/// `debug`: report on standard error what would have been sent.
///
/// The envelope is what the delivery software is handed; the headers are
/// what the recipients see. Blind recipients appear in the first and not the
/// second, so the two are reported separately.
fn report_unsent(msg: &ComposedMessage) {
    eprintln!("--- Debug mode: message not sent ---");
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
}

const SENDMAIL_PATHS: [&str; 3] = [
    "/usr/sbin/sendmail",
    "/usr/lib/sendmail",
    "/usr/bin/sendmail",
];

/// Hand the message to sendmail, waiting for it when `sendwait` is set.
///
/// The recipients are named on sendmail's command line rather than read from
/// the headers with `-t`: the message carries no `Bcc:` header, so under `-t`
/// blind recipients would get nothing, and naming them keeps a blind address
/// out of the delivered message however the mail system is set up.
/// They are bare addresses, since a display name holding a comma or a quote
/// is not something to hand an MTA as a recipient.
///
/// The `sendmail` variable names the program, as in BSD mailx; unset, the
/// usual places are searched.
fn deliver(msg: &ComposedMessage, vars: &Variables) -> Result<()> {
    let sendmail = match vars.get_nonempty("sendmail") {
        Some(program) => program,
        None => SENDMAIL_PATHS
            .iter()
            .copied()
            .find(|p| std::path::Path::new(p).exists())
            .ok_or("Cannot find sendmail")?,
    };

    let recipients: Vec<String> = msg
        .all_recipients()
        .into_iter()
        .map(crate::message::extract_address)
        .filter(|a| !a.is_empty())
        .collect();
    if recipients.is_empty() {
        return Err("No recipients specified".into());
    }

    let mut child = Command::new(sendmail)
        .arg("-oi") // a line of "." does not end the message
        .arg("--")
        .args(&recipients)
        .stdin(Stdio::piped())
        .stdout(Stdio::null())
        .stderr(Stdio::piped())
        .spawn()
        .context("Failed to run sendmail")?;

    child
        .stdin
        .as_mut()
        .ok_or("Failed to open stdin")?
        .write_all(&msg.format())
        .context("Failed to write message")?;

    if vars.is_set("sendwait") {
        let output = child
            .wait_with_output()
            .context("Failed to wait for sendmail")?;
        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(format!("sendmail failed: {}", stderr).into());
        }
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
    let from_line = format!("From {} {}", user, date);
    crate::mailbox::append_to_file(&path, |w| {
        crate::mbox::write_composed(w, &from_line, &msg.header_lines(false), &msg.body)
    })
    .context(&path)?;
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

/// Who a reply is addressed to.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReplyForm {
    /// The lowercase `reply` form: the sender of one message and its other
    /// recipients.
    Group,
    /// The capitalized `Reply` form: the sender of each message listed.
    Senders,
}

/// A reply to `originals`, addressed as `form` says, its subject that of
/// the first with `Re: ` before it unless it already begins so.
pub fn compose_reply(originals: &[Headers], form: ReplyForm, vars: &Variables) -> ComposedMessage {
    let mut msg = ComposedMessage::new();
    let Some(first) = originals.first() else {
        return msg;
    };
    let subject = first.subject();
    msg.subject = if subject.to_lowercase().starts_with("re:") {
        subject.to_string()
    } else {
        format!("Re: {}", subject)
    };

    for original in originals {
        add_sender(&mut msg, original);
    }
    if form == ReplyForm::Group {
        // Whether To and Cc join a Reply-To is implementation-defined
        // (spec 104911-104916); here they do. Unless `metoo` is set, the
        // user's own login and alternates come out (spec 104926-104928).
        let include = |addr: &str| vars.is_set("metoo") || !vars.is_me(addr);
        for addr in crate::util::addresses(first.to()).filter(|a| include(a)) {
            msg.add_to(addr);
        }
        let cc = first.get_header("cc").unwrap_or("");
        for addr in crate::util::addresses(cc).filter(|a| include(a)) {
            msg.add_cc(addr);
        }
    }
    msg
}

/// Address `msg` to the sender of `original`: its Reply-To addresses, or
/// without one its From.
fn add_sender(msg: &mut ComposedMessage, original: &Headers) {
    match original.get_header("reply-to") {
        Some(reply_to) => crate::util::addresses(reply_to).for_each(|a| msg.add_to(a)),
        None => msg.add_to(original.from()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A record entry is an mbox message: envelope, headers without `Bcc:`,
    /// an empty line, the `From `-quoted body as sent with each line
    /// terminated, and the separating empty line.
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
            "\nTo: to@example.com\nSubject: Subj\n\n>From the start\n>>From quoted\nline\r\nlast\n\n"
        );
    }
}
