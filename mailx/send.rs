//! Send mode implementation for mailx

use std::env;
use std::fs::{self, File};
use std::io::{self, BufRead, IsTerminal, Write};
use std::process::{Command, Stdio};

use crate::args::Args;
use crate::escapes::handle_escape;
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

/// Run send mode
pub fn send_mode(args: &Args, vars: &Variables) -> Result<(), String> {
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

    // Add subject from command line
    if let Some(ref subject) = args.subject {
        msg.subject = subject.clone();
    } else if is_tty && vars.get_bool("asksub") {
        // Prompt for subject
        print!("Subject: ");
        io::stdout().flush().map_err(|e| e.to_string())?;

        let mut subject = String::new();
        io::stdin()
            .read_line(&mut subject)
            .map_err(|e| e.to_string())?;
        msg.subject = subject.trim().to_string();
    }

    // Read message body
    let stdin = io::stdin();
    let escape_char = vars.escape_char();
    let mut interrupt_count = 0;

    if is_tty {
        // Interactive mode - handle escapes
        loop {
            let mut line = String::new();
            match stdin.lock().read_line(&mut line) {
                Ok(0) => break, // EOF
                Ok(_) => {}
                Err(e) => {
                    if e.kind() == io::ErrorKind::Interrupted {
                        interrupt_count += 1;
                        if interrupt_count >= 2 {
                            // Save to dead letter and abort
                            if vars.get_bool("save") && !msg.body.is_empty() {
                                save_dead_letter(&msg, vars);
                            }
                            return Err("Interrupt".to_string());
                        }
                        println!("(Interrupt -- one more to kill letter)");
                        continue;
                    }
                    return Err(e.to_string());
                }
            }

            interrupt_count = 0;

            // Check for escape character
            if line.starts_with(escape_char) && line.len() > 1 {
                let result = handle_escape(&line[1..], &mut msg, vars, None)?;
                if result.done {
                    if result.abort {
                        if vars.get_bool("save") && !msg.body.is_empty() {
                            save_dead_letter(&msg, vars);
                        }
                        return Err("Aborted".to_string());
                    }
                    break;
                }
                continue;
            }

            // Check for single period (if dot is set)
            if vars.get_bool("dot") && line.trim() == "." {
                break;
            }

            // Add to body
            msg.body.push_str(&line);
        }
    } else {
        // Non-interactive - just read stdin
        for line in stdin.lock().lines() {
            let line = line.map_err(|e| e.to_string())?;
            msg.body.push_str(&line);
            msg.body.push('\n');
        }
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

    let mut cmd = Command::new(sendmail);
    cmd.arg("-t") // Read recipients from headers
        .arg("-oi") // Don't treat . as end of message
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
            let filename = crate::message::extract_login(first_to);
            record_message(msg, filename, vars)?;
        }
    } else if let Some(record_file) = vars.get("record") {
        record_message(msg, record_file, vars)?;
    }

    Ok(())
}

/// Record a sent message to a file
fn record_message(msg: &ComposedMessage, filename: &str, vars: &Variables) -> Result<(), String> {
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
    let user = env::var("USER").unwrap_or_else(|_| "unknown".to_string());
    let date = chrono::Local::now().format("%a %b %e %H:%M:%S %Y");
    let from_line = format!("From {} {}", user, date);

    let mut file = fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(&path)
        .map_err(|e| format!("Cannot open {}: {}", path, e))?;

    writeln!(file, "{}", from_line).map_err(|e| e.to_string())?;
    write!(file, "{}", msg.format()).map_err(|e| e.to_string())?;
    if !msg.body.ends_with('\n') {
        writeln!(file).map_err(|e| e.to_string())?;
    }
    writeln!(file).map_err(|e| e.to_string())?;

    Ok(())
}

/// Save message to dead letter file
fn save_dead_letter(msg: &ComposedMessage, vars: &Variables) {
    let dead_path = vars.get("DEAD").map(|s| s.to_string()).unwrap_or_else(|| {
        let home = env::var("HOME").unwrap_or_else(|_| ".".to_string());
        format!("{}/dead.letter", home)
    });

    if let Ok(mut file) = File::create(&dead_path) {
        let _ = write!(file, "{}", msg.format());
        eprintln!("Message saved to {}", dead_path);
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
        // Reply to all recipients
        // Add original sender
        msg.add_to(original.from());

        // Add all To: recipients except ourselves
        let user = env::var("USER").unwrap_or_default();
        for addr in original.to().split(',') {
            let addr = addr.trim();
            if !addr.is_empty()
                && !addr.to_lowercase().contains(&user.to_lowercase())
                && !vars.is_alternate(addr)
            {
                msg.add_to(addr);
            }
        }

        // Add Cc: recipients
        if let Some(cc) = original.get_header("cc") {
            for addr in cc.split(',') {
                let addr = addr.trim();
                if !addr.is_empty()
                    && !addr.to_lowercase().contains(&user.to_lowercase())
                    && !vars.is_alternate(addr)
                {
                    msg.add_cc(addr);
                }
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
