//! Mailbox handling for mailx - mbox format parsing and writing

use std::fs::{self, File, OpenOptions};
use std::io::{self, BufRead, BufReader, Write};

use crate::message::{Message, MessageState};
use crate::variables::Variables;

/// A mailbox containing messages
#[derive(Debug)]
pub struct Mailbox {
    /// Path to the mailbox file
    pub path: String,
    /// Messages in the mailbox
    pub messages: Vec<Message>,
    /// Current message index (1-based, 0 means no current message)
    pub current: usize,
    /// Whether this is the system mailbox
    pub is_system_mailbox: bool,
    /// Whether the mailbox has been modified
    pub modified: bool,
}

impl Mailbox {
    /// Create a new empty mailbox
    pub fn new(path: String) -> Self {
        Mailbox {
            path,
            messages: Vec::new(),
            current: 0,
            is_system_mailbox: false,
            modified: false,
        }
    }

    /// Load a mailbox from a file
    pub fn load(path: &str) -> io::Result<Self> {
        let file = File::open(path)?;
        let reader = BufReader::new(file);
        let mut mailbox = Mailbox::new(path.to_string());

        let mut current_msg: Option<Message> = None;
        let mut in_headers = false;
        let mut header_continuation = false;
        let mut last_header_key = String::new();

        for line in reader.lines() {
            let line = line?;

            // Check for message separator (From line)
            if line.starts_with("From ") && !in_headers {
                // Save previous message if any
                if let Some(msg) = current_msg.take() {
                    mailbox.messages.push(msg);
                }

                // Start new message
                let mut msg = Message::new();
                msg.from_line = line;
                msg.state = MessageState::Unread; // Assume unread for now
                current_msg = Some(msg);
                in_headers = true;
                header_continuation = false;
                continue;
            }

            if let Some(ref mut msg) = current_msg {
                if in_headers {
                    if line.is_empty() {
                        // End of headers
                        in_headers = false;
                    } else if line.starts_with(' ') || line.starts_with('\t') {
                        // Header continuation
                        if header_continuation && !last_header_key.is_empty() {
                            if let Some(val) = msg.headers.get_mut(&last_header_key) {
                                val.push(' ');
                                val.push_str(line.trim());
                            }
                        }
                        // Append to last header line
                        if let Some(last) = msg.header_lines.last_mut() {
                            last.push('\n');
                            last.push_str(&line);
                        }
                    } else if let Some(colon_pos) = line.find(':') {
                        // New header
                        let key = line[..colon_pos].to_lowercase();
                        let value = line[colon_pos + 1..].trim().to_string();

                        // Check for Status header to determine message state
                        if key == "status" && (value.contains('R') || value.contains('O')) {
                            msg.state = MessageState::Read;
                        }

                        msg.headers.insert(key.clone(), value);
                        msg.header_lines.push(line);
                        last_header_key = key;
                        header_continuation = true;
                    } else {
                        // Malformed header, treat as body start
                        in_headers = false;
                        msg.body.push_str(&line);
                        msg.body.push('\n');
                    }
                } else {
                    // Body
                    // Handle escaped From lines (>From)
                    if line.starts_with(">From ") {
                        msg.body.push_str(&line[1..]);
                    } else {
                        msg.body.push_str(&line);
                    }
                    msg.body.push('\n');
                }
            }
        }

        // Don't forget the last message
        if let Some(msg) = current_msg {
            mailbox.messages.push(msg);
        }

        // Set current to first new/unread message, or first message
        mailbox.current = mailbox
            .messages
            .iter()
            .position(|m| m.state == MessageState::New || m.state == MessageState::Unread)
            .map(|i| i + 1)
            .unwrap_or(if mailbox.messages.is_empty() { 0 } else { 1 });

        Ok(mailbox)
    }

    /// Get the number of messages
    pub fn message_count(&self) -> usize {
        self.messages.len()
    }

    /// Get number of undeleted messages
    pub fn undeleted_count(&self) -> usize {
        self.messages
            .iter()
            .filter(|m| m.state != MessageState::Deleted)
            .count()
    }

    /// Set whether this is the system mailbox
    pub fn set_is_system_mailbox(&mut self, is_system: bool) {
        self.is_system_mailbox = is_system;
    }

    /// Get a message by number (1-based)
    pub fn get(&self, num: usize) -> Option<&Message> {
        if num > 0 && num <= self.messages.len() {
            Some(&self.messages[num - 1])
        } else {
            None
        }
    }

    /// Get a mutable message by number (1-based)
    pub fn get_mut(&mut self, num: usize) -> Option<&mut Message> {
        if num > 0 && num <= self.messages.len() {
            Some(&mut self.messages[num - 1])
        } else {
            None
        }
    }

    /// Find first undeleted message after the given number
    pub fn next_undeleted(&self, after: usize) -> Option<usize> {
        for i in after..self.messages.len() {
            if self.messages[i].state != MessageState::Deleted {
                return Some(i + 1);
            }
        }
        None
    }

    /// Find first undeleted message before the given number
    pub fn prev_undeleted(&self, before: usize) -> Option<usize> {
        if before <= 1 {
            return None;
        }
        for i in (0..before - 1).rev() {
            if self.messages[i].state != MessageState::Deleted {
                return Some(i + 1);
            }
        }
        None
    }

    /// Find first deleted message after current for undelete
    pub fn next_deleted(&self, after: usize) -> Option<usize> {
        for i in after..self.messages.len() {
            if self.messages[i].state == MessageState::Deleted {
                return Some(i + 1);
            }
        }
        None
    }

    /// Find first deleted message before current for undelete
    pub fn prev_deleted(&self, before: usize) -> Option<usize> {
        if before <= 1 {
            return None;
        }
        for i in (0..before - 1).rev() {
            if self.messages[i].state == MessageState::Deleted {
                return Some(i + 1);
            }
        }
        None
    }

    /// Print header summary for messages
    pub fn print_headers(&self, msg_nums: Option<&[usize]>, vars: &Variables) {
        let screen = vars.get_number("screen").unwrap_or(20) as usize;
        let show_to = vars.get_bool("showto");
        let user = std::env::var("USER").unwrap_or_default();

        let nums: Vec<usize> = msg_nums
            .map(|n| n.to_vec())
            .unwrap_or_else(|| (1..=self.messages.len()).collect());

        for &num in nums.iter().take(screen) {
            if let Some(msg) = self.get(num) {
                let current_marker = if num == self.current { '>' } else { ' ' };
                let state_char = msg.state.status_char();

                // Decide whether to show To or From
                let address_field = if show_to && msg.from().contains(&user) {
                    format!("To {}", truncate(msg.to(), 18))
                } else {
                    msg.short_from()
                };

                let date = msg.short_date();
                let lines = msg.line_count();
                let size = msg.size();
                let subject = truncate(msg.subject(), 25);

                println!(
                    "{}{}{:>4}  {:<18}  {:>6}  {:>5}/{:<5}  {}",
                    current_marker, state_char, num, address_field, date, lines, size, subject
                );
            }
        }
    }

    /// Handle quit - save read messages to mbox, delete saved, etc.
    pub fn quit(&mut self, vars: &Variables) -> Result<(), String> {
        if !self.modified
            && !self.messages.iter().any(|m| {
                m.state == MessageState::Read
                    || m.state == MessageState::Deleted
                    || m.state == MessageState::Saved
            })
        {
            return Ok(());
        }

        let hold = vars.get_bool("hold");
        let keepsave = vars.get_bool("keepsave");
        let keep = vars.get_bool("keep");

        // Collect messages for mbox
        let mut mbox_messages = Vec::new();
        let mut keep_messages = Vec::new();

        for msg in &self.messages {
            match msg.state {
                MessageState::Deleted => {
                    // Discard
                }
                MessageState::Saved => {
                    if self.is_system_mailbox && keepsave {
                        mbox_messages.push(msg.clone());
                    }
                    // Otherwise discard from current mailbox
                }
                MessageState::Read => {
                    if self.is_system_mailbox && !hold {
                        mbox_messages.push(msg.clone());
                    } else {
                        keep_messages.push(msg.clone());
                    }
                }
                MessageState::Preserved => {
                    keep_messages.push(msg.clone());
                }
                MessageState::New | MessageState::Unread => {
                    keep_messages.push(msg.clone());
                }
            }
        }

        // Save to mbox if needed
        if !mbox_messages.is_empty() {
            let mbox_path = get_mbox_path(vars);
            let append = vars.get_bool("append");
            save_messages_to_file(&mbox_path, &mbox_messages, append, true)
                .map_err(|e| format!("{}: {}", mbox_path, e))?;
        }

        // Rewrite the current mailbox with remaining messages
        if keep_messages.is_empty() && !keep {
            // Remove the mailbox file
            let _ = fs::remove_file(&self.path);
        } else if keep_messages.len() != self.messages.len()
            || self
                .messages
                .iter()
                .any(|m| m.state == MessageState::Deleted)
        {
            // Rewrite the mailbox
            save_messages_to_file(&self.path, &keep_messages, false, true)
                .map_err(|e| format!("{}: {}", self.path, e))?;
        }

        Ok(())
    }

    /// Save messages to a file
    pub fn save_messages(
        &self,
        msg_nums: &[usize],
        path: &str,
        include_headers: bool,
    ) -> Result<(), String> {
        let messages: Vec<&Message> = msg_nums.iter().filter_map(|&n| self.get(n)).collect();

        let msgs: Vec<Message> = messages.iter().map(|m| (*m).clone()).collect();
        save_messages_to_file(path, &msgs, true, include_headers)
            .map_err(|e| format!("{}: {}", path, e))
    }
}

fn save_messages_to_file(
    path: &str,
    messages: &[Message],
    append: bool,
    include_from_line: bool,
) -> io::Result<()> {
    let mut file = if append {
        OpenOptions::new().create(true).append(true).open(path)?
    } else {
        File::create(path)?
    };

    for msg in messages {
        if include_from_line {
            writeln!(file, "{}", msg.from_line)?;
        }
        for header in &msg.header_lines {
            writeln!(file, "{}", header)?;
        }
        writeln!(file)?;
        // Handle From escaping in body
        for line in msg.body.lines() {
            if line.starts_with("From ") {
                write!(file, ">")?;
            }
            writeln!(file, "{}", line)?;
        }
        // Ensure blank line between messages
        if !msg.body.ends_with('\n') {
            writeln!(file)?;
        }
    }

    Ok(())
}

fn get_mbox_path(vars: &Variables) -> String {
    vars.get("MBOX").map(|s| s.to_string()).unwrap_or_else(|| {
        let home = std::env::var("HOME").unwrap_or_else(|_| ".".to_string());
        format!("{}/mbox", home)
    })
}

fn truncate(s: &str, max_len: usize) -> String {
    if s.len() <= max_len {
        s.to_string()
    } else {
        format!("{}...", &s[..max_len.saturating_sub(3)])
    }
}
