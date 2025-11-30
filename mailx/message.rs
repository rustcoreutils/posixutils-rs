//! Message representation for mailx

use std::collections::HashMap;

/// Message state as defined by POSIX
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MessageState {
    New,
    Unread,
    Read,
    Deleted,
    Preserved,
    Saved,
}

impl MessageState {
    pub fn status_char(&self) -> char {
        match self {
            MessageState::New => 'N',
            MessageState::Unread => 'U',
            MessageState::Read => 'R',
            MessageState::Deleted => 'D',
            MessageState::Preserved => 'P',
            MessageState::Saved => '*',
        }
    }
}

/// A single email message
#[derive(Debug, Clone)]
pub struct Message {
    /// The full "From " line (envelope)
    pub from_line: String,
    /// Parsed headers (lowercase key -> original value)
    pub headers: HashMap<String, String>,
    /// Original header lines in order
    pub header_lines: Vec<String>,
    /// Message body
    pub body: String,
    /// Current state
    pub state: MessageState,
    /// Whether this message has been displayed
    pub displayed: bool,
}

impl Message {
    pub fn new() -> Self {
        Message {
            from_line: String::new(),
            headers: HashMap::new(),
            header_lines: Vec::new(),
            body: String::new(),
            state: MessageState::New,
            displayed: false,
        }
    }

    /// Get a header value (case-insensitive)
    pub fn get_header(&self, name: &str) -> Option<&str> {
        self.headers.get(&name.to_lowercase()).map(|s| s.as_str())
    }

    /// Get the From: header address
    pub fn from(&self) -> &str {
        self.get_header("from").unwrap_or("(unknown)")
    }

    /// Get the Subject: header
    pub fn subject(&self) -> &str {
        self.get_header("subject").unwrap_or("(no subject)")
    }

    /// Get the To: header
    pub fn to(&self) -> &str {
        self.get_header("to").unwrap_or("")
    }

    /// Get the Date: header
    pub fn date(&self) -> &str {
        self.get_header("date").unwrap_or("")
    }

    /// Get the total size in bytes
    pub fn size(&self) -> usize {
        let header_size: usize = self.header_lines.iter().map(|l| l.len() + 1).sum();
        self.from_line.len() + 1 + header_size + 1 + self.body.len()
    }

    /// Get the number of lines
    pub fn line_count(&self) -> usize {
        1 + self.header_lines.len() + 1 + self.body.lines().count()
    }

    /// Format the message for display with header filtering
    pub fn format_display(
        &self,
        show_all_headers: bool,
        ignored: &[String],
        retained: &[String],
    ) -> String {
        let mut output = String::new();

        // Add headers
        for line in &self.header_lines {
            let field_name = line.split(':').next().unwrap_or("").trim().to_lowercase();

            let include = if show_all_headers {
                true
            } else if !retained.is_empty() {
                retained.iter().any(|r| r.to_lowercase() == field_name)
            } else {
                !ignored.iter().any(|i| i.to_lowercase() == field_name)
            };

            if include {
                output.push_str(line);
                output.push('\n');
            }
        }

        output.push('\n');
        output.push_str(&self.body);

        output
    }

    /// Format the full message including From line
    pub fn format_full(&self) -> String {
        let mut output = self.from_line.clone();
        output.push('\n');

        for line in &self.header_lines {
            output.push_str(line);
            output.push('\n');
        }

        output.push('\n');
        output.push_str(&self.body);

        // Ensure trailing newline
        if !output.ends_with('\n') {
            output.push('\n');
        }

        output
    }

    /// Parse a date string and return a short date format for headers
    pub fn short_date(&self) -> String {
        let date = self.date();
        // Try to extract month day from common date formats
        // e.g., "Mon, 25 Nov 2024 10:30:00 +0000"
        let parts: Vec<&str> = date.split_whitespace().collect();
        if parts.len() >= 3 {
            // Try to find month and day
            for i in 0..parts.len().saturating_sub(1) {
                let month = match parts[i].trim_end_matches(',') {
                    "Jan" | "January" => Some("Jan"),
                    "Feb" | "February" => Some("Feb"),
                    "Mar" | "March" => Some("Mar"),
                    "Apr" | "April" => Some("Apr"),
                    "May" => Some("May"),
                    "Jun" | "June" => Some("Jun"),
                    "Jul" | "July" => Some("Jul"),
                    "Aug" | "August" => Some("Aug"),
                    "Sep" | "September" => Some("Sep"),
                    "Oct" | "October" => Some("Oct"),
                    "Nov" | "November" => Some("Nov"),
                    "Dec" | "December" => Some("Dec"),
                    _ => None,
                };
                if let Some(m) = month {
                    if let Ok(day) = parts[i + 1].trim_end_matches(',').parse::<u32>() {
                        return format!("{} {:2}", m, day);
                    }
                    if i > 0 {
                        if let Ok(day) = parts[i - 1].trim_end_matches(',').parse::<u32>() {
                            return format!("{} {:2}", m, day);
                        }
                    }
                }
            }
        }
        // Fallback - just return first 6 chars
        date.chars().take(6).collect()
    }

    /// Extract the sender's name or address for header display
    pub fn short_from(&self) -> String {
        let from = self.from();
        // Try to extract name from "Name <email>" format
        if let Some(start) = from.find('<') {
            let name = from[..start].trim().trim_matches('"');
            if !name.is_empty() {
                return truncate_str(name, 18);
            }
        }
        // Try to extract name from "(Name)" format
        if let Some(start) = from.find('(') {
            if let Some(end) = from.find(')') {
                let name = &from[start + 1..end];
                if !name.is_empty() {
                    return truncate_str(name, 18);
                }
            }
        }
        // Just use the address
        let addr = from.trim_start_matches('<').trim_end_matches('>');
        truncate_str(addr, 18)
    }
}

fn truncate_str(s: &str, max_len: usize) -> String {
    if s.len() <= max_len {
        s.to_string()
    } else {
        format!("{}...", &s[..max_len - 3])
    }
}

/// Extract email address from a string like "Name <email>" or just "email"
pub fn extract_address(s: &str) -> &str {
    if let Some(start) = s.find('<') {
        if let Some(end) = s.find('>') {
            return &s[start + 1..end];
        }
    }
    s.trim()
}

/// Extract login name from an email address
pub fn extract_login(addr: &str) -> &str {
    let addr = extract_address(addr);
    if let Some(at) = addr.find('@') {
        &addr[..at]
    } else {
        addr
    }
}
