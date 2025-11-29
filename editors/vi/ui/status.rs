//! Status line display and messages.

/// Status line builder.
pub struct StatusLine {
    /// Filename or "[No Name]".
    filename: String,
    /// Whether file is modified.
    modified: bool,
    /// Whether file is read-only.
    readonly: bool,
    /// Current line number.
    line: usize,
    /// Total lines.
    total_lines: usize,
    /// Current column (1-indexed for display).
    column: usize,
    /// Percentage through file.
    percent: Option<u8>,
}

impl StatusLine {
    /// Create a new status line.
    pub fn new() -> Self {
        Self {
            filename: "[No Name]".to_string(),
            modified: false,
            readonly: false,
            line: 1,
            total_lines: 0,
            column: 1,
            percent: None,
        }
    }

    /// Set the filename.
    pub fn set_filename(&mut self, name: Option<&str>) {
        self.filename = name
            .map(|s| s.to_string())
            .unwrap_or_else(|| "[No Name]".to_string());
    }

    /// Set modified flag.
    pub fn set_modified(&mut self, modified: bool) {
        self.modified = modified;
    }

    /// Set readonly flag.
    pub fn set_readonly(&mut self, readonly: bool) {
        self.readonly = readonly;
    }

    /// Set position information.
    pub fn set_position(&mut self, line: usize, column: usize, total_lines: usize) {
        self.line = line;
        self.column = column;
        self.total_lines = total_lines;

        // Calculate percentage
        if total_lines == 0 {
            self.percent = None;
        } else if line == 1 {
            self.percent = Some(0);
        } else if line >= total_lines {
            self.percent = Some(100);
        } else {
            self.percent = Some(((line * 100) / total_lines) as u8);
        }
    }

    /// Build the file info string (for Ctrl-G).
    pub fn file_info(&self) -> String {
        let mut info = format!("\"{}\"", self.filename);

        if self.readonly {
            info.push_str(" [readonly]");
        }

        if self.modified {
            info.push_str(" [Modified]");
        }

        if self.total_lines == 0 {
            info.push_str(" --No lines in buffer--");
        } else {
            info.push_str(&format!(
                " line {} of {} --{}%--",
                self.line,
                self.total_lines,
                self.percent.unwrap_or(0)
            ));
        }

        info
    }

    /// Build a short status string for the status line.
    pub fn short_status(&self, max_width: usize) -> String {
        // Format: "filename [+] line,col percent"
        let mut left = self.filename.clone();

        if self.modified {
            left.push_str(" [+]");
        }

        if self.readonly {
            left.push_str(" [RO]");
        }

        let right = if self.total_lines == 0 {
            String::new()
        } else {
            format!(
                "{},{} {}%",
                self.line,
                self.column,
                self.percent.unwrap_or(0)
            )
        };

        // Combine left and right with padding
        let left_len = left.chars().count();
        let right_len = right.chars().count();

        if left_len + right_len + 1 > max_width {
            // Truncate left side
            let available = max_width.saturating_sub(right_len + 4);
            let truncated: String = left.chars().take(available).collect();
            if available < left_len {
                format!("{}... {}", truncated, right)
            } else {
                format!("{} {}", left, right)
            }
        } else {
            let padding = max_width - left_len - right_len;
            format!("{}{:padding$}{}", left, "", right)
        }
    }
}

impl Default for StatusLine {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_file_info() {
        let mut status = StatusLine::new();
        status.set_filename(Some("test.txt"));
        status.set_position(10, 5, 100);

        let info = status.file_info();
        assert!(info.contains("test.txt"));
        assert!(info.contains("line 10 of 100"));
        assert!(info.contains("10%"));
    }

    #[test]
    fn test_file_info_modified() {
        let mut status = StatusLine::new();
        status.set_filename(Some("test.txt"));
        status.set_modified(true);
        status.set_position(1, 1, 10);

        let info = status.file_info();
        assert!(info.contains("[Modified]"));
    }

    #[test]
    fn test_file_info_empty() {
        let status = StatusLine::new();
        let info = status.file_info();
        assert!(info.contains("No lines in buffer"));
    }

    #[test]
    fn test_short_status() {
        let mut status = StatusLine::new();
        status.set_filename(Some("test.txt"));
        status.set_position(50, 10, 100);

        let short = status.short_status(40);
        assert!(short.contains("test.txt"));
        assert!(short.contains("50,10"));
        assert!(short.contains("50%"));
    }
}
