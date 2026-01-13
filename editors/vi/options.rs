//! Editor options management.
//!
//! This module handles vi editor options set via :set command.
//! POSIX vi specifies these options and their behaviors.

use crate::error::{Result, ViError};

/// Editor options.
#[derive(Debug, Clone)]
pub struct Options {
    // Display options
    /// Show line numbers (number/nu).
    pub number: bool,
    /// Number of spaces for a tab (tabstop/ts).
    pub tabstop: usize,
    /// Spaces for autoindent shift (shiftwidth/sw).
    pub shiftwidth: usize,
    /// Display $ at end of changed text (list).
    pub list: bool,
    /// Show matching bracket (showmatch/sm).
    pub showmatch: bool,
    /// Show mode in status line (showmode).
    pub showmode: bool,
    /// Number of lines for window (window).
    pub window: usize,
    /// Scroll amount for Ctrl-D/U (scroll).
    pub scroll: usize,

    // Edit options
    /// Autoindent new lines (autoindent/ai).
    pub autoindent: bool,
    /// Auto write before commands (autowrite/aw).
    pub autowrite: bool,
    /// Expand tabs to spaces (expandtab/et).
    pub expandtab: bool,
    /// Case insensitive search (ignorecase/ic).
    pub ignorecase: bool,
    /// Magic characters in patterns (magic).
    pub magic: bool,
    /// Read-only mode (readonly/ro).
    pub readonly: bool,
    /// Wrap long lines (wrap).
    pub wrap: bool,
    /// Wrap search at end of file (wrapscan/ws).
    pub wrapscan: bool,

    // Error handling
    /// Ring bell on error (errorbells/eb).
    pub errorbells: bool,
    /// Flash screen instead of bell (flash).
    pub flash: bool,

    // File options
    /// Backup file extension (backup).
    pub backup: bool,
    /// Write any buffer (writeany/wa).
    pub writeany: bool,

    // Paragraph/section definitions
    /// Paragraph delimiters (paragraphs).
    pub paragraphs: String,
    /// Section delimiters (sections).
    pub sections: String,

    // Tags
    /// Tags file name (tags).
    pub tags: String,
    /// Tag stack limit (taglength).
    pub taglength: usize,

    // Timeout
    /// Timeout for mapped sequences (timeout).
    pub timeout: bool,
    /// Terminal type (term).
    pub term: String,

    // Shell
    /// Shell program (shell/sh).
    pub shell: String,
}

impl Default for Options {
    fn default() -> Self {
        Self {
            // Display
            number: false,
            tabstop: 8,
            shiftwidth: 8,
            list: false,
            showmatch: false,
            showmode: true,
            window: 24,
            scroll: 12,

            // Edit
            autoindent: false,
            autowrite: false,
            expandtab: false,
            ignorecase: false,
            magic: true,
            readonly: false,
            wrap: true,
            wrapscan: true,

            // Error
            errorbells: false,
            flash: false,

            // File
            backup: false,
            writeany: false,

            // Paragraph/section
            paragraphs: "IPLPPPQPP LIpplpipbp".to_string(),
            sections: "NHSHH HUnhsh".to_string(),

            // Tags
            tags: "tags".to_string(),
            taglength: 0,

            // Timeout
            timeout: true,
            term: std::env::var("TERM").unwrap_or_else(|_| "xterm".to_string()),

            // Shell
            shell: std::env::var("SHELL").unwrap_or_else(|_| "/bin/sh".to_string()),
        }
    }
}

impl Options {
    /// Create default options.
    pub fn new() -> Self {
        Self::default()
    }

    /// Set window size based on terminal size.
    pub fn set_window_size(&mut self, rows: usize) {
        self.window = rows.saturating_sub(1); // Leave room for status line
        self.scroll = self.window / 2;
    }

    /// Parse and apply a :set command argument.
    pub fn set(&mut self, arg: &str) -> Result<Option<String>> {
        let arg = arg.trim();

        // Handle "all" - show all options
        if arg == "all" {
            return Ok(Some(self.show_all()));
        }

        // Handle empty - show changed options
        if arg.is_empty() {
            return Ok(Some(self.show_changed()));
        }

        // Handle "no" prefix for boolean options
        if let Some(name) = arg.strip_prefix("no") {
            return self.set_bool(name, false);
        }

        // Handle "=value" for numeric/string options
        if let Some((name, value)) = arg.split_once('=') {
            return self.set_value(name.trim(), value.trim());
        }

        // Handle "?" suffix to query option
        if let Some(name) = arg.strip_suffix('?') {
            return self.query(name.trim());
        }

        // Otherwise, try to set boolean option to true
        self.set_bool(arg, true)
    }

    /// Set a boolean option.
    fn set_bool(&mut self, name: &str, value: bool) -> Result<Option<String>> {
        match name {
            "number" | "nu" => self.number = value,
            "list" => self.list = value,
            "showmatch" | "sm" => self.showmatch = value,
            "showmode" => self.showmode = value,
            "autoindent" | "ai" => self.autoindent = value,
            "autowrite" | "aw" => self.autowrite = value,
            "expandtab" | "et" => self.expandtab = value,
            "ignorecase" | "ic" => self.ignorecase = value,
            "magic" => self.magic = value,
            "readonly" | "ro" => self.readonly = value,
            "wrap" => self.wrap = value,
            "wrapscan" | "ws" => self.wrapscan = value,
            "errorbells" | "eb" => self.errorbells = value,
            "flash" => self.flash = value,
            "backup" => self.backup = value,
            "writeany" | "wa" => self.writeany = value,
            "timeout" => self.timeout = value,
            _ => return Err(ViError::InvalidOption(name.to_string())),
        }
        Ok(None)
    }

    /// Set a value option.
    fn set_value(&mut self, name: &str, value: &str) -> Result<Option<String>> {
        match name {
            "tabstop" | "ts" => {
                self.tabstop = value
                    .parse()
                    .map_err(|_| ViError::InvalidOption(format!("{}={}", name, value)))?;
            }
            "shiftwidth" | "sw" => {
                self.shiftwidth = value
                    .parse()
                    .map_err(|_| ViError::InvalidOption(format!("{}={}", name, value)))?;
            }
            "window" => {
                self.window = value
                    .parse()
                    .map_err(|_| ViError::InvalidOption(format!("{}={}", name, value)))?;
            }
            "scroll" => {
                self.scroll = value
                    .parse()
                    .map_err(|_| ViError::InvalidOption(format!("{}={}", name, value)))?;
            }
            "paragraphs" => self.paragraphs = value.to_string(),
            "sections" => self.sections = value.to_string(),
            "tags" => self.tags = value.to_string(),
            "taglength" | "tl" => {
                self.taglength = value
                    .parse()
                    .map_err(|_| ViError::InvalidOption(format!("{}={}", name, value)))?;
            }
            "term" => self.term = value.to_string(),
            "shell" | "sh" => self.shell = value.to_string(),
            _ => return Err(ViError::InvalidOption(name.to_string())),
        }
        Ok(None)
    }

    /// Query an option value.
    fn query(&self, name: &str) -> Result<Option<String>> {
        let result = match name {
            "number" | "nu" => format!("{}number", if self.number { "" } else { "no" }),
            "tabstop" | "ts" => format!("tabstop={}", self.tabstop),
            "shiftwidth" | "sw" => format!("shiftwidth={}", self.shiftwidth),
            "list" => format!("{}list", if self.list { "" } else { "no" }),
            "showmatch" | "sm" => format!("{}showmatch", if self.showmatch { "" } else { "no" }),
            "showmode" => format!("{}showmode", if self.showmode { "" } else { "no" }),
            "window" => format!("window={}", self.window),
            "scroll" => format!("scroll={}", self.scroll),
            "autoindent" | "ai" => format!("{}autoindent", if self.autoindent { "" } else { "no" }),
            "autowrite" | "aw" => format!("{}autowrite", if self.autowrite { "" } else { "no" }),
            "expandtab" | "et" => format!("{}expandtab", if self.expandtab { "" } else { "no" }),
            "ignorecase" | "ic" => format!("{}ignorecase", if self.ignorecase { "" } else { "no" }),
            "magic" => format!("{}magic", if self.magic { "" } else { "no" }),
            "readonly" | "ro" => format!("{}readonly", if self.readonly { "" } else { "no" }),
            "wrap" => format!("{}wrap", if self.wrap { "" } else { "no" }),
            "wrapscan" | "ws" => format!("{}wrapscan", if self.wrapscan { "" } else { "no" }),
            "errorbells" | "eb" => format!("{}errorbells", if self.errorbells { "" } else { "no" }),
            "flash" => format!("{}flash", if self.flash { "" } else { "no" }),
            "backup" => format!("{}backup", if self.backup { "" } else { "no" }),
            "writeany" | "wa" => format!("{}writeany", if self.writeany { "" } else { "no" }),
            "paragraphs" => format!("paragraphs={}", self.paragraphs),
            "sections" => format!("sections={}", self.sections),
            "tags" => format!("tags={}", self.tags),
            "taglength" | "tl" => format!("taglength={}", self.taglength),
            "timeout" => format!("{}timeout", if self.timeout { "" } else { "no" }),
            "term" => format!("term={}", self.term),
            "shell" | "sh" => format!("shell={}", self.shell),
            _ => return Err(ViError::InvalidOption(name.to_string())),
        };
        Ok(Some(result))
    }

    /// Show all options in formatted columns.
    fn show_all(&self) -> String {
        // Collect all options
        let mut items: Vec<String> = Vec::new();

        // Boolean options (alphabetically sorted)
        items.push(format!(
            "{}autoindent",
            if self.autoindent { "" } else { "no" }
        ));
        items.push(format!(
            "{}autowrite",
            if self.autowrite { "" } else { "no" }
        ));
        items.push(format!("{}backup", if self.backup { "" } else { "no" }));
        items.push(format!(
            "{}errorbells",
            if self.errorbells { "" } else { "no" }
        ));
        items.push(format!(
            "{}expandtab",
            if self.expandtab { "" } else { "no" }
        ));
        items.push(format!("{}flash", if self.flash { "" } else { "no" }));
        items.push(format!(
            "{}ignorecase",
            if self.ignorecase { "" } else { "no" }
        ));
        items.push(format!("{}list", if self.list { "" } else { "no" }));
        items.push(format!("{}magic", if self.magic { "" } else { "no" }));
        items.push(format!("{}number", if self.number { "" } else { "no" }));
        items.push(format!("{}readonly", if self.readonly { "" } else { "no" }));
        items.push(format!(
            "{}showmatch",
            if self.showmatch { "" } else { "no" }
        ));
        items.push(format!("{}showmode", if self.showmode { "" } else { "no" }));
        items.push(format!("{}timeout", if self.timeout { "" } else { "no" }));
        items.push(format!("{}wrap", if self.wrap { "" } else { "no" }));
        items.push(format!("{}wrapscan", if self.wrapscan { "" } else { "no" }));
        items.push(format!("{}writeany", if self.writeany { "" } else { "no" }));

        // Numeric options
        items.push(format!("scroll={}", self.scroll));
        items.push(format!("shiftwidth={}", self.shiftwidth));
        items.push(format!("tabstop={}", self.tabstop));
        items.push(format!("taglength={}", self.taglength));
        items.push(format!("window={}", self.window));

        // String options
        items.push(format!("paragraphs={}", self.paragraphs));
        items.push(format!("sections={}", self.sections));
        items.push(format!("shell={}", self.shell));
        items.push(format!("tags={}", self.tags));
        items.push(format!("term={}", self.term));

        // Format in 3 columns with fixed width
        Self::format_columns(&items, 3, 20)
    }

    /// Format items in columns.
    fn format_columns(items: &[String], cols: usize, col_width: usize) -> String {
        let mut result = String::new();
        for chunk in items.chunks(cols) {
            for (i, item) in chunk.iter().enumerate() {
                if i > 0 {
                    result.push_str("  "); // separator between columns
                }
                result.push_str(&format!("{:<width$}", item, width = col_width));
            }
            result.push('\n');
        }
        // Remove trailing newline
        if result.ends_with('\n') {
            result.pop();
        }
        result
    }

    /// Show changed options (different from defaults).
    fn show_changed(&self) -> String {
        let defaults = Self::default();
        let mut changes = Vec::new();

        if self.number != defaults.number {
            changes.push(format!("{}number", if self.number { "" } else { "no" }));
        }
        if self.tabstop != defaults.tabstop {
            changes.push(format!("tabstop={}", self.tabstop));
        }
        if self.shiftwidth != defaults.shiftwidth {
            changes.push(format!("shiftwidth={}", self.shiftwidth));
        }
        if self.autoindent != defaults.autoindent {
            changes.push(format!(
                "{}autoindent",
                if self.autoindent { "" } else { "no" }
            ));
        }
        if self.ignorecase != defaults.ignorecase {
            changes.push(format!(
                "{}ignorecase",
                if self.ignorecase { "" } else { "no" }
            ));
        }

        if changes.is_empty() {
            "No changes from defaults".to_string()
        } else {
            changes.join("  ")
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_options() {
        let opts = Options::default();
        assert!(!opts.number);
        assert_eq!(opts.tabstop, 8);
        assert!(opts.magic);
        assert!(opts.wrapscan);
    }

    #[test]
    fn test_set_bool() {
        let mut opts = Options::new();
        opts.set("number").unwrap();
        assert!(opts.number);

        opts.set("nonumber").unwrap();
        assert!(!opts.number);
    }

    #[test]
    fn test_set_value() {
        let mut opts = Options::new();
        opts.set("tabstop=4").unwrap();
        assert_eq!(opts.tabstop, 4);

        opts.set("sw=2").unwrap();
        assert_eq!(opts.shiftwidth, 2);
    }

    #[test]
    fn test_query() {
        let mut opts = Options::new();
        let result = opts.set("tabstop?").unwrap();
        assert_eq!(result, Some("tabstop=8".to_string()));
    }

    #[test]
    fn test_abbreviations() {
        let mut opts = Options::new();
        opts.set("ai").unwrap();
        assert!(opts.autoindent);

        opts.set("noai").unwrap();
        assert!(!opts.autoindent);
    }

    #[test]
    fn test_invalid_option() {
        let mut opts = Options::new();
        let result = opts.set("invalid");
        assert!(result.is_err());
    }
}
