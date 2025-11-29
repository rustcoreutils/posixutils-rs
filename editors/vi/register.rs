//! Register management for vi.
//!
//! Registers store text for yank/delete/put operations.
//!
//! POSIX vi defines these registers:
//! - Unnamed register ("): Default for most operations
//! - Named registers (a-z): User-accessible registers
//! - Numbered registers (1-9): Store previous deletes
//!
//! Uppercase named registers (A-Z) append to the corresponding lowercase register.

use std::collections::HashMap;

/// Content stored in a register.
#[derive(Debug, Clone)]
pub struct RegisterContent {
    /// The text content.
    pub text: String,
    /// Whether this is line-wise content.
    pub linewise: bool,
}

impl RegisterContent {
    /// Create new register content.
    pub fn new(text: String, linewise: bool) -> Self {
        Self { text, linewise }
    }

    /// Create character-wise content.
    pub fn chars(text: String) -> Self {
        Self::new(text, false)
    }

    /// Create line-wise content.
    pub fn lines(text: String) -> Self {
        Self::new(text, true)
    }
}

/// Register storage.
#[derive(Debug)]
pub struct Registers {
    /// Named registers (a-z and ").
    named: HashMap<char, RegisterContent>,
    /// Numbered registers (1-9) for previous deletes.
    numbered: [Option<RegisterContent>; 9],
    /// Small delete register (-).
    small_delete: Option<RegisterContent>,
    /// Last search pattern (/).
    search_pattern: Option<String>,
    /// Last inserted text (.).
    last_insert: Option<String>,
    /// Last executed command (:).
    last_command: Option<String>,
    /// Current file name (%).
    file_name: Option<String>,
    /// Alternate file name (#).
    alt_file_name: Option<String>,
}

impl Registers {
    /// Create a new register storage.
    pub fn new() -> Self {
        Self {
            named: HashMap::new(),
            numbered: Default::default(),
            small_delete: None,
            search_pattern: None,
            last_insert: None,
            last_command: None,
            file_name: None,
            alt_file_name: None,
        }
    }

    /// Get content from a register.
    pub fn get(&self, name: char) -> Option<&RegisterContent> {
        match name {
            '"' => self.named.get(&'"'),
            'a'..='z' => self.named.get(&name),
            'A'..='Z' => self.named.get(&name.to_ascii_lowercase()),
            '1'..='9' => {
                let idx = (name as u8 - b'1') as usize;
                self.numbered[idx].as_ref()
            }
            '-' => self.small_delete.as_ref(),
            _ => None,
        }
    }

    /// Get the default (unnamed) register.
    pub fn get_unnamed(&self) -> Option<&RegisterContent> {
        self.named.get(&'"')
    }

    /// Set content in a register.
    pub fn set(&mut self, name: char, content: RegisterContent) {
        match name {
            '"' => {
                self.named.insert('"', content);
            }
            'a'..='z' => {
                self.named.insert(name, content);
            }
            'A'..='Z' => {
                // Uppercase appends to lowercase
                let lower = name.to_ascii_lowercase();
                if let Some(existing) = self.named.get_mut(&lower) {
                    existing.text.push_str(&content.text);
                    // Linewise takes precedence if either is linewise
                    existing.linewise = existing.linewise || content.linewise;
                } else {
                    self.named.insert(lower, content);
                }
            }
            _ => {} // Ignore invalid register names
        }
    }

    /// Set the unnamed register.
    pub fn set_unnamed(&mut self, content: RegisterContent) {
        self.named.insert('"', content);
    }

    /// Store a delete in numbered registers.
    /// This shifts registers 1-8 to 2-9 and puts new content in 1.
    pub fn push_delete(&mut self, content: RegisterContent) {
        // Also set the unnamed register
        self.set_unnamed(content.clone());

        // Shift numbered registers
        for i in (1..9).rev() {
            self.numbered[i] = self.numbered[i - 1].take();
        }
        self.numbered[0] = Some(content);
    }

    /// Store a small delete (less than one line, not linewise).
    pub fn set_small_delete(&mut self, content: RegisterContent) {
        self.small_delete = Some(content.clone());
        self.set_unnamed(content);
    }

    /// Store a yank operation.
    /// Sets both the specified register and the unnamed register.
    pub fn yank(&mut self, register: Option<char>, content: RegisterContent) {
        self.set_unnamed(content.clone());
        if let Some(name) = register {
            self.set(name, content);
        }
    }

    /// Store a delete operation.
    /// For linewise deletes or deletes >= 1 line, use numbered registers.
    /// For small character-wise deletes, use the small delete register.
    pub fn delete(&mut self, register: Option<char>, content: RegisterContent, is_small: bool) {
        if let Some(name) = register {
            self.set(name, content.clone());
            self.set_unnamed(content);
        } else if is_small && !content.linewise {
            self.set_small_delete(content);
        } else {
            self.push_delete(content);
        }
    }

    /// Set the search pattern register.
    pub fn set_search(&mut self, pattern: &str) {
        self.search_pattern = Some(pattern.to_string());
    }

    /// Get the search pattern.
    pub fn get_search(&self) -> Option<&str> {
        self.search_pattern.as_deref()
    }

    /// Set the last inserted text.
    pub fn set_last_insert(&mut self, text: &str) {
        self.last_insert = Some(text.to_string());
    }

    /// Get the last inserted text.
    pub fn get_last_insert(&self) -> Option<&str> {
        self.last_insert.as_deref()
    }

    /// Set the last ex command.
    pub fn set_last_command(&mut self, cmd: &str) {
        self.last_command = Some(cmd.to_string());
    }

    /// Get the last ex command.
    pub fn get_last_command(&self) -> Option<&str> {
        self.last_command.as_deref()
    }

    /// Set the current file name.
    pub fn set_file_name(&mut self, name: &str) {
        self.file_name = Some(name.to_string());
    }

    /// Get the current file name.
    pub fn get_file_name(&self) -> Option<&str> {
        self.file_name.as_deref()
    }

    /// Set the alternate file name.
    pub fn set_alt_file_name(&mut self, name: &str) {
        self.alt_file_name = Some(name.to_string());
    }

    /// Get the alternate file name.
    pub fn get_alt_file_name(&self) -> Option<&str> {
        self.alt_file_name.as_deref()
    }

    /// Clear all registers.
    pub fn clear(&mut self) {
        self.named.clear();
        self.numbered = Default::default();
        self.small_delete = None;
    }
}

impl Default for Registers {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_unnamed_register() {
        let mut regs = Registers::new();
        regs.set_unnamed(RegisterContent::chars("hello".to_string()));

        let content = regs.get_unnamed().unwrap();
        assert_eq!(content.text, "hello");
        assert!(!content.linewise);
    }

    #[test]
    fn test_named_register() {
        let mut regs = Registers::new();
        regs.set('a', RegisterContent::chars("test".to_string()));

        let content = regs.get('a').unwrap();
        assert_eq!(content.text, "test");
    }

    #[test]
    fn test_uppercase_appends() {
        let mut regs = Registers::new();
        regs.set('a', RegisterContent::chars("hello".to_string()));
        regs.set('A', RegisterContent::chars(" world".to_string()));

        let content = regs.get('a').unwrap();
        assert_eq!(content.text, "hello world");
    }

    #[test]
    fn test_numbered_registers_shift() {
        let mut regs = Registers::new();

        regs.push_delete(RegisterContent::lines("line1\n".to_string()));
        regs.push_delete(RegisterContent::lines("line2\n".to_string()));
        regs.push_delete(RegisterContent::lines("line3\n".to_string()));

        // Most recent is in register 1
        assert_eq!(regs.get('1').unwrap().text, "line3\n");
        assert_eq!(regs.get('2').unwrap().text, "line2\n");
        assert_eq!(regs.get('3').unwrap().text, "line1\n");
    }

    #[test]
    fn test_small_delete() {
        let mut regs = Registers::new();
        regs.set_small_delete(RegisterContent::chars("x".to_string()));

        let content = regs.get('-').unwrap();
        assert_eq!(content.text, "x");
    }

    #[test]
    fn test_yank_sets_unnamed() {
        let mut regs = Registers::new();
        regs.yank(Some('a'), RegisterContent::chars("yanked".to_string()));

        // Both named and unnamed should be set
        assert_eq!(regs.get('a').unwrap().text, "yanked");
        assert_eq!(regs.get_unnamed().unwrap().text, "yanked");
    }

    #[test]
    fn test_search_register() {
        let mut regs = Registers::new();
        regs.set_search("pattern");

        assert_eq!(regs.get_search(), Some("pattern"));
    }
}
