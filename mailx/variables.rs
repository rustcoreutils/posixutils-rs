//! Internal variables for mailx

use std::collections::HashMap;

/// Mailx internal variables
#[derive(Debug)]
pub struct Variables {
    /// String/numeric variables
    values: HashMap<String, String>,
    /// Boolean variables (set = true, unset = false)
    booleans: HashMap<String, bool>,
    /// Aliases
    pub aliases: HashMap<String, Vec<String>>,
    /// Alternate names for the user
    pub alternates: Vec<String>,
    /// Ignored header fields
    pub ignored_headers: Vec<String>,
    /// Retained header fields
    pub retained_headers: Vec<String>,
}

impl Variables {
    pub fn new() -> Self {
        let mut vars = Variables {
            values: HashMap::new(),
            booleans: HashMap::new(),
            aliases: HashMap::new(),
            alternates: Vec::new(),
            ignored_headers: Vec::new(),
            retained_headers: Vec::new(),
        };

        // Set defaults per POSIX
        vars.set_bool("asksub", true);
        vars.set_bool("header", true);
        vars.set_bool("save", true);
        vars.set("prompt", "? ");
        vars.set("SHELL", "/bin/sh");
        vars.set("VISUAL", "vi");
        vars.set("indentprefix", "\t");
        vars.set("toplines", "5");

        vars
    }

    /// Set a string/numeric variable
    pub fn set(&mut self, name: &str, value: &str) {
        // Handle ask/asksub synonyms
        let name = if name == "ask" { "asksub" } else { name };

        self.values.insert(name.to_string(), value.to_string());
    }

    /// Get a string variable
    pub fn get(&self, name: &str) -> Option<&str> {
        let name = if name == "ask" { "asksub" } else { name };
        self.values.get(name).map(|s| s.as_str())
    }

    /// Get a numeric variable
    pub fn get_number(&self, name: &str) -> Option<i64> {
        self.get(name).and_then(|s| s.parse().ok())
    }

    /// Set a boolean variable
    pub fn set_bool(&mut self, name: &str, value: bool) {
        // Handle ask/asksub synonyms
        let name = if name == "ask" { "asksub" } else { name };

        self.booleans.insert(name.to_string(), value);
    }

    /// Get a boolean variable
    pub fn get_bool(&self, name: &str) -> bool {
        let name = if name == "ask" { "asksub" } else { name };
        self.booleans.get(name).copied().unwrap_or(false)
    }

    /// Unset a variable
    pub fn unset(&mut self, name: &str) {
        let name = if name == "ask" { "asksub" } else { name };
        self.values.remove(name);
        self.booleans.remove(name);
    }

    /// Check if a variable is set (either as string or boolean)
    pub fn is_set(&self, name: &str) -> bool {
        let name = if name == "ask" { "asksub" } else { name };
        self.values.contains_key(name) || self.booleans.get(name).copied().unwrap_or(false)
    }

    /// Get the escape character
    pub fn escape_char(&self) -> char {
        self.get("escape")
            .and_then(|s| s.chars().next())
            .unwrap_or('~')
    }

    /// Print all set variables
    pub fn print_all(&self) {
        let mut names: Vec<&String> = self.values.keys().collect();
        names.sort();
        for name in names {
            if let Some(value) = self.values.get(name) {
                println!("{}=\"{}\"", name, value);
            }
        }

        let mut bools: Vec<(&String, &bool)> = self.booleans.iter().collect();
        bools.sort_by_key(|(k, _)| *k);
        for (name, value) in bools {
            if *value {
                println!("{}", name);
            }
        }
    }

    /// Expand an alias
    pub fn expand_alias(&self, name: &str) -> Vec<String> {
        if let Some(addrs) = self.aliases.get(name) {
            let mut result = Vec::new();
            for addr in addrs {
                // Recursively expand
                let expanded = self.expand_alias(addr);
                if expanded.is_empty() {
                    result.push(addr.clone());
                } else {
                    result.extend(expanded);
                }
            }
            result
        } else {
            Vec::new()
        }
    }

    /// Check if an address is an alternate for the user
    pub fn is_alternate(&self, addr: &str) -> bool {
        let addr_lower = addr.to_lowercase();
        self.alternates
            .iter()
            .any(|a| a.to_lowercase() == addr_lower)
    }
}

/// Parse a set command argument
pub fn parse_set_arg(arg: &str) -> (&str, Option<&str>) {
    if let Some(eq_pos) = arg.find('=') {
        let name = &arg[..eq_pos];
        let value = &arg[eq_pos + 1..];
        // Remove quotes if present
        let value = value.trim_matches('"').trim_matches('\'');
        (name, Some(value))
    } else {
        (arg, None)
    }
}
