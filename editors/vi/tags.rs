//! ctags `tags`-file lookup for vi/ex (`-t`, `:tag`, `^]`).
//!
//! A tags file has one tab-separated entry per line:
//! ```text
//! <tagname>\t<filename>\t<address>
//! ```
//! where `<address>` is either a line number or an ex search pattern of the
//! form `/pattern/` or `?pattern?` (optionally followed by a `;"` extended-
//! ctags comment, which is ignored). The tag name is compared literally (not
//! as a regular expression); when the `taglength` option is non-zero only the
//! leading `taglength` characters are significant.

use std::fs::File;
use std::io::{BufRead, BufReader};

/// Where a tag entry says the definition lives.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TagAddress {
    /// Absolute line number.
    Line(usize),
    /// A search pattern (BRE) identifying the line.
    Pattern(String),
}

/// A resolved tag: the file and the location within it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TagMatch {
    /// File containing the definition.
    pub file: String,
    /// Address of the definition within the file.
    pub address: TagAddress,
}

/// Whether `name` matches `tag` honoring `taglength` (0 = full comparison).
fn name_matches(tag: &str, name: &str, taglength: usize) -> bool {
    if taglength == 0 {
        tag == name
    } else {
        let n = taglength;
        let a: String = tag.chars().take(n).collect();
        let b: String = name.chars().take(n).collect();
        a == b
    }
}

/// Parse the address field of a tags entry.
fn parse_address(field: &str) -> Option<TagAddress> {
    let field = field.trim();
    if !field.is_empty() && field.chars().all(|c| c.is_ascii_digit()) {
        return field.parse::<usize>().ok().map(TagAddress::Line);
    }
    // Strip a trailing extended-ctags `;"` comment if present.
    let core = match field.find(";\"") {
        Some(idx) => &field[..idx],
        None => field,
    };
    let core = core.trim();
    let bytes = core.as_bytes();
    if bytes.len() >= 2 {
        let delim = bytes[0];
        if (delim == b'/' || delim == b'?') && bytes[bytes.len() - 1] == delim {
            let inner = &core[1..core.len() - 1];
            // ctags escapes the delimiter as \/ (or \?); unescape it.
            let esc = format!("\\{}", delim as char);
            let pattern = inner.replace(&esc, &(delim as char).to_string());
            return Some(TagAddress::Pattern(pattern));
        }
    }
    None
}

/// Look up `name` in the given tags files (in order), returning the first
/// matching entry. `tags_files` is the split `tags` option.
pub fn lookup(tags_files: &[&str], name: &str, taglength: usize) -> Option<TagMatch> {
    for tf in tags_files {
        let Ok(file) = File::open(tf) else { continue };
        let reader = BufReader::new(file);
        for line in reader.lines().map_while(Result::ok) {
            if line.is_empty() || line.starts_with("!_TAG_") {
                continue; // skip ctags pseudo-tags / blank lines
            }
            let mut parts = line.splitn(3, '\t');
            let (Some(tag), Some(fname), Some(addr)) = (parts.next(), parts.next(), parts.next())
            else {
                continue;
            };
            if name_matches(tag, name, taglength) {
                if let Some(address) = parse_address(addr) {
                    return Some(TagMatch {
                        file: fname.to_string(),
                        address,
                    });
                }
            }
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;

    #[test]
    fn test_parse_address_line() {
        assert_eq!(parse_address("42"), Some(TagAddress::Line(42)));
    }

    #[test]
    fn test_parse_address_pattern() {
        assert_eq!(
            parse_address("/^int main()$/"),
            Some(TagAddress::Pattern("^int main()$".to_string()))
        );
    }

    #[test]
    fn test_parse_address_pattern_with_extension() {
        assert_eq!(
            parse_address("/^foo$/;\"\tf"),
            Some(TagAddress::Pattern("^foo$".to_string()))
        );
    }

    #[test]
    fn test_lookup_finds_entry() {
        let dir = tempfile::tempdir().unwrap();
        let tags = dir.path().join("tags");
        let mut f = File::create(&tags).unwrap();
        writeln!(f, "main\tsrc/main.c\t/^int main()$/").unwrap();
        writeln!(f, "helper\tsrc/util.c\t17").unwrap();
        drop(f);

        let path = tags.to_str().unwrap();
        let m = lookup(&[path], "helper", 0).unwrap();
        assert_eq!(m.file, "src/util.c");
        assert_eq!(m.address, TagAddress::Line(17));

        let m = lookup(&[path], "main", 0).unwrap();
        assert_eq!(m.address, TagAddress::Pattern("^int main()$".to_string()));

        assert!(lookup(&[path], "missing", 0).is_none());
    }

    #[test]
    fn test_taglength() {
        let dir = tempfile::tempdir().unwrap();
        let tags = dir.path().join("tags");
        let mut f = File::create(&tags).unwrap();
        writeln!(f, "function_one\ta.c\t1").unwrap();
        drop(f);
        let path = tags.to_str().unwrap();
        // With taglength 4, "funcXXX" matches "function_one" on the first 4.
        assert!(lookup(&[path], "funczzzz", 4).is_some());
        assert!(lookup(&[path], "fxxx", 4).is_none());
    }
}
