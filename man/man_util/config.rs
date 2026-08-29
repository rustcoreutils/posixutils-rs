//
// Copyright (c) 2025-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::{
    collections::HashMap,
    fs::File,
    io::{BufRead, BufReader},
    path::PathBuf,
};

use crate::ManError;

/// # ManConfig
///
/// Parsed configuration file
///
/// ## Fields:
/// * `manpaths`
/// * `output_options`
#[derive(Debug, Default)]
pub struct ManConfig {
    pub manpaths: Vec<PathBuf>,
    pub output_options: HashMap<String, Option<String>>,
}

/// # parse_config_file
///
/// Parses man configuration file.
///
/// # Params:
/// * path - path to configuration file
///
/// # Errors:
/// * io
pub fn parse_config_file(path: PathBuf) -> Result<ManConfig, ManError> {
    let file = File::open(path)?;
    parse_config(BufReader::new(file))
}

/// Parse configuration from any reader (split out so it can be tested without
/// touching the filesystem).
pub fn parse_config<R: BufRead>(reader: R) -> Result<ManConfig, ManError> {
    let mut conf = ManConfig::default();

    for line_result in reader.lines() {
        let line = line_result?;
        let line = line.trim();

        if line.is_empty() || line.starts_with("#") {
            continue;
        }

        let mut parts = line.split_whitespace();
        let directive = match parts.next() {
            Some(d) => d,
            None => continue,
        };

        match directive {
            // `manpath` is mandoc's spelling; `MANDATORY_MANPATH` is man-db's,
            // used by /etc/man_db.conf and /etc/manpath.config. Without the
            // latter those files parsed to zero manual roots on every Debian
            // and Fedora system, which is the whole of their content that this
            // implementation can act on.
            "manpath" | "MANDATORY_MANPATH" => {
                if let Some(path) = parts.next() {
                    conf.manpaths.push(PathBuf::from(path));
                }
            }
            "output" => {
                if let Some(option_name) = parts.next() {
                    let value = parts.next().map(|s| s.to_string());
                    conf.output_options.insert(option_name.to_string(), value);
                }
            }
            // Deliberately unhandled man-db directives, not merely unknown:
            //
            // MANPATH_MAP derives roots by mapping $PATH entries to manual
            // directories. On a stock system every target it names is already
            // reachable through MANDATORY_MANPATH or the built-in roots, so it
            // would cost a $PATH walk per invocation for nothing.
            //
            // MANDB_MAP names the cat-page cache directory; this
            // implementation neither reads nor writes preformatted pages.
            //
            // SECTION overrides the section search order site-wide. POSIX does
            // not define sections at all, and honouring it would make the
            // result of `man printf` depend on the host.
            _ => continue,
        }
    }

    Ok(conf)
}

#[cfg(test)]
mod tests {
    use super::parse_config;
    use std::io::Cursor;
    use std::path::PathBuf;

    #[test]
    fn man_db_mandatory_manpath_is_recognized() {
        // /etc/man_db.conf and /etc/manpath.config contain none of the
        // directives this parser knew, so on Debian and Fedora they
        // contributed zero manual roots while appearing to be read.
        let src = "# comment\n\
                   MANDATORY_MANPATH\t/usr/share/man\n\
                   MANPATH_MAP /usr/bin /usr/share/man\n\
                   MANDB_MAP /usr/share/man /var/cache/man\n\
                   SECTION 1 8\n";
        let conf = parse_config(Cursor::new(src)).unwrap();
        assert_eq!(conf.manpaths, vec![PathBuf::from("/usr/share/man")]);
    }

    #[test]
    fn mandoc_directives_still_parse() {
        let src = "manpath /usr/share/man\nmanpath /usr/local/share/man\noutput width 100\n";
        let conf = parse_config(Cursor::new(src)).unwrap();
        assert_eq!(
            conf.manpaths,
            vec![
                PathBuf::from("/usr/share/man"),
                PathBuf::from("/usr/local/share/man")
            ]
        );
        assert_eq!(
            conf.output_options.get("width"),
            Some(&Some("100".to_string()))
        );
    }

    #[test]
    fn unknown_directives_are_ignored() {
        let conf = parse_config(Cursor::new("NOT_A_DIRECTIVE x\n\n# c\n")).unwrap();
        assert!(conf.manpaths.is_empty());
    }
}
