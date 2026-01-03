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
    let reader = BufReader::new(file);

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
            "manpath" => {
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
            _ => continue, // Ignore unknown directives for forward compatibility
        }
    }

    Ok(conf)
}
