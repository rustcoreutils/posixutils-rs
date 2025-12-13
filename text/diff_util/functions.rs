use chrono::{DateTime, Local};
use std::{
    fs::File,
    io::{self, Read},
    path::{Path, PathBuf},
    time::SystemTime,
};

use super::constants::UTF8_NOT_ALLOWED_BYTES;
use crate::diff_util::constants::COULD_NOT_UNWRAP_FILENAME;

/// POSIX context diff timestamp format: "%a %b %e %T %Y"
/// Example: "Sat Dec 13 10:26:40 2025"
pub fn system_time_to_context_format(system_time: SystemTime) -> String {
    let dt: DateTime<Local> = system_time.into();
    dt.format("%a %b %e %T %Y").to_string()
}

/// POSIX unified diff timestamp format: "%Y-%m-%d %H:%M:%S"
/// Example: "2025-12-13 10:26:40"
pub fn system_time_to_unified_format(system_time: SystemTime) -> String {
    let dt: DateTime<Local> = system_time.into();
    dt.format("%Y-%m-%d %H:%M:%S").to_string()
}

pub fn is_binary(file_path: &PathBuf) -> io::Result<bool> {
    let mut file = File::open(file_path)?;
    let mut buffer = [0; 1024];

    if let Ok(count) = file.read(&mut buffer) {
        for buf_item in buffer.iter().take(count) {
            if UTF8_NOT_ALLOWED_BYTES.contains(buf_item) {
                return Ok(true);
            }
        }
    }

    Ok(false)
}

pub fn check_existance(path_buf: &Path) -> io::Result<bool> {
    if !path_buf.exists() {
        println!(
            "diff: {}: No such file or directory",
            path_buf.to_str().unwrap_or(COULD_NOT_UNWRAP_FILENAME)
        );

        return Ok(false);
    }

    Ok(true)
}
