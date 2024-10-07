use chrono::{DateTime, Local};
use std::{
    fs::File,
    io::{self, Read},
    path::{Path, PathBuf},
    time::SystemTime,
};

use super::constants::UTF8_NOT_ALLOWED_BYTES;
use crate::diff_util::constants::COULD_NOT_UNWRAP_FILENAME;

pub fn system_time_to_rfc2822(system_time: SystemTime) -> String {
    Into::<DateTime<Local>>::into(system_time).to_rfc2822()
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
