use chrono::{DateTime, Local};
use std::{
    fs::File,
    io::{self, Read},
    path::Path,
    time::SystemTime,
};

use super::constants::UTF8_NOT_ALLOWED_BYTES;

pub fn system_time_to_rfc2822(system_time: SystemTime) -> String {
    Into::<DateTime<Local>>::into(system_time).to_rfc2822()
}

pub fn is_binary(file_path: &Path) -> io::Result<bool> {
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

pub fn check_existence(path: &Path) -> bool {
    if !path.exists() {
        eprintln!("diff: {}: No such file or directory", path.display());

        return false;
    }

    true
}
