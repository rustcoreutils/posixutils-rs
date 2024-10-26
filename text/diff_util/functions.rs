use chrono::{DateTime, Local};
use std::{path::Path, time::SystemTime};

pub fn system_time_to_rfc2822(system_time: SystemTime) -> String {
    Into::<DateTime<Local>>::into(system_time).to_rfc2822()
}

pub fn check_existence(path: &Path) -> bool {
    if !path.exists() {
        eprintln!("diff: {}: No such file or directory", path.display());

        return false;
    }

    true
}
