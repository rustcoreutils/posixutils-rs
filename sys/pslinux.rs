use std::fs;
use std::fs::read_to_string;
use std::io::Error;
use std::path::PathBuf;

pub struct ProcessInfo {
    pub pid: i32,
    pub ppid: i32,
    pub uid: u32,
    pub gid: u32,
    pub path: String,
}

pub fn list_processes() -> Result<Vec<ProcessInfo>, Error> {
    let mut processes = Vec::new();
    for entry in fs::read_dir("/proc")? {
        let entry = entry?;
        let path = entry.path();
        if let Ok(pid) = entry.file_name().to_str().unwrap_or("").parse::<i32>() {
            if pid > 0 {
                if let Some(info) = get_process_info(pid, &path) {
                    processes.push(info);
                }
            }
        }
    }
    Ok(processes)
}

fn get_process_info(pid: i32, proc_path: &PathBuf) -> Option<ProcessInfo> {
    let status_path = proc_path.join("status");
    let cmdline_path = proc_path.join("cmdline");
    let stat_path = proc_path.join("stat");
    let exe_path = proc_path.join("exe");

    let status = read_to_string(status_path).ok()?;
    let cmdline = read_to_string(cmdline_path).unwrap_or_default();
    let exe = fs::read_link(exe_path).unwrap_or_else(|_| PathBuf::from("[Permission denied]"));
    let mut comm = String::new();

    if let Ok(stat) = read_to_string(stat_path) {
        if let Some(start) = stat.find('(') {
            if let Some(end) = stat.find(')') {
                comm = stat[start + 1..end].to_string();
            }
        }
    }

    let mut ppid = 0;
    let mut uid = 0;
    let mut gid = 0;

    for line in status.lines() {
        if line.starts_with("PPid:") {
            if let Some(val) = line.split_whitespace().nth(1) {
                ppid = val.parse().unwrap_or(0);
            }
        } else if line.starts_with("Uid:") {
            if let Some(val) = line.split_whitespace().nth(1) {
                uid = val.parse().unwrap_or(0);
            }
        } else if line.starts_with("Gid:") {
            if let Some(val) = line.split_whitespace().nth(1) {
                gid = val.parse().unwrap_or(0);
            }
        }
    }

    let path = if !cmdline.is_empty() {
        cmdline.replace('\0', " ")
    } else if !comm.is_empty() {
        format!("[{}]", comm)
    } else {
        exe.to_string_lossy().to_string()
    };

    Some(ProcessInfo {
        pid,
        ppid,
        uid,
        gid,
        path,
    })
}
