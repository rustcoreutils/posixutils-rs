use libc::{c_int, c_void, pid_t, proc_listallpids, proc_pidinfo, proc_pidpath};
use std::ffi::CStr;
use std::io::Error;

const PROC_PIDPATHINFO_MAXSIZE: usize = 4096;

pub struct ProcessInfo {
    pub pid: pid_t,
    pub ppid: pid_t,
    pub uid: u32,
    pub gid: u32,
    pub path: String,
}

pub fn list_processes() -> Result<Vec<ProcessInfo>, Error> {
    let mut pids: Vec<pid_t> = vec![0; 1024];
    let num_pids = unsafe {
        proc_listallpids(
            pids.as_mut_ptr() as *mut c_void,
            (pids.len() * std::mem::size_of::<pid_t>()) as c_int,
        )
    };

    if num_pids < 0 {
        return Err(Error::last_os_error());
    }

    let mut processes = Vec::new();

    for &pid in &pids[0..num_pids as usize] {
        if pid == 0 {
            continue;
        }
        if let Some(info) = get_process_info(pid) {
            processes.push(info);
        }
    }

    Ok(processes)
}

fn get_process_info(pid: pid_t) -> Option<ProcessInfo> {
    let mut proc_info = std::mem::MaybeUninit::<libc::proc_bsdinfo>::uninit();
    let proc_info_size = std::mem::size_of::<libc::proc_bsdinfo>();
    let res = unsafe {
        proc_pidinfo(
            pid,
            libc::PROC_PIDTBSDINFO,
            0,
            proc_info.as_mut_ptr() as *mut c_void,
            proc_info_size as c_int,
        )
    };

    if res <= 0 {
        return None;
    }

    let proc_info = unsafe { proc_info.assume_init() };

    let mut path_buf = vec![0u8; PROC_PIDPATHINFO_MAXSIZE];
    let path_len = unsafe {
        proc_pidpath(
            pid,
            path_buf.as_mut_ptr() as *mut c_void,
            PROC_PIDPATHINFO_MAXSIZE as u32,
        )
    };

    let path = if path_len > 0 {
        unsafe { CStr::from_ptr(path_buf.as_ptr() as *const i8) }
            .to_string_lossy()
            .into_owned()
    } else {
        String::new()
    };

    Some(ProcessInfo {
        pid: proc_info.pbi_pid as pid_t,
        ppid: proc_info.pbi_ppid as pid_t,
        uid: proc_info.pbi_uid,
        gid: proc_info.pbi_gid,
        path,
    })
}
